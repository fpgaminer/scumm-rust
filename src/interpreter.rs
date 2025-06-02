use std::{
	cell::RefCell,
	collections::{HashMap, HashSet, VecDeque},
	io::{self, Write},
	pin::Pin,
	rc::Rc,
};

use crate::ast::*;
use futures::{FutureExt, future::LocalBoxFuture};
use indexmap::IndexMap;
use log::{debug, error, info, warn};
use wasm_bindgen::prelude::*;
#[cfg(target_arch = "wasm32")]
use web_sys::{Document, Element};


pub enum Declaration {
	Script(Block),
	Object(ObjectDef),
	Class(ObjectDef),
	Room(RoomDef),
}


// ---------------------------------------------------------------------------
// Runtime object definition (built from the AST once at start-up)
// ---------------------------------------------------------------------------
#[derive(Default)]
pub struct ObjectDef {
	pub name: String,                  // human readable name
	pub classes: Vec<String>,          // List of classes this object inherits
	pub verbs: HashMap<String, Block>, // vLook, vOpen, …
	pub room: u32,                     // 0 = nowhere / inventory only
	pub state: u32,
	pub states: Vec<String>, // List of image paths for each state (starting at state 1)
	pub x: i32,              // x position in pixels
	pub y: i32,              // y position in pixels
	pub width: u32,          // width in pixels
	pub height: u32,         // height in pixels
}


#[derive(Default)]
pub struct RoomDef {
	pub name: String,
	pub entry_script: Option<Block>, // Script to run when entering this room
	pub image: Option<String>,       // Background image for the room
}


/// SCUMM VM task context
#[derive(Clone)]
pub struct Ctx {
	pub vars: Rc<RefCell<HashMap<String, Value>>>,
	pub delay: Rc<RefCell<u32>>, // delay for blocking calls like wait()
	pub interpreter: Interpreter,
}


// ---------------------------------------------------------------------------
// Web interface for managing DOM
// ---------------------------------------------------------------------------
#[cfg(not(target_arch = "wasm32"))]
#[derive(Clone)]
struct WebInterface;

#[cfg(not(target_arch = "wasm32"))]
impl WebInterface {
	fn new() -> Result<Self, JsValue> {
		Ok(WebInterface)
	}

	#[allow(dead_code)]
	fn display_message(&self, _message: &str) -> Result<(), JsValue> {
		Ok(())
	}
}

#[cfg(target_arch = "wasm32")]
#[derive(Clone)]
struct WebInterface {
	document: Document,
	game_container: Element,
	//room_container: Element,
}

#[cfg(target_arch = "wasm32")]
impl WebInterface {
	fn new() -> Result<Self, JsValue> {
		let window = web_sys::window().ok_or("No global `window` exists")?;
		let document = window.document().ok_or("Should have a document on window")?;

		// Create main game container
		let game_container = document.create_element("div")?;
		game_container.set_attribute("id", "game-container")?;
		game_container.set_attribute(
			"style",
			"position: relative; width: 640px; height: 480px; background: #000 url('room1.png') no-repeat center/cover; margin: 0 auto; border: 2px solid #333;",
		)?;

		// Create room container for objects
		let room_container = document.create_element("div")?;
		room_container.set_attribute("id", "room-container")?;
		room_container.set_attribute("style", "position: relative; width: 100%; height: 100%;")?;

		game_container.append_child(&room_container)?;

		// Append to body or app div
		if let Some(app) = document.get_element_by_id("app") {
			app.append_child(&game_container)?;
		} else {
			document.body().unwrap().append_child(&game_container)?;
		}

		Ok(WebInterface {
			document,
			game_container,
			//room_container,
		})
	}

	//fn create_object_div(&self, object_def: &ObjectDef, _room_id: i32) -> Result<Element, JsValue> {
	//	let div = self.document.create_element("div")?;
	//div.set_attribute("id", &format!("object-{}", object_def.id))?;
	//	div.set_attribute("class", "game-object")?;
	//div.set_attribute("data-object-id", &object_def.id.to_string())?;

	// Basic positioning and styling
	//	let style = "position: absolute; cursor: pointer; border: 1px solid #666; padding: 5px; background: #333; color: white; font-family: monospace; font-size: 12px;".to_string();

	// Set costume-based appearance
	/*if let Some(costume) = object_def.costume {
		// For now, use costume number as a simple visual indicator
		// Later this would load actual image files
		style.push_str(&format!(
			" background-image: url('obj{:03}.png'); background-size: contain; background-repeat: no-repeat; width: 32px; height: 32px;",
			costume
		));
		div.set_inner_html(&format!("<span style='display:none'>{}</span>", object_def.name));
	} else {
		// No costume - show as text
		div.set_inner_html(&object_def.name);
		style.push_str(" min-width: 60px; text-align: center;");
	}*/

	// Random positioning for now (later would be specified in the script)
	//let x = (object_def.id * 37) % 500; // Simple pseudo-random positioning
	//let y = (object_def.id * 73) % 350;
	//style.push_str(&format!(" left: {}px; top: {}px;", x, y));

	//	div.set_attribute("style", &style)?;
	//	div.set_attribute("title", &object_def.name)?;

	//	Ok(div)
	//}

	/*fn show_object(&self, object_id: i32, _room_id: i32) -> Result<(), JsValue> {
		if let Some(div) = self.document.get_element_by_id(&format!("object-{}", object_id)) {
			div.set_attribute("style", &div.get_attribute("style").unwrap_or_default().replace("display: none;", ""))?;
		}
		Ok(())
	}

	fn hide_object(&self, object_id: i32) -> Result<(), JsValue> {
		if let Some(div) = self.document.get_element_by_id(&format!("object-{}", object_id)) {
			let current_style = div.get_attribute("style").unwrap_or_default();
			div.set_attribute("style", &format!("{}; display: none;", current_style))?;
		}
		Ok(())
	}*/

	fn display_message(&self, message: &str) -> Result<(), JsValue> {
		// Create or update message area
		let message_div = if let Some(existing) = self.document.get_element_by_id("game-message") {
			existing
		} else {
			let div = self.document.create_element("div")?;
			div.set_attribute("id", "game-message")?;
			div.set_attribute("style", "position: absolute; bottom: 10px; left: 10px; right: 10px; background: rgba(0,0,0,0.8); color: white; padding: 10px; font-family: monospace; border: 1px solid #666;")?;
			self.game_container.append_child(&div)?;
			div
		};

		message_div.set_inner_html(message);
		Ok(())
	}
}


#[derive(Clone, Debug, PartialEq)]
pub enum Value {
	Number(i32),
	Bool(bool),
	Str(String),
	Null,
}

impl Value {
	pub fn truthy(&self) -> bool {
		match self {
			Value::Bool(b) => *b,
			Value::Number(n) => *n != 0,
			Value::Str(s) => !s.is_empty(),
			Value::Null => false,
		}
	}
	pub fn as_number(&self) -> i32 {
		match self {
			Value::Number(n) => *n,
			Value::Bool(b) => {
				if *b {
					1
				} else {
					0
				}
			},
			_ => 0,
		}
	}
	pub fn as_string(&self) -> String {
		match self {
			Value::Str(s) => s.clone(),
			Value::Number(n) => n.to_string(),
			Value::Bool(b) => b.to_string(),
			Value::Null => "<null>".into(),
		}
	}
}


// ---------------------------------------------------------------------------
// World model (super‑simple!)
// ---------------------------------------------------------------------------
#[derive(Default)]
struct World {
	inventory: HashSet<u32>, // OBJ currently held by player
	current_room: u32,
}


type BuiltinFn = dyn for<'a> Fn(Vec<Value>, &'a Ctx) -> LocalBoxFuture<'a, Value>;
type TaskFuture = Pin<Box<dyn Future<Output = ()> + 'static>>;

/// A running instance of a script
struct Task {
	fut: TaskFuture,
	ctx: Ctx,
}


#[derive(Clone)]
pub struct Interpreter {
	//tasks: VecDeque<Task>, // RUNNABLE queue
	pub builtins: Rc<HashMap<String, Rc<BuiltinFn>>>,
	//scripts: Rc<HashMap<String, Script>>,
	//pub consts: Rc<HashMap<String, Value>>,
	world: Rc<RefCell<World>>,
	//objects: Rc<RefCell<HashMap<i32, ObjectDef>>>, // id → def
	//#[allow(dead_code)]
	//object_names: Rc<HashMap<String, i32>>, // lowercase name → id
	web_interface: WebInterface, // Web DOM interface
	run_queue: Rc<RefCell<VecDeque<Task>>>,
	pub declarations: Rc<RefCell<IndexMap<String, Declaration>>>,
}

impl Interpreter {
	pub fn new(ast: &[TopLevel]) -> Self {
		// Initialize the interpreter
		let this = Interpreter {
			builtins: Rc::new(build_builtins()),
			world: Rc::new(RefCell::new(World::default())),
			web_interface: WebInterface::new().unwrap(),
			run_queue: Rc::new(RefCell::new(VecDeque::new())),
			declarations: Rc::new(RefCell::new(IndexMap::new())),
		};

		// Execute the AST to build up all the declarations
		for tl in ast {
			match tl.exec(&this) {
				Ok(_) => {},
				Err(err) => {
					error!("Error executing top-level statement: {:?}", err);
				},
			}
		}

		// Start the main script
		this.spawn_script("main");

		this
	}

	// --------------------------------------------------
	// Web interface initialization
	// --------------------------------------------------
	pub fn init_web_interface(&self) -> Result<(), JsValue> {
		// Create divs for all objects in the current room
		/*for (id, def) in &*self.objects {
			if def.room == self.world.borrow().current_room {
				let object_div = self.web_interface.create_object_div(def, def.room)?;
				self.web_interface.room_container.append_child(&object_div)?;

				// Hide objects that are initially not in room (initialRoom 0)
				if def.room == 0 {
					self.web_interface.hide_object(*id)?;
				}
			}
		}*/
		Ok(())
	}

	// --------------------------------------------------
	// Script spawning helpers
	// --------------------------------------------------
	fn spawn_script_value(&self, v: Value) {
		match v {
			Value::Number(n) => self.spawn_script(&n.to_string()),
			Value::Str(s) => self.spawn_script(&s),
			_ => {},
		}
	}

	fn spawn_script(&self, key: &str) {
		let script = self.declarations.borrow().get(key).and_then(|decl| match decl {
			Declaration::Script(script) => Some(script.clone()),
			_ => None,
		});
		let script = match script {
			Some(script) => script,
			None => {
				error!("Unknown script: {}", key);
				return;
			},
		};

		info!("startScript -> {}", key);
		let ctx = Ctx {
			vars: Rc::new(RefCell::new(HashMap::new())),
			delay: Rc::new(RefCell::new(0)),
			interpreter: self.clone(),
		};
		let _key = key.to_string();
		let cloned_ctx = ctx.clone();
		let task = Task {
			fut: Box::pin(async move {
				if let Err(_err) = script.exec(&ctx).await {
					error!("Error executing script {}: {:?}", _key, _err);
				} else {
					debug!("Script {} executed successfully", _key);
				}
			}),
			ctx: cloned_ctx,
		};
		self.run_queue.borrow_mut().push_back(task);
	}

	fn to_id(&self, v: &Value) -> u32 {
		match v {
			Value::Number(n) => (*n).try_into().unwrap_or(0), // Convert i32 to u32, default to 0 if negative
			Value::Str(s) => {
				self.declarations.borrow().get_index_of(s).map(|id| id as u32).unwrap_or(0) // 0 = unknown
			},
			_ => 0,
		}
	}

	// --------------------------------------------------
	// Web-specific tick method
	// Performs a single tick of the interpreter
	// This is intended to be called once per animation frame
	// --------------------------------------------------
	pub fn tick_web(&self) {
		// The entry script may have changed the room already
		if self.world.borrow().current_room == 0 {
			self.world.borrow_mut().current_room = 1;
		}

		debug!("Ticking interpreter with {} tasks", self.run_queue.borrow().len());

		//---------------------------------------------------------------
		// Run each runnable task *once* (co-operative multitasking)
		//---------------------------------------------------------------
		let mut n = self.run_queue.borrow().len();

		while n > 0 {
			let mut task = self.run_queue.borrow_mut().pop_front().unwrap();
			//let mut task = self.tasks.pop_front().unwrap();
			n -= 1;

			// Check if the task has a delay
			if *task.ctx.delay.borrow() > 0 {
				*task.ctx.delay.borrow_mut() -= 1;
				self.run_queue.borrow_mut().push_back(task);
				continue;
			}

			// Continue the task
			match task.fut.as_mut().poll(&mut std::task::Context::from_waker(futures::task::noop_waker_ref())) {
				std::task::Poll::Ready(()) => {
					debug!("Task completed");
				},
				std::task::Poll::Pending => {
					self.run_queue.borrow_mut().push_back(task);
					continue; // Still running, continue to next task
				},
			}
		}

		debug!("Interpreter finished executing scripts");
	}

	pub fn add_declaration<K: Into<String>>(&self, name: K, decl: Declaration) -> Result<u32, anyhow::Error> {
		match self.declarations.borrow_mut().entry(name.into()) {
			indexmap::map::Entry::Occupied(entry) => {
				// Declaration already exists, return an error
				anyhow::bail!("Declaration for '{}' already exists", entry.key());
			},
			indexmap::map::Entry::Vacant(entry) => {
				// Insert the new declaration
				let id = entry.index() as u32;
				entry.insert(decl);
				Ok(id)
			},
		}
	}

	pub fn with_object_by_name<F, R>(&self, name: &str, f: F) -> Option<R>
	where
		F: FnOnce(&ObjectDef) -> R,
	{
		self.declarations.borrow().get(name).and_then(|decl| match decl {
			Declaration::Object(obj) => Some(f(obj)),
			_ => None,
		})
	}

	pub fn with_object_by_id<F, R>(&self, id: u32, f: F) -> Option<R>
	where
		F: FnOnce(&ObjectDef) -> R,
	{
		self.declarations.borrow().get_index(id as usize).and_then(|(_, decl)| match decl {
			Declaration::Object(obj) => Some(f(obj)),
			_ => None,
		})
	}

	pub fn with_object_by_id_mut<F, R>(&self, id: u32, f: F) -> Option<R>
	where
		F: FnOnce(&mut ObjectDef) -> R,
	{
		self.declarations.borrow_mut().get_index_mut(id as usize).and_then(|(_, decl)| match decl {
			Declaration::Object(obj) => Some(f(obj)),
			_ => None,
		})
	}

	// --------------------------------------------------
	// Run until no runnable tasks remain (original version for terminal use)
	// --------------------------------------------------
	/*pub fn run(&mut self) {
		// The entry script may have changed the room already
		if self.world.current_room == 0 {
			self.world.current_room = 1;
		}

		loop {
			//---------------------------------------------------------------
			// 1) run each runnable task *once* (co-operative multitasking)
			//---------------------------------------------------------------
			let mut n = self.tasks.len();
			while n > 0 {
				let mut task = self.tasks.pop_front().unwrap();
				n -= 1;

				if task.delay > 0 {
					task.delay -= 1;
					self.tasks.push_back(task);
					continue;
				}

				let stmt_ref = match task.next() {
					Some(s) => s,
					None => continue,
				};
				let act = self.exec_stmt(stmt_ref.clone(), &mut task);
				task.ip += 1;
				if let Action::Splice { at, items } = act {
					for (i, s) in items.into_iter().enumerate() {
						task.stmts.insert(at + i, s.clone());
					}
				}
				if task.ip < task.stmts.len() {
					self.tasks.push_back(task);
				}
			}

			//---------------------------------------------------------------
			// 2) if no script produced output this tick, ask the player
			//---------------------------------------------------------------
			self.describe_room();
			let Some(line) = read_player_command() else { break };
			if line.is_empty() {
				continue;
			}
			if line == "quit" {
				info!("Bye!");
				break;
			}
			if line == "inventory" {
				if self.world.inventory.is_empty() {
					info!("Your pockets are empty.");
				} else {
					info!("You are carrying:");
					for id in &self.world.inventory {
						if let Some(def) = self.objects.get(id) {
							info!(" – {}", def.name);
						}
					}
				}
				continue;
			}
			let words: Vec<&str> = line.split_whitespace().collect();
			if words.is_empty() {
				continue;
			}

			// single-verb commands ----------------------------------------
			let verb_word = words[0];
			let verb_name = user_verb_to_scumm(verb_word);
			if verb_name.is_empty() {
				println!("I don’t understand.");
				continue;
			}

			// pick <obj> or look <obj> etc.  “use key on door” -------------
			let rest = &words[1..];
			if rest.is_empty() {
				println!("{} what?", verb_word);
				continue;
			}

			// detect “on/with” to support 2-object USE
			let split_at = rest.iter().position(|w| *w == "on" || *w == "with");
			let (obj_words, tgt_words) = match split_at {
				Some(pos) => (&rest[..pos], &rest[pos + 1..]),
				None => (rest, &[][..]),
			};
			let obj_name = obj_words.join(" ");
			let obj_id = *self.object_names.get(&obj_name).unwrap_or(&0);
			if obj_id == 0 {
				println!("There is no '{}' here.", obj_name);
				continue;
			}
			let tgt_id = if !tgt_words.is_empty() {
				let name = tgt_words.join(" ");
				*self.object_names.get(&name).unwrap_or(&0)
			} else {
				0
			};

			// finally: run the verb ---------------------------------------
			if tgt_id != 0 {
				// try the first object (obj_id) first …
				if !self.run_verb(obj_id, verb_name, Some(tgt_id)) {
					// … fallback to the second if the first has no such verb
					if !self.run_verb(tgt_id, verb_name, Some(obj_id)) {
						println!("{} {} on {}? I don’t know how to do that.", verb_word, obj_name, tgt_words.join(" "));
					}
				}
			} else {
				self.run_verb(obj_id, verb_name, None);
			}
		}
	}*/
}


// --------------------------------------------------
// Built‑ins
// --------------------------------------------------
pub fn builtin_async<F>(f: F) -> Rc<BuiltinFn>
where
	F: 'static + for<'a> Fn(Vec<Value>, &'a Ctx) -> LocalBoxFuture<'a, Value>,
{
	Rc::new(move |args, ctx| f(args, ctx))
}


fn build_builtins() -> HashMap<String, Rc<BuiltinFn>> {
	use Value::*;
	let mut builtins: HashMap<String, Rc<BuiltinFn>> = HashMap::new();

	builtins.insert(
		"print".into(),
		builtin_async(|args, _| {
			async move {
				let output = args.iter().map(|v| v.as_string()).collect::<Vec<_>>().join(" ");
				info!("{}", output);
				Null
			}
			.boxed_local()
		}),
	);

	builtins.insert(
		"wait".into(),
		builtin_async(|mut args, ctx| {
			async move {
				let ticks = args.pop().unwrap_or(Number(1)).as_number();
				*ctx.delay.borrow_mut() = ticks as u32;
				yield_now().await; // yield
				Null
			}
			.boxed_local()
		}),
	);

	builtins.insert(
		"breakScript".into(),
		builtin_async(|_, _ctx| {
			async move {
				debug!("Break script called!");
				yield_now().await; // stop right here, resume next tick
				Value::Null
			}
			.boxed_local()
		}),
	);

	builtins.insert(
		"sayLine".into(),
		builtin_async(|args, ctx| {
			async move {
				if args.len() >= 2 {
					let txt = args[1].as_string();
					let message = txt.trim_matches('"');

					ctx.interpreter.web_interface.display_message(message).unwrap_or_else(|_| {
						info!("{}", message);
					});
				}
				Null
			}
			.boxed_local()
		}),
	);

	builtins.insert(
		"prompt".into(),
		builtin_async(|args, _| {
			async move {
				let msg = args.first().map(|v| v.as_string()).unwrap_or_default();
				let prompt_text = msg.trim_matches('"');

				// Use web prompt API
				let window = web_sys::window().unwrap();
				if let Ok(Some(result)) = window.prompt_with_message(prompt_text) {
					Value::Str(result)
				} else {
					Value::Str(String::new())
				}
			}
			.boxed_local()
		}),
	);

	builtins.insert(
		"startScript".into(),
		builtin_async(|mut args, ctx| {
			async move {
				if let Some(id) = args.pop() {
					ctx.interpreter.spawn_script_value(id);
				}
				Null
			}
			.boxed_local()
		}),
	);

	// ---- World manipulation stubs ----
	// putActorAt(actor, x, y, room)
	builtins.insert(
		"putActorAt".into(),
		builtin_async(|args, ctx| {
			async move {
				if args.len() != 4 {
					error!("putActorAt requires exactly 4 arguments: actor, x, y, room");
					return Null;
				}

				let actor = ctx.interpreter.to_id(&args[0]);
				let room = ctx.interpreter.to_id(&args[3]);

				// NOT FULLY IMPLEMENTED YET
				// Assumes the target actor is the player
				// No position is tracked, so just updates the current room
				ctx.interpreter.world.borrow_mut().current_room = room;

				info!("[actor {actor}] appears in room {room}");
				Null
			}
			.boxed_local()
		}),
	);

	builtins.insert(
		"setCameraAt".into(),
		builtin_async(|args, _| {
			async move {
				if args.len() >= 2 {
					debug!("[camera] room {} target x {}", args[0].as_string(), args[1].as_string());
				}
				Null
			}
			.boxed_local()
		}),
	);
	builtins.insert(
		"walkActorTo".into(),
		builtin_async(|args, _| {
			async move {
				if args.len() >= 3 {
					debug!("[actor {}] walks to {},{}", args[0].as_string(), args[1].as_string(), args[2].as_string());
				}
				Null
			}
			.boxed_local()
		}),
	);
	builtins.insert(
		"faceActor".into(),
		builtin_async(|args, _| {
			async move {
				if args.len() >= 2 {
					debug!("[actor {}] faces dir {}", args[0].as_string(), args[1].as_string());
				}
				Null
			}
			.boxed_local()
		}),
	);

	builtins.insert(
		"getState".into(),
		builtin_async(|args, ctx| {
			async move {
				if args.len() != 1 {
					error!("getState requires exactly 1 argument: object");
					return Null;
				}

				let id = ctx.interpreter.to_id(&args[0]);

				if let Some(state) = ctx.interpreter.with_object_by_id(id, |object| object.state) {
					Value::Number(state as i32)
				} else {
					warn!("[object {id}] not found");
					Null
				}
			}
			.boxed_local()
		}),
	);

	builtins.insert(
		"setState".into(),
		builtin_async(|args, ctx| {
			async move {
				if args.len() != 2 {
					error!("setState requires exactly 2 arguments: object, state");
					return Null;
				}

				let id = ctx.interpreter.to_id(&args[0]);
				let state_value = args[1].as_number() as u32;

				match ctx.interpreter.with_object_by_id_mut(id, |object| {
					object.state = state_value;
					debug!("[object {id}] state set to {state_value}");
				}) {
					Some(_) => {},
					None => {
						warn!("[object {id}] not found");
					},
				}

				Null
			}
			.boxed_local()
		}),
	);

	builtins.insert(
		"objectInHand".into(),
		builtin_async(|args, ctx| {
			async move {
				if let Some(obj) = args.first() {
					let id = ctx.interpreter.to_id(obj);
					Value::Bool(ctx.interpreter.world.borrow().inventory.contains(&id))
				} else {
					Value::Bool(false)
				}
			}
			.boxed_local()
		}),
	);

	builtins.insert(
		"addToInventory".into(),
		builtin_async(|args, ctx| {
			async move {
				if let Some(obj) = args.first() {
					let id = ctx.interpreter.to_id(obj);
					ctx.interpreter.world.borrow_mut().inventory.insert(id);

					// Log and potentially update inventory display
					debug!("[inventory] added object {id}");

					// Future enhancement: update visual inventory display
				}
				Null
			}
			.boxed_local()
		}),
	);

	builtins.insert(
		"pickupObject".into(),
		builtin_async(|_args, _ctx| {
			async move {
				debug!("pickupObject called – this is a stub!");
				Null
			}
			.boxed_local()
		}),
	);

	builtins.insert(
		"animateObject".into(),
		builtin_async(|args, ctx| {
			async move {
				if args.len() >= 2 {
					let obj_id = ctx.interpreter.to_id(&args[0]);
					let anim_id = args[1].as_number();

					// Log animation and potentially trigger visual effect
					debug!("[object {obj_id}] plays anim {anim_id}");

					// Future enhancement: add CSS animation or transform to the object div
					/*if let Some(obj_div) = ctx.interpreter.web_interface.document.get_element_by_id(&format!("object-{}", obj_id)) {
						// Simple bounce animation for now
						obj_div
							.set_attribute(
								"style",
								&format!("{}; animation: bounce 0.5s ease-in-out;", obj_div.get_attribute("style").unwrap_or_default()),
							)
							.unwrap_or_else(|e| {
								error!("Error animating object: {:?}", e);
							});
					}*/
				}
				Null
			}
			.boxed_local()
		}),
	);

	builtins.insert(
		"loadRoom".into(),
		builtin_async(|args, ctx| {
			async move {
				if let Some(room) = args.first() {
					let id = ctx.interpreter.to_id(room);
					info!("[game] loading room {id} – thanks for playing!");
					ctx.interpreter.run_queue.borrow_mut().clear(); // stop
				}
				Null
			}
			.boxed_local()
		}),
	);

	builtins
}


#[derive(Debug)]
#[must_use = "must await or poll this future"]
pub struct YieldNow(bool);

impl Future for YieldNow {
	type Output = ();

	fn poll(mut self: Pin<&mut Self>, cx: &mut std::task::Context<'_>) -> std::task::Poll<Self::Output> {
		if !self.0 {
			self.0 = true;
			cx.waker().wake_by_ref();
			std::task::Poll::Pending
		} else {
			std::task::Poll::Ready(())
		}
	}
}

pub fn yield_now() -> YieldNow {
	YieldNow(false)
}


// ---------------------------------------------------------------------------
// Convenience wrapper
// ---------------------------------------------------------------------------


#[allow(dead_code)]
fn user_verb_to_scumm(cmd: &str) -> &'static str {
	match cmd {
		"look" | "examine" => "vLook",
		"open" => "vOpen",
		"close" => "vClose",
		"use" => "vUse",
		"read" => "vRead",
		"take" | "get" | "pickup" => "vPickUp",
		_ => "",
	}
}


#[allow(dead_code)]
fn read_player_command() -> Option<String> {
	print!(">> ");
	io::stdout().flush().ok()?;
	let mut buf = String::new();
	if io::stdin().read_line(&mut buf).is_ok() {
		Some(buf.trim().to_lowercase())
	} else {
		None
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use std::collections::HashMap;

	fn run_script(src: &str) -> HashMap<String, Value> {
		let ast = crate::parse_str(src).expect("parse failed");

		// Use the real constructor so that interpreter state is initialised
		let interp = Interpreter::new(&ast);

		// Grab the context of the spawned `main` script so we can inspect its vars
		let ctx = interp.run_queue.borrow().front().expect("no script spawned").ctx.clone();

		// Tick until all tasks have completed. This replicates the
		// interpreter loop without relying on web-specific APIs.
		use futures::task::{Context, Poll, noop_waker_ref};

		while !interp.run_queue.borrow().is_empty() {
			let mut n = interp.run_queue.borrow().len();
			while n > 0 {
				let mut task = interp.run_queue.borrow_mut().pop_front().unwrap();
				n -= 1;

				if *task.ctx.delay.borrow() > 0 {
					*task.ctx.delay.borrow_mut() -= 1;
					interp.run_queue.borrow_mut().push_back(task);
					continue;
				}

				match task.fut.as_mut().poll(&mut Context::from_waker(noop_waker_ref())) {
					Poll::Ready(()) => {},
					Poll::Pending => interp.run_queue.borrow_mut().push_back(task),
				}
			}
		}

		ctx.vars.borrow().clone()
	}

	#[test]
	fn math_precedence() {
		let vars = run_script("script main() {\n    int a = 3 + 4 * 5;\n    int b = (3 + 4) * 5;\n}");
		assert_eq!(vars.get("a"), Some(&Value::Number(23)));
		assert_eq!(vars.get("b"), Some(&Value::Number(35)));
	}

	#[test]
	fn variable_reassignment() {
		let vars = run_script("script main() {\n    int x = 1;\n    x = x + 4 * 2;\n}");
		assert_eq!(vars.get("x"), Some(&Value::Number(9)));
	}
}
