use std::{
	cell::RefCell,
	collections::{HashMap, HashSet, VecDeque},
	io::{self, Write},
	pin::Pin,
	rc::Rc,
};

use crate::ast::*;
use futures::{FutureExt, future::LocalBoxFuture};
use wasm_bindgen::prelude::*;
use web_sys::{Document, Element};


// ---------------------------------------------------------------------------
// Runtime object definition (built from the AST once at start-up)
// ---------------------------------------------------------------------------
struct ObjectDef {
	id: i32,
	name: String,                  // human readable name
	verbs: HashMap<String, Block>, // vLook, vOpen, …
	init_room: i32,                // 0 = nowhere / inventory only
	init_state: u32,
	costume: Option<i32>, // costume number for visual display
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
#[derive(Clone)]
struct WebInterface {
	document: Document,
	game_container: Element,
	room_container: Element,
}

impl WebInterface {
	fn new() -> Result<Self, JsValue> {
		let window = web_sys::window().ok_or("No global `window` exists")?;
		let document = window.document().ok_or("Should have a document on window")?;

		// Create main game container
		let game_container = document.create_element("div")?;
		game_container.set_attribute("id", "game-container")?;
		game_container.set_attribute(
			"style",
			"position: relative; width: 640px; height: 480px; background: #000; margin: 0 auto; border: 2px solid #333;",
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
			room_container,
		})
	}

	fn create_object_div(&self, object_def: &ObjectDef, room_id: i32) -> Result<Element, JsValue> {
		let div = self.document.create_element("div")?;
		div.set_attribute("id", &format!("object-{}", object_def.id))?;
		div.set_attribute("class", "game-object")?;
		div.set_attribute("data-object-id", &object_def.id.to_string())?;

		// Basic positioning and styling
		let mut style = "position: absolute; cursor: pointer; border: 1px solid #666; padding: 5px; background: #333; color: white; font-family: monospace; font-size: 12px;".to_string();

		// Set costume-based appearance
		if let Some(costume) = object_def.costume {
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
		}

		// Random positioning for now (later would be specified in the script)
		let x = (object_def.id * 37) % 500; // Simple pseudo-random positioning
		let y = (object_def.id * 73) % 350;
		style.push_str(&format!(" left: {}px; top: {}px;", x, y));

		div.set_attribute("style", &style)?;
		div.set_attribute("title", &object_def.name)?;

		Ok(div)
	}

	fn show_object(&self, object_id: i32, room_id: i32) -> Result<(), JsValue> {
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
	}

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
	object_state: HashMap<i32, u32>, // OBJ -> current state number
	object_room: HashMap<i32, i32>,  // OBJ -> room id (0 = nowhere)
	inventory: HashSet<i32>,         // OBJ currently held by player
	current_room: i32,
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
	scripts: Rc<HashMap<String, Script>>,
	pub consts: Rc<HashMap<String, Value>>,
	world: Rc<RefCell<World>>,
	objects: Rc<HashMap<i32, ObjectDef>>,   // id → def
	object_names: Rc<HashMap<String, i32>>, // lowercase name → id  (for parser)
	web_interface: WebInterface,            // Web DOM interface
	run_queue: Rc<RefCell<VecDeque<Task>>>,
}

impl Interpreter {
	// --------------------------------------------------
	// Construction
	// --------------------------------------------------
	pub fn new(ast: &[TopLevel]) -> Self {
		let mut scripts = HashMap::new();
		let mut consts = HashMap::new();
		let mut class_verbs = HashMap::new();

		// Pass 1 – collect scripts, defines, and classes
		for tl in ast {
			match tl {
				TopLevel::Script(s) => match &s.name {
					ScriptName::Identifier(name) => {
						scripts.insert(name.clone(), s.clone());
					},
					ScriptName::Number(n) => {
						scripts.insert(n.to_string(), s.clone());
					},
				},
				TopLevel::Directive(name, val) if name == "define" => {
					let mut parts = val.split_whitespace();
					if let Some(ident) = parts.next() {
						if let Some(rest) = parts.next() {
							if let Ok(n) = rest.parse::<i32>() {
								consts.insert(ident.to_string(), Value::Number(n));
							} else {
								consts.insert(ident.to_string(), Value::Str(rest.to_string()));
							}
						}
					}
				},
				TopLevel::Class(c) => {
					for st in &c.body.statements {
						if let Statement::Verb(v) = st {
							if let Some(body) = &v.body {
								class_verbs
									.entry(c.name.clone())
									.or_insert_with(HashMap::new)
									.insert(v.name.clone(), body.clone());
							}
						}
					}
				},
				_ => {},
			}
		}

		// Pass 2 – build objects
		let mut objects = HashMap::new();
		let mut object_names = HashMap::new();

		for tl in ast {
			if let TopLevel::Object(o) = tl {
				// Resolve numeric id ⇒ the identifier *must* be in the #define table
				let id_num = match consts.get(&o.id) {
					Some(Value::Number(n)) => *n,
					_ => {
						web_sys::console::warn_1(&JsValue::from_str(&format!("[warn] object id {} has no #define – using 0", o.id)));
						0
					},
				};

				let mut verbs = HashMap::new();
				let mut declared_classes = Vec::new();
				let mut init_room = 0;
				let mut init_state = 0;
				let mut has_init_room = false;
				let mut costume = None;

				for st in &o.body.statements {
					match st {
						Statement::Verb(v) if v.body.is_some() => {
							verbs.insert(v.name.clone(), v.body.as_ref().unwrap().clone());
						},
						Statement::PropertyAssignment(p) if p.name == "initialRoom" => {
							init_room = match &p.value {
								PropertyValue::Number(n) => *n as i32,
								PropertyValue::Identifier(ident) => consts
									.get(ident)
									.and_then(|v| if let Value::Number(n) = v { Some(*n) } else { None })
									.unwrap_or(0),
								_ => 0,
							};
							has_init_room = true;
						},
						Statement::PropertyAssignment(p) if p.name == "costume" => {
							costume = match &p.value {
								PropertyValue::Number(n) => Some(*n as i32),
								PropertyValue::Identifier(ident) => consts.get(ident).and_then(|v| if let Value::Number(n) = v { Some(*n) } else { None }),
								_ => None,
							};
						},
						Statement::ClassDeclaration(name) => declared_classes.push(name.clone()),
						Statement::State(s) => init_state = s.number,
						_ => {},
					}
				}

				/* -------- bring in verbs from each declared class -------- */
				for cname in declared_classes {
					if let Some(cverbs) = class_verbs.get(&cname) {
						for (vname, block) in cverbs {
							// object-level verb overrides class verb with same name
							verbs.entry(vname.clone()).or_insert(block.clone());
						}
					} else {
						eprintln!("[warn] class '{}' not found", cname);
					}
				}

				if !has_init_room {
					// use #define ROOM_ID if present, otherwise fall back to 1
					init_room = consts
						.get("ROOM_ID")
						.and_then(|v| if let Value::Number(n) = v { Some(*n) } else { None })
						.unwrap_or(1);
				}

				let def = ObjectDef {
					id: id_num,
					name: o.name.trim_matches('"').to_owned(),
					verbs,
					init_room,
					init_state,
					costume,
				};
				objects.insert(id_num, def);
			}
		}

		let mut world = World::default();
		for (id, def) in &objects {
			if def.init_room != 0 {
				world.object_room.insert(*id, def.init_room);
			}
			world.object_state.insert(*id, def.init_state);
		}

		// build reverse lookup for the command parser
		for (id, def) in &objects {
			object_names.insert(def.name.to_lowercase(), *id);
		}

		scripts
			.get("1")
			.or_else(|| scripts.get("ROOM_ID"))
			.or_else(|| scripts.iter().next().map(|(_, script)| script))
			.map(|script| script.name.clone());

		let this = Self {
			builtins: Rc::new(build_builtins()),
			scripts: Rc::new(scripts),
			consts: Rc::new(consts),
			world: Rc::new(RefCell::new(world)),
			objects: Rc::new(objects),
			object_names: Rc::new(object_names),
			web_interface: WebInterface::new().unwrap(),
			run_queue: Rc::new(RefCell::new(VecDeque::new())),
		};

		// Spawn initial script
		// Looks for a script with ID 1 (ROOM_ID), fallsback to first script
		let entry_script = if this.scripts.contains_key("1") {
			Some("1".to_owned())
		} else if this.scripts.contains_key("ROOM_ID") {
			Some("ROOM_ID".to_owned())
		} else {
			// Fallback to first script if no entry point is defined
			this.scripts.iter().next().map(|(name, _)| name.clone())
		};

		if let Some(script_name) = entry_script {
			this.spawn_script(&script_name);
		}

		this
	}

	// --------------------------------------------------
	// Web interface initialization
	// --------------------------------------------------
	pub fn init_web_interface(&self) -> Result<(), JsValue> {
		// Create divs for all objects in the current room
		for (id, def) in &*self.objects {
			if def.init_room == self.world.borrow().current_room || def.init_room == 1 {
				let object_div = self.web_interface.create_object_div(def, def.init_room)?;
				self.web_interface.room_container.append_child(&object_div)?;

				// Hide objects that are initially not in room (initialRoom 0)
				if def.init_room == 0 {
					self.web_interface.hide_object(*id)?;
				}
			}
		}
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
		if let Some(scr) = self.scripts.get(key) {
			eprintln!("[info] startScript -> {}", key);
			let ctx = Ctx {
				vars: Rc::new(RefCell::new(HashMap::new())),
				delay: Rc::new(RefCell::new(0)),
				interpreter: self.clone(),
			};
			let script = scr.clone();
			let key = key.to_string();
			let cloned_ctx = ctx.clone();
			let task = Task {
				fut: Box::pin(async move {
					if let Err(err) = script.body.exec(&ctx).await {
						web_sys::console::error_1(&JsValue::from_str(&format!("Error executing script {}: {:?}", key, err)));
					} else {
						web_sys::console::log_1(&JsValue::from_str(&format!("Script {} executed successfully", key)));
					}
				}),
				ctx: cloned_ctx,
			};
			self.run_queue.borrow_mut().push_back(task);
		} else {
			eprintln!("[warn] startScript: unknown script {key}");
		}
	}

	fn to_id(&self, v: &Value) -> i32 {
		match v {
			Value::Number(n) => *n,
			Value::Str(s) => {
				if let Some(Value::Number(n)) = self.consts.get(s) {
					*n
				} else {
					0 // unknown -> 0
				}
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

		web_sys::console::log_1(&JsValue::from_str(
			format!("Ticking interpreter with {} tasks", self.run_queue.borrow().len()).as_str(),
		));

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
					web_sys::console::log_1(&JsValue::from_str("Task completed"));
				},
				std::task::Poll::Pending => {
					self.run_queue.borrow_mut().push_back(task);
					continue; // Still running, continue to next task
				},
			}
		}

		web_sys::console::log_1(&JsValue::from_str("Interpreter finished executing scripts"));
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
				println!("Bye!");
				break;
			}
			if line == "inventory" {
				if self.world.inventory.is_empty() {
					println!("Your pockets are empty.");
				} else {
					println!("You are carrying:");
					for id in &self.world.inventory {
						if let Some(def) = self.objects.get(id) {
							println!(" – {}", def.name);
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


	fn describe_room(&self) {
		println!();
		println!("You are in room {}.", self.world.borrow().current_room);
		let here: Vec<&ObjectDef> = self
			.objects
			.values()
			.filter(|o| self.world.borrow().object_room.get(&o.id) == Some(&self.world.borrow().current_room))
			.collect();
		if here.is_empty() {
			println!("Nothing of interest here.");
		} else {
			print!("You see: ");
			for (i, o) in here.iter().enumerate() {
				if i > 0 {
					print!(", ");
				}
				print!("{}", o.name);
			}
			println!(".");
		}
	}
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
				for v in args {
					web_sys::console::log_1(&JsValue::from_str(&v.as_string()));
				}
				web_sys::console::log_1(&JsValue::from_str("")); // add a newline
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
		"breakHere".into(),
		builtin_async(|_, _ctx| {
			async move {
				web_sys::console::log_1(&JsValue::from_str("Break here called!"));
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
						web_sys::console::log_1(&JsValue::from_str(message));
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
	builtins.insert(
		"putActorInRoom".into(),
		builtin_async(|args, ctx| {
			async move {
				if args.len() >= 2 {
					let actor = ctx.interpreter.to_id(&args[0]);
					let room = ctx.interpreter.to_id(&args[1]);
					ctx.interpreter.world.borrow_mut().object_room.insert(actor, room);

					if room == ctx.interpreter.world.borrow().current_room {
						ctx.interpreter.web_interface.show_object(actor, room).unwrap_or_else(|e| {
							web_sys::console::error_1(&JsValue::from_str(&format!("Error showing object: {:?}", e)));
						});
					}

					web_sys::console::log_1(&JsValue::from_str(&format!("[actor {actor}] appears in room {room}")));
				}
				Null
			}
			.boxed_local()
		}),
	);

	builtins.insert(
		"putActor".into(),
		builtin_async(|args, _| {
			async move {
				if args.len() >= 3 {
					println!("[actor {}] placed at {},{}", args[0].as_string(), args[1].as_string(), args[2].as_string());
				}
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
					println!("[camera] room {} target x {}", args[0].as_string(), args[1].as_string());
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
					println!("[actor {}] walks to {},{}", args[0].as_string(), args[1].as_string(), args[2].as_string());
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
					println!("[actor {}] faces dir {}", args[0].as_string(), args[1].as_string());
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
				if let Some(obj) = args.first() {
					let id = ctx.interpreter.to_id(obj);
					let state = ctx.interpreter.world.borrow().object_state.get(&id).copied().unwrap_or(0);
					Value::Number(state as i32)
				} else {
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
				if args.len() >= 2 {
					let obj = ctx.interpreter.to_id(&args[0]);
					let val = args[1].as_number() as u32;
					ctx.interpreter.world.borrow_mut().object_state.insert(obj, val);

					// Log to web console and potentially update visual state
					web_sys::console::log_1(&JsValue::from_str(&format!("[object {obj}] state set to {val}")));

					// Future enhancement: update object visual appearance based on state
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

					// Log to web console and potentially update inventory display
					web_sys::console::log_1(&JsValue::from_str(&format!("[inventory] added object {id}")));

					// Future enhancement: update visual inventory display
				}
				Null
			}
			.boxed_local()
		}),
	);

	builtins.insert(
		"pickupObject".into(),
		builtin_async(|args, ctx| {
			async move {
				if let Some(obj) = args.first() {
					let id = ctx.interpreter.to_id(obj);
					ctx.interpreter.world.borrow_mut().inventory.insert(id);

					// Remove object from room and hide it in web interface
					ctx.interpreter.web_interface.hide_object(id).unwrap_or_else(|e| {
						web_sys::console::error_1(&JsValue::from_str(&format!("Error hiding object: {:?}", e)));
					});

					web_sys::console::log_1(&JsValue::from_str(&format!("You pick up object {id}")));
				}
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
					web_sys::console::log_1(&JsValue::from_str(&format!("[object {obj_id}] plays anim {anim_id}")));

					// Future enhancement: add CSS animation or transform to the object div
					if let Some(obj_div) = ctx.interpreter.web_interface.document.get_element_by_id(&format!("object-{}", obj_id)) {
						// Simple bounce animation for now
						obj_div
							.set_attribute(
								"style",
								&format!("{}; animation: bounce 0.5s ease-in-out;", obj_div.get_attribute("style").unwrap_or_default()),
							)
							.unwrap_or_else(|e| {
								web_sys::console::error_1(&JsValue::from_str(&format!("Error animating object: {:?}", e)));
							});
					}
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
					println!("[game] loading room {id} – thanks for playing!");
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
