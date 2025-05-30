//! A very small, cooperative‑multitasking interpreter for a subset of
//! Scumm‑style script syntax. *Text‑only* for now – prints to stdout and offers
//! a `wait(ticks)` blocking builtin. Scheduler is cooperative like classic
//! SCUMM. Call `run_scripts(ast)` once you have built the AST.
//!
//! Compared with the previous version this file now supports:
//! * Nested `{ … }` blocks (executed by splicing their contents)
//! * A global constant table populated from `#define` directives
//! * A registry of *all* scripts so `startScript()` can spawn them by number or
//!   identifier at run‑time
//! * A minimal world model (object states, rooms, inventory)
//! * Many new built‑ins used by `example.sc`: `sayLine`, `prompt`,
//!   `startScript`, `breakHere`, `putActorInRoom`, `putActor`, `setCameraAt`,
//!   `walkActorTo`, `faceActor`, `getState`, `setState`, `objectInHand`,
//!   `animateObject`, `addToInventory`, `pickupObject`, `loadRoom`.
//!
//! This is **still** a stub implementation – its purpose is to let the example
//! game run end‑to‑end in a terminal and demonstrate control‑flow. Feel free to
//! flesh out the data model or add more opcodes later.

use std::{
	collections::{HashMap, HashSet, VecDeque},
	io::{self, Write},
};

use crate::ast::*;

// ---------------------------------------------------------------------------
// Runtime object definition (built from the AST once at start-up)
// ---------------------------------------------------------------------------
struct ObjectDef<'a> {
	id: i32,
	name: String,                      // human readable name
	verbs: HashMap<String, &'a Block>, // vLook, vOpen, …
	init_room: i32,                    // 0 = nowhere / inventory only
	init_state: u32,
}


// ---------------------------------------------------------------------------
// Dynamic value type
// ---------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
	Number(i32),
	Bool(bool),
	Str(String),
	Null,
}

impl Value {
	fn truthy(&self) -> bool {
		match self {
			Value::Bool(b) => *b,
			Value::Number(n) => *n != 0,
			Value::Str(s) => !s.is_empty(),
			Value::Null => false,
		}
	}
	fn as_number(&self) -> i32 {
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
	fn as_string(&self) -> String {
		match self {
			Value::Str(s) => s.clone(),
			Value::Number(n) => n.to_string(),
			Value::Bool(b) => b.to_string(),
			Value::Null => "<null>".into(),
		}
	}
}

// ---------------------------------------------------------------------------
// Environments (locals per task)
// ---------------------------------------------------------------------------

#[derive(Default, Debug)]
struct Env {
	vars: HashMap<String, Value>,
}
impl Env {
	fn get(&self, name: &str) -> Option<Value> {
		self.vars.get(name).cloned()
	}
	fn set(&mut self, name: String, value: Value) {
		self.vars.insert(name, value);
	}
}

// ---------------------------------------------------------------------------
// Task = running script instance
// ---------------------------------------------------------------------------

#[derive(Debug)]
struct Task<'a> {
	stmts: Vec<&'a Statement>,
	ip: usize,
	env: Env,
	delay: u32,
}
impl<'a> Task<'a> {
	fn new(block: &'a Block) -> Self {
		Self {
			stmts: block.statements.iter().collect(),
			ip: 0,
			env: Env::default(),
			delay: 0,
		}
	}
	fn next(&self) -> Option<&'a Statement> {
		self.stmts.get(self.ip).copied()
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

// ---------------------------------------------------------------------------
// Splice action returned from exec_stmt
// ---------------------------------------------------------------------------

enum Action<'a> {
	None,
	Splice { at: usize, items: Vec<&'a Statement> },
}

// ---------------------------------------------------------------------------
// Interpreter / scheduler
// ---------------------------------------------------------------------------

type BuiltinFn = fn(&mut Interpreter, &mut Task, Vec<Value>) -> Value;

pub struct Interpreter<'a> {
	tasks: VecDeque<Task<'a>>, // RUNNABLE queue
	builtins: HashMap<String, BuiltinFn>,
	scripts: HashMap<String, &'a Script>,
	consts: HashMap<String, Value>,
	world: World,
	objects: HashMap<i32, ObjectDef<'a>>, // id → def
	object_names: HashMap<String, i32>,   // lowercase name → id  (for parser)
}

impl<'a> Interpreter<'a> {
	// --------------------------------------------------
	// Construction
	// --------------------------------------------------
	pub fn new(ast: &'a [TopLevel]) -> Self {
		let mut scripts = HashMap::new();
		let mut consts = HashMap::new();

		// Pass 1 – collect scripts & defines
		for tl in ast {
			match tl {
				TopLevel::Script(s) => match &s.name {
					ScriptName::Identifier(name) => {
						scripts.insert(name.clone(), s);
					},
					ScriptName::Number(n) => {
						scripts.insert(n.to_string(), s);
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
				_ => {},
			}
		}

		// ------------------------------------------------------------------
		// pass 1b – harvest every class’ verbs so we can attach them to
		//           objects that declare `class  <name>;` later on
		// ------------------------------------------------------------------
		let mut class_verbs: HashMap<String, HashMap<String, &'a Block>> = HashMap::new();

		for tl in ast {
			if let TopLevel::Class(c) = tl {
				let mut vmap = HashMap::new();
				for st in &c.body.statements {
					if let Statement::Verb(v) = st {
						if let Some(body) = &v.body {
							vmap.insert(v.name.clone(), body);   // <-- NO extra ‘v’
						}
					}
				}
				class_verbs.insert(c.name.clone(), vmap);
			}
		}


		let mut objects = HashMap::new();
		let mut object_names = HashMap::new();

		for tl in ast {
			if let TopLevel::Object(o) = tl {
				// Resolve numeric id ⇒ the identifier *must* be in the #define table
				let id_num = match consts.get(&o.id) {
					Some(Value::Number(n)) => *n,
					_ => {
						eprintln!("[warn] object id {} has no #define – using 0", o.id);
						0
					},
				};

				let mut verbs = HashMap::new();
				let mut declared_classes = Vec::new();
				let mut init_room = 0;
				let mut init_state = 0;
				let mut has_init_room = false;

				for st in &o.body.statements {
					match st {
						Statement::Verb(v) if v.body.is_some() => {
							verbs.insert(v.name.clone(), v.body.as_ref().unwrap());
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
							verbs.entry(vname.clone()).or_insert(*block);
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

		// Spawn script with ID 1 (ROOM_ID) as entry point, or fallback to first script
		let mut tasks = VecDeque::new();

		// Try to find script with ID "1" first (ROOM_ID convention)
		let entry_script = scripts
			.get("1")
			.or_else(|| scripts.get("ROOM_ID"))
			.or_else(|| scripts.iter().next().map(|(_, script)| script));

		if let Some(scr) = entry_script {
			let script_name = scripts
				.iter()
				.find(|(_, script)| std::ptr::eq(*script, scr))
				.map(|(name, _)| name.as_str())
				.unwrap_or("unknown");
			eprintln!("[info] starting script {script_name}");
			tasks.push_back(Task::new(&scr.body));
		}

		let mut this = Self {
			tasks,
			builtins: HashMap::new(),
			scripts,
			consts,
			world,
			objects,
			object_names,
		};
		this.install_builtins();
		this
	}

	// --------------------------------------------------
	// Built‑ins
	// --------------------------------------------------
	fn install_builtins(&mut self) {
		use Value::*;

		self.builtins.insert("print".into(), |_, _, args| {
			for v in args {
				print!("{}", v.as_string());
			}
			println!();
			Null
		});

		self.builtins.insert("wait".into(), |_, task, mut args| {
			let ticks = args.pop().unwrap_or(Number(1)).as_number();
			task.delay = ticks as u32;
			Null
		});

		self.builtins.insert("breakHere".into(), |_, task, _| {
			task.delay = 1; // yield for one tick
			Null
		});

		self.builtins.insert("sayLine".into(), |_, _, args| {
			if args.len() >= 2 {
				let txt = args[1].as_string();
				println!("{}", txt.trim_matches('"'));
			}
			Null
		});

		self.builtins.insert("prompt".into(), |_, _, args| {
			let msg = args.get(0).map(|v| v.as_string()).unwrap_or_default();
			print!("{}", msg.trim_matches('"'));
			io::stdout().flush().ok();
			let mut input = String::new();
			io::stdin().read_line(&mut input).ok();
			Value::Str(input.trim().to_string())
		});

		self.builtins.insert("startScript".into(), |interp, _, mut args| {
			if let Some(id) = args.pop() {
				interp.spawn_script_value(id);
			}
			Null
		});

		// ---- World manipulation stubs ----
		self.builtins.insert("putActorInRoom".into(), |interp, _, args| {
			if args.len() >= 2 {
				let actor = interp.to_id(&args[0]);
				let room = interp.to_id(&args[1]);
				interp.world.object_room.insert(actor, room);
				println!("[actor {actor}] appears in room {room}");
			}
			Null
		});

		self.builtins.insert("putActor".into(), |_, _, args| {
			if args.len() >= 3 {
				println!("[actor {}] placed at {},{}", args[0].as_string(), args[1].as_string(), args[2].as_string());
			}
			Null
		});
		self.builtins.insert("setCameraAt".into(), |_, _, args| {
			if args.len() >= 2 {
				println!("[camera] room {} target x {}", args[0].as_string(), args[1].as_string());
			}
			Null
		});
		self.builtins.insert("walkActorTo".into(), |_, _, args| {
			if args.len() >= 3 {
				println!("[actor {}] walks to {},{}", args[0].as_string(), args[1].as_string(), args[2].as_string());
			}
			Null
		});
		self.builtins.insert("faceActor".into(), |_, _, args| {
			if args.len() >= 2 {
				println!("[actor {}] faces dir {}", args[0].as_string(), args[1].as_string());
			}
			Null
		});

		self.builtins.insert("getState".into(), |interp, _, args| {
			if let Some(obj) = args.get(0) {
				let id = interp.to_id(obj);
				let state = interp.world.object_state.get(&id).copied().unwrap_or(0);
				Value::Number(state as i32)
			} else {
				Null
			}
		});

		self.builtins.insert("setState".into(), |interp, _, args| {
			if args.len() >= 2 {
				let obj = interp.to_id(&args[0]);
				let val = args[1].as_number() as u32;
				interp.world.object_state.insert(obj, val);
				println!("[object {obj}] state set to {val}");
			}
			Null
		});

		self.builtins.insert("objectInHand".into(), |interp, _, args| {
			if let Some(obj) = args.get(0) {
				let id = interp.to_id(obj);
				Value::Bool(interp.world.inventory.contains(&id))
			} else {
				Value::Bool(false)
			}
		});

		self.builtins.insert("addToInventory".into(), |interp, _, args| {
			if let Some(obj) = args.get(0) {
				let id = interp.to_id(obj);
				interp.world.inventory.insert(id);
				println!("[inventory] added object {id}");
			}
			Null
		});

		self.builtins.insert("pickupObject".into(), |interp, _, args| {
			if let Some(obj) = args.get(0) {
				let id = interp.to_id(obj);
				interp.world.inventory.insert(id);
				println!("You pick up object {id}");
			}
			Null
		});

		self.builtins.insert("animateObject".into(), |_, _, args| {
			if args.len() >= 2 {
				println!("[object {}] plays anim {}", args[0].as_string(), args[1].as_string());
			}
			Null
		});

		self.builtins.insert("loadRoom".into(), |interp, _, args| {
			if let Some(room) = args.get(0) {
				let id = interp.to_id(room);
				println!("[game] loading room {id} – thanks for playing!");
				interp.tasks.clear(); // stop
			}
			Null
		});
	}

	// --------------------------------------------------
	// Script spawning helpers
	// --------------------------------------------------
	fn spawn_script_value(&mut self, v: Value) {
		match v {
			Value::Number(n) => self.spawn_script(&n.to_string()),
			Value::Str(s) => self.spawn_script(&s),
			_ => {},
		}
	}

	fn spawn_script(&mut self, key: &str) {
		if let Some(scr) = self.scripts.get(key) {
			eprintln!("[info] startScript -> {}", key);
			self.tasks.push_back(Task::new(&scr.body));
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
	// Run until no runnable tasks remain
	// --------------------------------------------------
	pub fn run(&mut self) {
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
				let act = self.exec_stmt(stmt_ref, &mut task);
				task.ip += 1;
				if let Action::Splice { at, items } = act {
					for (i, s) in items.into_iter().enumerate() {
						task.stmts.insert(at + i, s);
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
	}


	// --------------------------------------------------
	// Execute a single statement, returning structural changes
	// --------------------------------------------------
	fn exec_stmt(&mut self, stmt: &'a Statement, task: &mut Task<'a>) -> Action<'a> {
		match stmt {
			// ---- nested block ----
			Statement::Block(b) => {
				if b.statements.is_empty() {
					Action::None
				} else {
					Action::Splice {
						at: task.ip + 1,
						items: b.statements.iter().collect(),
					}
				}
			},
			// ---- expr & var ----
			Statement::Expression(e) => {
				self.eval_expr(e, task);
				Action::None
			},
			Statement::VariableDeclaration(v) => {
				let val = self.eval_expr(&v.value, task);
				task.env.set(v.name.clone(), val);
				Action::None
			},
			// ---- IF ----
			Statement::If(ifst) => {
				let chosen: &Block = if self.eval_expr(&ifst.condition, task).truthy() {
					&ifst.then_block
				} else if let Some(ref else_block) = ifst.else_block {
					else_block
				} else {
					return Action::None;
				};
				if chosen.statements.is_empty() {
					Action::None
				} else {
					Action::Splice {
						at: task.ip + 1,
						items: chosen.statements.iter().collect(),
					}
				}
			},
			// ---- WHILE ----
			Statement::While(wh) => {
				if self.eval_expr(&wh.condition, task).truthy() {
					let mut items: Vec<&'a Statement> = wh.body.statements.iter().collect();
					items.push(stmt); // loop back
					Action::Splice { at: task.ip + 1, items }
				} else {
					Action::None
				}
			},
			// Everything else (class / verb / etc.) is no‑op at run‑time for now.
			_ => Action::None,
		}
	}

	// --------------------------------------------------
	// Expression evaluation
	// --------------------------------------------------
	fn eval_expr(&mut self, expr: &'a Expression, task: &mut Task<'a>) -> Value {
		use Expression::*;
		match expr {
			Primary(p) => self.eval_primary(p, task),
			Assignment(lhs, rhs) => {
				if let Expression::Primary(crate::ast::Primary::Identifier(name)) = &**lhs {
					let v = self.eval_expr(rhs, task);
					task.env.set(name.clone(), v.clone());
					v
				} else {
					Value::Null
				}
			},
			LogicalOr(a, b) => {
				let av = self.eval_expr(a, task);
				if av.truthy() { av } else { self.eval_expr(b, task) }
			},
			LogicalAnd(a, b) => {
				let av = self.eval_expr(a, task);
				if !av.truthy() { av } else { self.eval_expr(b, task) }
			},
			Equality(a, op, b) => Value::Bool(match op {
				EqualityOp::Equal => self.eval_expr(a, task) == self.eval_expr(b, task),
				EqualityOp::NotEqual => self.eval_expr(a, task) != self.eval_expr(b, task),
			}),
			Comparison(a, op, b) => {
				let av = self.eval_expr(a, task).as_number();
				let bv = self.eval_expr(b, task).as_number();
				Value::Bool(match op {
					ComparisonOp::Less => av < bv,
					ComparisonOp::Greater => av > bv,
					ComparisonOp::LessEqual => av <= bv,
					ComparisonOp::GreaterEqual => av >= bv,
				})
			},
			Term(a, op, b) => {
				let av = self.eval_expr(a, task).as_number();
				let bv = self.eval_expr(b, task).as_number();
				Value::Number(match op {
					TermOp::Add => av + bv,
					TermOp::Subtract => av - bv,
				})
			},
			Factor(a, op, b) => {
				let av = self.eval_expr(a, task).as_number();
				let bv = self.eval_expr(b, task).as_number();
				Value::Number(match op {
					FactorOp::Multiply => av * bv,
					FactorOp::Divide => av / bv,
				})
			},
			Unary(u, e) => {
				let v = self.eval_expr(e, task);
				match u {
					UnaryOp::Not => Value::Bool(!v.truthy()),
					UnaryOp::Negate => Value::Number(-v.as_number()),
				}
			},
		}
	}

	fn eval_primary(&mut self, prim: &'a crate::ast::Primary, task: &mut Task<'a>) -> Value {
		match prim {
			crate::ast::Primary::Number(n) => Value::Number(*n as i32),
			crate::ast::Primary::String(s) => Value::Str(s.clone()),
			crate::ast::Primary::Identifier(id) => {
				if let Some(v) = task.env.get(id) {
					v
				} else if let Some(c) = self.consts.get(id) {
					c.clone()
				} else {
					// Treat unknown identifier as its literal name (useful for script symbols)
					Value::Str(id.clone())
				}
			},
			crate::ast::Primary::Parenthesized(expr) => self.eval_expr(expr, task),
			crate::ast::Primary::FunctionCall(fc) => {
				let args = fc.arguments.iter().map(|e| self.eval_expr(e, task)).collect::<Vec<_>>();
				if let Some(f) = self.builtins.get(&fc.name) {
					f(self, task, args)
				} else {
					println!("[warn] unknown function {}", fc.name);
					Value::Null
				}
			},
		}
	}

	fn run_verb(&mut self, obj_id: i32, verb: &str, maybe_target: Option<i32>) -> bool {
		if let Some(def) = self.objects.get(&obj_id) {
			if let Some(block) = def.verbs.get(verb) {
				let mut t = Task::new(block);
				t.env.set("this".into(), Value::Number(obj_id));
				if let Some(tgt) = maybe_target {
					t.env.set("verbObj".into(), Value::Number(tgt));
				}
				self.tasks.push_back(t);
				return true;            // ――― found & queued
			}
		}
		false                             // ――― no such verb here
	}


	fn describe_room(&self) {
		println!();
		println!("You are in room {}.", self.world.current_room);
		let here: Vec<&ObjectDef> = self
			.objects
			.values()
			.filter(|o| self.world.object_room.get(&o.id) == Some(&self.world.current_room))
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

// ---------------------------------------------------------------------------
// Convenience wrapper
// ---------------------------------------------------------------------------

pub fn run_scripts(ast: &[TopLevel]) {
	let mut i = Interpreter::new(ast);
	i.run();
}


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
