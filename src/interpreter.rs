//! A very small, cooperative‑multitasking interpreter for a subset of
//! Scumm‑style script syntax. *Text‑only* for now – prints to stdout and offers
//! a `wait(ticks)` blocking builtin. Scheduler is cooperative like classic
//! SCUMM. Call `run_scripts(ast)` once you have built the AST.
//!
//! ## Borrow‑checker strategy
//! We cannot mutate `task.stmts` while holding an immutable borrow to a
//! statement inside it.  Therefore `exec_stmt` **returns** a splice action to be
//! applied *after* the borrowed reference is dropped.

use std::collections::{HashMap, VecDeque};

use crate::ast::*;

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
}

// ---------------------------------------------------------------------------
// Environments (locals per task)
// ---------------------------------------------------------------------------

#[derive(Default, Debug)]
struct Env {
	vars: HashMap<String, Value>,
}
impl Env {
	fn get(&self, name: &str) -> Value {
		self.vars.get(name).cloned().unwrap_or(Value::Null)
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
// Splice action returned from exec_stmt (must be at module scope)
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
}

impl<'a> Interpreter<'a> {
	pub fn new(ast: &'a [TopLevel]) -> Self {
		let mut tasks = VecDeque::new();
		if let Some(scr) = ast.iter().find_map(|tl| if let TopLevel::Script(s) = tl { Some(s) } else { None }) {
			tasks.push_back(Task::new(&scr.body));
		}
		let mut this = Self {
			tasks,
			builtins: HashMap::new(),
		};
		this.install_builtins();
		this
	}

	fn install_builtins(&mut self) {
		self.builtins.insert("print".into(), |_, _, args| {
			for v in args {
				match v {
					Value::Number(n) => print!("{}", n),
					Value::Bool(b) => print!("{}", b),
					Value::Str(s) => print!("{}", s),
					Value::Null => print!("<null>"),
				}
			}
			println!();
			Value::Null
		});
		self.builtins.insert("wait".into(), |_, task, mut args| {
			task.delay = args.pop().unwrap_or(Value::Number(1)).as_number() as u32;
			Value::Null
		});
	}

	// ------------------------------------------------------------------
	// Run until no runnable tasks remain
	// ------------------------------------------------------------------

	pub fn run(&mut self) {
		while let Some(mut task) = self.tasks.pop_front() {
			if task.delay > 0 {
				task.delay -= 1;
				self.tasks.push_back(task);
				continue;
			}

			let stmt_ref = match task.next() {
				Some(s) => s,
				None => continue,
			}; // finished
			let action = self.exec_stmt(stmt_ref, &mut task);
			task.ip += 1; // advance before splicing
			// drop borrow explicitly
			let _ = stmt_ref;

			// apply queued splice action
			if let Action::Splice { at, items } = action {
				for (i, s) in items.into_iter().enumerate() {
					task.stmts.insert(at + i, s);
				}
			}

			if task.ip < task.stmts.len() {
				self.tasks.push_back(task);
			}
		}
	}

	// ------------------------------------------------------------------
	// Execute a single statement, returning structural changes
	// ------------------------------------------------------------------

	fn exec_stmt(&mut self, stmt: &'a Statement, task: &mut Task<'a>) -> Action<'a> {
		match stmt {
			// ---------------- expr & var ----------------
			Statement::Expression(e) => {
				self.eval_expr(e, task);
				Action::None
			},
			Statement::VariableDeclaration(v) => {
				let val = self.eval_expr(&v.value, task);
				task.env.set(v.name.clone(), val);
				Action::None
			},

			// ---------------- IF ------------------------
			Statement::If(ifst) => {
				// choose appropriate block without referencing a temporary
				let chosen: &Block = if self.eval_expr(&ifst.condition, task).truthy() {
					&ifst.then_block
				} else if let Some(ref else_block) = ifst.else_block {
					else_block
				} else {
					return Action::None; // nothing to execute
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

			// ---------------- WHILE ---------------------
			Statement::While(wh) => {
				if self.eval_expr(&wh.condition, task).truthy() {
					let mut items: Vec<&'a Statement> = wh.body.statements.iter().collect();
					items.push(stmt); // loop back
					Action::Splice { at: task.ip + 1, items }
				} else {
					Action::None
				}
			},

			_ => Action::None, // others unimplemented for now
		}
	}

	// ------------------------------------------------------------------
	// Expression evaluation
	// ------------------------------------------------------------------

	fn eval_expr(&mut self, expr: &'a Expression, task: &mut Task<'a>) -> Value {
		match expr {
			Expression::Primary(p) => self.eval_primary(p, task),
			Expression::Assignment(lhs, rhs) => {
				if let Expression::Primary(crate::ast::Primary::Identifier(name)) = &**lhs {
					let v = self.eval_expr(rhs, task);
					task.env.set(name.clone(), v.clone());
					v
				} else {
					Value::Null
				}
			},
			Expression::LogicalOr(a, b) => {
				let av = self.eval_expr(a, task);
				if av.truthy() { av } else { self.eval_expr(b, task) }
			},
			Expression::LogicalAnd(a, b) => {
				let av = self.eval_expr(a, task);
				if !av.truthy() { av } else { self.eval_expr(b, task) }
			},
			Expression::Equality(a, op, b) => Value::Bool(match op {
				EqualityOp::Equal => self.eval_expr(a, task) == self.eval_expr(b, task),
				EqualityOp::NotEqual => self.eval_expr(a, task) != self.eval_expr(b, task),
			}),
			Expression::Comparison(a, op, b) => {
				let av = self.eval_expr(a, task).as_number();
				let bv = self.eval_expr(b, task).as_number();
				Value::Bool(match op {
					ComparisonOp::Less => av < bv,
					ComparisonOp::Greater => av > bv,
					ComparisonOp::LessEqual => av <= bv,
					ComparisonOp::GreaterEqual => av >= bv,
				})
			},
			Expression::Term(a, op, b) => {
				let av = self.eval_expr(a, task).as_number();
				let bv = self.eval_expr(b, task).as_number();
				Value::Number(match op {
					TermOp::Add => av + bv,
					TermOp::Subtract => av - bv,
				})
			},
			Expression::Factor(a, op, b) => {
				let av = self.eval_expr(a, task).as_number();
				let bv = self.eval_expr(b, task).as_number();
				Value::Number(match op {
					FactorOp::Multiply => av * bv,
					FactorOp::Divide => av / bv,
				})
			},
			Expression::Unary(u, e) => {
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
			crate::ast::Primary::Identifier(id) => task.env.get(id),
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
}

// ---------------------------------------------------------------------------
// Convenience wrapper
// ---------------------------------------------------------------------------

pub fn run_scripts(ast: &[TopLevel]) {
	let mut i = Interpreter::new(ast);
	i.run();
}
