use crate::interpreter::{Ctx, Declaration, Interpreter, ObjectDef, RoomDef, Value};
use async_recursion::async_recursion;

#[derive(Debug, Clone, PartialEq)]
pub enum TopLevel {
	Script(Script),
	Object(Object),
	Class(Class),
	Room(Room),
}

impl TopLevel {
	pub fn exec(&self, interp: &Interpreter) -> Result<u32, anyhow::Error> {
		match self {
			TopLevel::Script(script) => script.exec(interp),
			TopLevel::Object(obj) => obj.exec(interp),
			TopLevel::Class(class) => class.exec(interp),
			TopLevel::Room(room) => room.exec(interp),
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct Script {
	pub name: String,
	pub body: Block,
}

impl Script {
	pub fn exec(&self, interp: &Interpreter) -> Result<u32, anyhow::Error> {
		interp.add_declaration(&self.name, Declaration::Script(self.body.clone()))
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct Object {
	pub id: String,
	pub body: Block,
}

impl Object {
	pub fn exec(&self, interp: &Interpreter) -> Result<u32, anyhow::Error> {
		let mut obj = ObjectDef::default();

		for stmt in &self.body.statements {
			match stmt {
				Statement::PropertyAssignment(prop) if prop.name == "name" => {
					if let PropertyValue::String(name) = &prop.value {
						obj.name = name.clone();
					} else {
						anyhow::bail!("Property 'name' must be a string");
					}
				},
				Statement::PropertyAssignment(prop) if prop.name == "state" => {
					if let PropertyValue::Number(state) = &prop.value {
						obj.state = (*state).try_into().map_err(|_| anyhow::anyhow!("State must be >= 0"))?;
					} else {
						anyhow::bail!("Property 'state' must be a number");
					}
				},
				Statement::PropertyAssignment(prop) if prop.name == "x" || prop.name == "y" || prop.name == "w" || prop.name == "h" => {
					if let PropertyValue::Number(v) = &prop.value {
						match prop.name.as_str() {
							"x" => obj.x = *v,
							"y" => obj.y = *v,
							"w" => obj.width = (*v).try_into().map_err(|_| anyhow::anyhow!("Width must be >= 0"))?,
							"h" => obj.height = (*v).try_into().map_err(|_| anyhow::anyhow!("Height must be >= 0"))?,
							_ => unreachable!(), // Handled above
						}
					} else {
						anyhow::bail!("Property '{}' must be a number", prop.name);
					}
				},
				Statement::States(entries) => {
					obj.states = entries.iter().map(|e| e.image.clone()).collect();
				},
				Statement::PropertyAssignment(prop) => {
					anyhow::bail!("Unsupported property assignment in object definition: {:?}", prop);
				},
				Statement::Verb(verb_stmt) => {
					if let Some(body) = &verb_stmt.body {
						obj.verbs.insert(verb_stmt.name.clone(), body.clone());
					} else {
						obj.verbs.insert(verb_stmt.name.clone(), Block { statements: vec![] });
					}
				},
				_ => anyhow::bail!("Unsupported statement in object definition: {:?}", stmt),
			}
		}

		interp.add_declaration(&self.id, Declaration::Object(obj))
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct Class {
	pub name: String,
	pub body: Block,
}

impl Class {
	pub fn exec(&self, interp: &Interpreter) -> Result<u32, anyhow::Error> {
		let mut class_def = ObjectDef {
			name: self.name.clone(),
			..Default::default()
		};

		for stmt in &self.body.statements {
			match stmt {
				Statement::Verb(verb_stmt) => {
					if let Some(body) = &verb_stmt.body {
						class_def.verbs.insert(verb_stmt.name.clone(), body.clone());
					} else {
						class_def.verbs.insert(verb_stmt.name.clone(), Block { statements: vec![] });
					}
				},
				_ => anyhow::bail!("Unsupported statement in class definition: {:?}", stmt),
			}
		}

		interp.add_declaration(&self.name, Declaration::Class(class_def))
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct Room {
	pub id: String,
	pub body: Block,
}

impl Room {
	pub fn exec(&self, interp: &Interpreter) -> Result<u32, anyhow::Error> {
		let mut room_def = RoomDef::default();
		let mut objects = Vec::new();
		room_def.name = self.id.clone();

		for stmt in &self.body.statements {
			match stmt {
				Statement::PropertyAssignment(prop) if prop.name == "image" => {
					if let PropertyValue::String(image) = &prop.value {
						room_def.image = Some(image.clone());
					} else {
						anyhow::bail!("Property 'image' must be a string");
					}
				},
				Statement::ObjectDeclaration(obj_stmt) => {
					objects.push(obj_stmt.exec(interp)?);
				},
				Statement::ScriptDeclaration(script) if script.name == "entry" => {
					// Special handling for entry script
					room_def.entry_script = Some(script.body.clone());
				},
				_ => anyhow::bail!("Unsupported statement in room definition: {:?}", stmt),
			}
		}

		let room_id = interp.add_declaration(&self.id, Declaration::Room(room_def))?;
		for obj_id in objects {
			interp.with_object_by_id_mut(obj_id, |obj| {
				obj.room = room_id;
			});
		}

		Ok(room_id)
	}
}


#[derive(Debug, Clone, PartialEq)]
pub struct Block {
	pub statements: Vec<Statement>,
}

impl Block {
	pub async fn exec(&self, ctx: &Ctx) -> Result<(), anyhow::Error> {
		for stmt in &self.statements {
			stmt.exec(ctx).await?;
		}

		Ok(())
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
	Block(Block),
	Expression(Expression),
	If(IfStatement),
	While(WhileStatement),
	ClassDeclaration(String),
	Verb(VerbStatement),
	VariableDeclaration(VariableDeclaration),
	PropertyAssignment(PropertyAssignment),
	States(Vec<StateEntry>),
	State(StateStatement),
	ObjectDeclaration(Object),
	ScriptDeclaration(Script),
}

impl Statement {
	#[async_recursion(?Send)]
	pub async fn exec(&self, ctx: &Ctx) -> Result<(), anyhow::Error> {
		match self {
			Statement::Expression(e) => e.exec(ctx).await.map(|_| ()),
			Statement::Block(block) => block.exec(ctx).await,
			Statement::VariableDeclaration(v) => {
				let val = v.value.exec(ctx).await?;
				ctx.vars.borrow_mut().insert(v.name.clone(), val);
				Ok(())
			},
			Statement::If(if_stmt) => {
				let condition = if_stmt.condition.exec(ctx).await?;
				if condition.truthy() {
					if_stmt.then_block.exec(ctx).await
				} else if let Some(else_block) = &if_stmt.else_block {
					else_block.exec(ctx).await
				} else {
					Ok(())
				}
			},
			Statement::While(while_stmt) => {
				while while_stmt.condition.exec(ctx).await?.truthy() {
					while_stmt.body.exec(ctx).await?;
				}
				Ok(())
			},
			_ => anyhow::bail!("Execution not implemented for this statement type: {:?}", self),
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfStatement {
	pub condition: Expression,
	pub then_block: Block,
	pub else_block: Option<Block>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileStatement {
	pub condition: Expression,
	pub body: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VerbStatement {
	pub name: String,
	pub body: Option<Block>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableDeclaration {
	pub var_type: String,
	pub name: String,
	pub value: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PropertyAssignment {
	pub name: String,
	pub value: PropertyValue,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PropertyValue {
	Number(i32),
	String(String),
	Identifier(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct StateStatement {
	pub number: u32,
	pub assignments: Vec<(String, Primary)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StateEntry {
	pub x: i32,
	pub y: i32,
	pub image: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
	Assignment(Box<Expression>, Box<Expression>),
	LogicalOr(Box<Expression>, Box<Expression>),
	LogicalAnd(Box<Expression>, Box<Expression>),
	Equality(Box<Expression>, EqualityOp, Box<Expression>),
	Comparison(Box<Expression>, ComparisonOp, Box<Expression>),
	Term(Box<Expression>, TermOp, Box<Expression>),
	Factor(Box<Expression>, FactorOp, Box<Expression>),
	Unary(UnaryOp, Box<Expression>),
	Primary(Primary),
}

impl Expression {
	#[async_recursion(?Send)]
	pub async fn exec(&self, ctx: &Ctx) -> Result<Value, anyhow::Error> {
		match self {
			Expression::Primary(primary) => Ok(primary.exec(ctx).await?),
			Expression::Assignment(lhs, rhs) => {
				if let Expression::Primary(Primary::Identifier(name)) = &**lhs {
					let v = rhs.exec(ctx).await?;
					ctx.vars.borrow_mut().insert(name.clone(), v.clone());
					return Ok(v);
				} else {
					anyhow::bail!("Left-hand side of assignment must be an identifier, found: {:?}", lhs);
				}
			},
			Expression::LogicalOr(a, b) => {
				let av = a.exec(ctx).await?;
				if av.truthy() { Ok(av) } else { b.exec(ctx).await }
			},
			Expression::LogicalAnd(a, b) => {
				let av = a.exec(ctx).await?;
				if av.truthy() { b.exec(ctx).await } else { Ok(av) }
			},
			Expression::Equality(lhs, op, rhs) => Ok(Value::Bool(match op {
				EqualityOp::Equal => lhs.exec(ctx).await? == rhs.exec(ctx).await?,
				EqualityOp::NotEqual => lhs.exec(ctx).await? != rhs.exec(ctx).await?,
			})),
			Expression::Comparison(lhs, op, rhs) => {
				let av = lhs.exec(ctx).await?.as_number();
				let bv = rhs.exec(ctx).await?.as_number();
				Ok(Value::Bool(match op {
					ComparisonOp::Less => av < bv,
					ComparisonOp::Greater => av > bv,
					ComparisonOp::LessEqual => av <= bv,
					ComparisonOp::GreaterEqual => av >= bv,
				}))
			},
			Expression::Term(lhs, op, rhs) => {
				let av = lhs.exec(ctx).await?.as_number();
				let bv = rhs.exec(ctx).await?.as_number();
				Ok(Value::Number(match op {
					TermOp::Add => av + bv,
					TermOp::Subtract => av - bv,
				}))
			},
			Expression::Factor(lhs, op, rhs) => {
				let av = lhs.exec(ctx).await?.as_number();
				let bv = rhs.exec(ctx).await?.as_number();
				Ok(Value::Number(match op {
					FactorOp::Multiply => av * bv,
					FactorOp::Divide => av / bv,
				}))
			},
			Expression::Unary(op, expr) => {
				let val = expr.exec(ctx).await?;
				Ok(match op {
					UnaryOp::Not => Value::Bool(!val.truthy()),
					UnaryOp::Negate => Value::Number(-val.as_number()),
				})
			},
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum EqualityOp {
	Equal,
	NotEqual,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ComparisonOp {
	Less,
	Greater,
	LessEqual,
	GreaterEqual,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TermOp {
	Add,
	Subtract,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FactorOp {
	Multiply,
	Divide,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
	Not,
	Negate,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Primary {
	FunctionCall(FunctionCall),
	Number(u32),
	String(String),
	Identifier(String),
	Parenthesized(Box<Expression>),
}

impl Primary {
	pub async fn exec(&self, ctx: &Ctx) -> Result<Value, anyhow::Error> {
		match self {
			Primary::Number(n) => Ok(Value::Number(*n as i32)),
			Primary::String(s) => Ok(Value::Str(s.clone())),

			Primary::Identifier(id) => {
				if let Some(v) = ctx.vars.borrow().get(id) {
					Ok(v.clone())
				} else if let Some(c) = ctx.interpreter.declarations.borrow().get_index_of(id) {
					Ok(Value::Number(c as i32))
				} else {
					// Treat unknown identifier as its literal name (useful for script symbols)
					Ok(Value::Str(id.clone()))
				}
			},

			Primary::FunctionCall(call) => call.exec(ctx).await,
			Primary::Parenthesized(expr) => expr.exec(ctx).await,
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCall {
	pub name: String,
	pub arguments: Vec<Expression>,
}

impl FunctionCall {
	pub async fn exec(&self, ctx: &Ctx) -> Result<Value, anyhow::Error> {
		let mut vals = Vec::with_capacity(self.arguments.len());
		for arg in &self.arguments {
			vals.push(arg.exec(ctx).await?);
		}

		if let Some(f) = ctx.interpreter.builtins.get(&self.name) {
			Ok(f(vals, ctx).await)
		} else {
			anyhow::bail!("Function '{}' not found", self.name)
		}
	}
}
