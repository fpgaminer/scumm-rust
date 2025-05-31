use crate::interpreter::{Ctx, Value};
use async_recursion::async_recursion;

#[derive(Debug, Clone, PartialEq)]
pub enum TopLevel {
	Script(Script),
	Object(Object),
	Class(Class),
	Directive(String, String), // directive name and value
}

#[derive(Debug, Clone, PartialEq)]
pub struct Script {
	pub name: ScriptName,
	pub body: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Object {
	pub id: String,
	pub name: String,
	pub body: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Class {
	pub name: String,
	pub body: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ScriptName {
	Number(u32),
	Identifier(String),
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
	State(StateStatement),
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
	Number(u32),
	String(String),
	Identifier(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct StateStatement {
	pub number: u32,
	pub assignments: Vec<(String, Primary)>,
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
				} else if let Some(c) = ctx.interpreter.consts.get(id) {
					Ok(c.clone())
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
