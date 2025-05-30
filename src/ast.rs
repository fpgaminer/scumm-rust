#[derive(Debug)]
pub enum TopLevel {
	Script(Script),
	Object(Object),
	Class(Class),
	Directive(String, String), // directive name and value
}

#[derive(Debug)]
pub struct Script {
	pub name: ScriptName,
	pub body: Block,
}

#[derive(Debug)]
pub struct Object {
	pub id: String,
	pub name: String,
	pub body: Block,
}

#[derive(Debug)]
pub struct Class {
	pub name: String,
	pub body: Block,
}

#[derive(Debug)]
pub enum ScriptName {
	Number(u32),
	Identifier(String),
}

#[derive(Debug)]
pub struct Block {
	pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub enum Statement {
	Block(Block),
	Expression(Expression),
	If(IfStatement),
	While(WhileStatement),
	ClassDeclaration(String),
	VerbStatement(VerbStatement),
	VariableDeclaration(VariableDeclaration),
	PropertyAssignment(PropertyAssignment),
	StateStatement(StateStatement),
}

#[derive(Debug)]
pub struct IfStatement {
	pub condition: Expression,
	pub then_block: Block,
	pub else_block: Option<Block>,
}

#[derive(Debug)]
pub struct WhileStatement {
	pub condition: Expression,
	pub body: Block,
}

#[derive(Debug)]
pub struct VerbStatement {
	pub name: String,
	pub body: Option<Block>,
}

#[derive(Debug)]
pub struct VariableDeclaration {
	pub var_type: String,
	pub name: String,
	pub value: Expression,
}

#[derive(Debug)]
pub struct PropertyAssignment {
	pub name: String,
	pub value: PropertyValue,
}

#[derive(Debug)]
pub enum PropertyValue {
	Number(u32),
	String(String),
	Identifier(String),
}

#[derive(Debug)]
pub struct StateStatement {
	pub number: u32,
	pub assignments: Vec<(String, Primary)>,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub enum EqualityOp {
	Equal,
	NotEqual,
}

#[derive(Debug)]
pub enum ComparisonOp {
	Less,
	Greater,
	LessEqual,
	GreaterEqual,
}

#[derive(Debug)]
pub enum TermOp {
	Add,
	Subtract,
}

#[derive(Debug)]
pub enum FactorOp {
	Multiply,
	Divide,
}

#[derive(Debug)]
pub enum UnaryOp {
	Not,
	Negate,
}

#[derive(Debug)]
pub enum Primary {
	FunctionCall(FunctionCall),
	Number(u32),
	String(String),
	Identifier(String),
	Parenthesized(Box<Expression>),
}

#[derive(Debug)]
pub struct FunctionCall {
	pub name: String,
	pub arguments: Vec<Expression>,
}
