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

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCall {
	pub name: String,
	pub arguments: Vec<Expression>,
}
