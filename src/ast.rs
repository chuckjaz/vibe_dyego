#[derive(Debug, PartialEq)]
#[allow(dead_code)]
pub enum Type {
    Simple(SimpleType),
    Union(Vec<SimpleType>),
}

#[derive(Debug, PartialEq)]
pub struct SimpleType {
    pub base: BaseType,
    pub specifiers: Vec<TypeSpecifier>,
}

#[derive(Debug, PartialEq)]
#[allow(dead_code)]
pub enum BaseType {
    User(String),
    Tuple(TupleType),
    Object(ObjectType),
    Value(ValueType),
}

#[derive(Debug, PartialEq)]
pub struct ObjectType {
    pub fields: Vec<ValueField>,
    pub functions: Vec<FunctionSignature>,
}

#[derive(Debug, PartialEq)]
pub struct ValueType {
    pub fields: Vec<ValueField>,
    pub functions: Vec<FunctionSignature>,
}

#[derive(Debug, PartialEq)]
pub struct FunctionSignature {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub return_type: Type,
}

#[derive(Debug, PartialEq)]
pub struct TupleType {
    pub types: Vec<Type>,
}

#[derive(Debug, PartialEq, Clone, Copy)]
#[allow(dead_code)]
pub enum TypeSpecifier {
    Array,
    Optional,
}

use crate::lexer::Span;

#[derive(Debug, PartialEq)]
#[allow(dead_code)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug, PartialEq)]
pub struct Statement {
    pub kind: StatementKind,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub enum StatementKind {
    Variable(VariableStatement),
    Function(FunctionDefinition),
    ValueType(ValueTypeDeclaration),
    ObjectType(ObjectTypeDeclaration),
    Expression(Expression),
    WhileLoop(Box<WhileLoop>),
    ForLoop(Box<ForLoop>),
}

#[derive(Debug, PartialEq)]
pub struct ObjectTypeDeclaration {
    pub name: String,
    pub fields: Vec<ValueField>,
    pub body: Block,
}

#[derive(Debug, PartialEq)]
pub struct ValueTypeDeclaration {
    pub name: String,
    pub fields: Vec<ValueField>,
    pub body: Block,
}

#[derive(Debug, PartialEq)]
pub enum Mutability {
    Val,
    Var,
}

#[derive(Debug, PartialEq)]
pub struct ValueField {
    pub mutability: Mutability,
    pub name: String,
    pub type_annotation: Type,
}

#[derive(Debug, PartialEq)]
pub struct FunctionDefinition {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub return_type: Option<Type>,
    pub body: Block,
}

#[derive(Debug, PartialEq)]
pub struct Parameter {
    pub name: String,
    pub type_annotation: Type,
}

#[derive(Debug, PartialEq)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub expression: Option<Box<Expression>>,
}

#[derive(Debug, PartialEq)]
pub struct VariableStatement {
    pub mutable: bool,
    pub name: String,
    pub type_annotation: Option<Type>,
    pub value: Expression,
}

#[derive(Debug, PartialEq)]
#[allow(dead_code)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
#[allow(dead_code)]
pub enum ExpressionKind {
    Literal(Literal),
    Binary(Box<Expression>, BinaryOperator, Box<Expression>),
    Unary(UnaryOperator, Box<Expression>),
    Assignment(Box<AssignmentExpression>),
    FunctionCall(Box<FunctionCall>),
    ArrayLiteral(Box<ArrayLiteral>),
    IfElse(Box<IfElse>),
    When(Box<WhenExpression>),
    Identifier(String),
    GroupedExpression(Box<Expression>),
    Tuple(Tuple),
}

#[derive(Debug, PartialEq)]
pub struct Tuple {
    pub elements: Vec<Expression>,
}

#[derive(Debug, PartialEq)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    GreaterThan,
    LessThan,
    GreaterThanOrEqual,
    LessThanOrEqual,
    Equality,
    Inequality,
    And,
    Or,
}

#[derive(Debug, PartialEq)]
pub enum UnaryOperator {
    Negate,
    Not,
}

#[derive(Debug, PartialEq)]
pub struct AssignmentExpression {
    pub name: String,
    pub value: Expression,
}

#[derive(Debug, PartialEq)]
pub struct FunctionCall {
    pub name: String,
    pub arguments: Vec<Expression>,
}

#[derive(Debug, PartialEq)]
pub struct ArrayLiteral {
    pub elements: Vec<Expression>,
}

#[derive(Debug, PartialEq)]
pub struct WhileLoop {
    pub condition: Expression,
    pub body: Block,
}

#[derive(Debug, PartialEq)]
pub struct ForLoop {
    pub iterator: String,
    pub iterable: Expression,
    pub body: Block,
}

#[derive(Debug, PartialEq)]
pub struct IfElse {
    pub condition: Expression,
    pub then_branch: Block,
    pub else_branch: Option<Block>,
}

#[derive(Debug, PartialEq)]
pub struct WhenExpression {
    pub expression: Expression,
    pub branches: Vec<WhenBranch>,
}

#[derive(Debug, PartialEq)]
pub struct WhenBranch {
    pub condition: Expression,
    pub result: Expression,
}

#[derive(Debug, PartialEq)]
#[allow(dead_code)]
pub enum Literal {
    Integer(u64),
    Float(f64),
    String(String),
    Boolean(bool),
    Null,
}