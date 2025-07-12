#[derive(Debug, PartialEq)]
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
pub enum BaseType {
    Primitive(PrimitiveType),
    User(String),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TypeSpecifier {
    Array,
    Optional,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum PrimitiveType {
    I8, I16, I32, I64,
    U8, U16, U32, U64,
    F32, F64,
    Boolean,
    Rune,
    String,
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Variable(VariableStatement),
    Function(FunctionDefinition),
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
    pub value: Expression,
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Literal(Literal),
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    Null,
}