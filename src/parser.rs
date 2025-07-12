use pest::iterators::Pair;
use crate::ast::*;
use crate::Rule;

pub fn parse_program(pair: Pair<Rule>) -> Program {
    let mut statements = Vec::new();
    for statement_pair in pair.into_inner() {
        if let Rule::statement = statement_pair.as_rule() {
            statements.push(parse_statement(statement_pair));
        }
    }
    Program { statements }
}

fn parse_statement(pair: Pair<Rule>) -> Statement {
    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::variable_statement => Statement::Variable(parse_variable_statement(inner)),
        Rule::function_definition => Statement::Function(parse_function_definition(inner)),
        _ => unreachable!(),
    }
}

fn parse_function_definition(pair: Pair<Rule>) -> FunctionDefinition {
    let mut inner = pair.into_inner();
    let name = inner.next().unwrap().as_str().to_string();
    let parameters = parse_parameter_list(inner.next().unwrap());
    let next = inner.next().unwrap();
    let (return_type, body) = if next.as_rule() == Rule::r#type {
        (Some(parse_type(next)), parse_block(inner.next().unwrap()))
    } else {
        (None, parse_block(next))
    };
    FunctionDefinition {
        name,
        parameters,
        return_type,
        body,
    }
}

fn parse_parameter_list(pair: Pair<Rule>) -> Vec<Parameter> {
    let mut parameters = Vec::new();
    for parameter_pair in pair.into_inner() {
        parameters.push(parse_parameter(parameter_pair));
    }
    parameters
}

fn parse_parameter(pair: Pair<Rule>) -> Parameter {
    let mut inner = pair.into_inner();
    let name = inner.next().unwrap().as_str().to_string();
    let type_annotation = parse_type(inner.next().unwrap());
    Parameter {
        name,
        type_annotation,
    }
}

fn parse_block(pair: Pair<Rule>) -> Block {
    let mut inner = pair.into_inner();
    let mut statements = Vec::new();
    let mut expression = None;
    while let Some(current) = inner.next() {
        match current.as_rule() {
            Rule::statement => {
                statements.push(parse_statement(current));
            }
            Rule::expression => {
                expression = Some(Box::new(parse_expression(current)));
                break;
            }
            _ => unreachable!(),
        }
    }
    Block {
        statements,
        expression,
    }
}

fn parse_variable_statement(pair: Pair<Rule>) -> VariableStatement {
    let mut inner = pair.into_inner();
    let kw = inner.next().unwrap();
    let mutable = kw.as_str() == "var";
    let name = inner.next().unwrap().as_str().to_string();
    let value = parse_expression(inner.next().unwrap());
    VariableStatement { mutable, name, value }
}

fn parse_expression(pair: Pair<Rule>) -> Expression {
    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::literal => Expression::Literal(parse_literal(inner)),
        _ => unreachable!(),
    }
}

fn parse_literal(pair: Pair<Rule>) -> Literal {
    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::integer_literal => Literal::Integer(inner.as_str().parse().unwrap()),
        Rule::float_literal => Literal::Float(inner.as_str().parse().unwrap()),
        Rule::string_literal => Literal::String(inner.as_str()[1..inner.as_str().len() - 1].to_string()),
        Rule::boolean_literal => Literal::Boolean(inner.as_str().parse().unwrap()),
        Rule::null_literal => Literal::Null,
        _ => unreachable!(),
    }
}

pub fn parse_type(pair: Pair<Rule>) -> Type {
    let mut simple_types = Vec::new();
    for current in pair.into_inner() {
        simple_types.push(parse_simple_type(current));
    }

    if simple_types.len() == 1 {
        Type::Simple(simple_types.remove(0))
    } else {
        Type::Union(simple_types)
    }
}

fn parse_simple_type(pair: Pair<Rule>) -> SimpleType {
    let mut inner = pair.into_inner();
    let base_type_pair = inner.next().unwrap();
    let base = parse_base_type(base_type_pair);

    let mut specifiers = Vec::new();
    for specifier_pair in inner {
        match specifier_pair.as_str() {
            "[]" => specifiers.push(TypeSpecifier::Array),
            "?" => specifiers.push(TypeSpecifier::Optional),
            _ => unreachable!(),
        }
    }

    SimpleType { base, specifiers }
}

fn parse_base_type(pair: Pair<Rule>) -> BaseType {
    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::primitive_type => BaseType::Primitive(parse_primitive_type(inner)),
        Rule::user_type => BaseType::User(inner.as_str().to_string()),
        _ => unreachable!(),
    }
}

fn parse_primitive_type(pair: Pair<Rule>) -> PrimitiveType {
    match pair.as_str() {
        "i8" => PrimitiveType::I8,
        "i16" => PrimitiveType::I16,
        "i32" => PrimitiveType::I32,
        "i64" => PrimitiveType::I64,
        "u8" => PrimitiveType::U8,
        "u16" => PrimitiveType::U16,
        "u32" => PrimitiveType::U32,
        "u64" => PrimitiveType::U64,
        "f32" => PrimitiveType::F32,
        "f64" => PrimitiveType::F64,
        "Boolean" => PrimitiveType::Boolean,
        "Rune" => PrimitiveType::Rune,
        "String" => PrimitiveType::String,
        _ => unreachable!(),
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{Block, Expression, FunctionDefinition, Literal, Parameter, Program, Statement, Type, SimpleType, BaseType, PrimitiveType};
    use crate::parser::parse_program;
    use crate::{Rule, DyegoParser};
    use pest::Parser;

    fn parse(input: &str) -> Program {
        let mut pairs = DyegoParser::parse(Rule::program, input).unwrap();
        let pair = pairs.next().unwrap();
        parse_program(pair)
    }

    #[test]
    fn test_function_definition() {
        let input = "fun my_func(a: i32): i32 { 1 }";
        let expected = Program {
            statements: vec![Statement::Function(FunctionDefinition {
                name: "my_func".to_string(),
                parameters: vec![Parameter {
                    name: "a".to_string(),
                    type_annotation: Type::Simple(SimpleType {
                        base: BaseType::Primitive(PrimitiveType::I32),
                        specifiers: vec![],
                    }),
                }],
                return_type: Some(Type::Simple(SimpleType {
                    base: BaseType::Primitive(PrimitiveType::I32),
                    specifiers: vec![],
                })),
                body: Block {
                    statements: vec![],
                    expression: Some(Box::new(Expression::Literal(Literal::Integer(1)))),
                },
            })],
        };
        assert_eq!(parse(input), expected);
    }
}
