use crate::ast::{Block, Expression, ExpressionKind, Literal, Module, Statement, StatementKind};
use crate::lexer::{Lexer, Span};
use crate::parser::Parser;

use crate::ast::{FunctionDefinition, VariableStatement};

#[test]
fn test_parse_module_statement() {
    let input = "module my_module { 1 }";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let expected = Statement {
        kind: StatementKind::Module(Module {
            name: "my_module".to_string(),
            body: Block {
                statements: vec![],
                expression: Some(Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Integer(1)),
                    span: Span { start: 19, end: 20 },
                })),
            },
            published_names: vec![],
        }),
        span: Span { start: 0, end: 22 },
    };
    assert_eq!(parser.parse_statement(), Ok(expected));
}

#[test]
fn test_parse_module_with_publish_statement() {
    let input = "module my_module publish (a, b, c) { 1 }";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let expected = Statement {
        kind: StatementKind::Module(Module {
            name: "my_module".to_string(),
            body: Block {
                statements: vec![],
                expression: Some(Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Integer(1)),
                    span: Span { start: 37, end: 38 },
                })),
            },
            published_names: vec!["a".to_string(), "b".to_string(), "c".to_string()],
        }),
        span: Span { start: 0, end: 40 },
    };
    assert_eq!(parser.parse_statement(), Ok(expected));
}

#[test]
fn test_parse_public_function_statement() {
    let input = "public fun my_fun() {}";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let expected = Statement {
        kind: StatementKind::Function(FunctionDefinition {
            public: true,
            name: "my_fun".to_string(),
            parameters: vec![],
            return_type: None,
            body: Block {
                statements: vec![],
                expression: None,
            },
        }),
        span: Span { start: 7, end: 22 },
    };
    assert_eq!(parser.parse_statement(), Ok(expected));
}

#[test]
fn test_parse_public_variable_statement() {
    let input = "public val a = 1";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let expected = Statement {
        kind: StatementKind::Variable(VariableStatement {
            public: true,
            mutable: false,
            name: "a".to_string(),
            type_annotation: None,
            value: Expression {
                kind: ExpressionKind::Literal(Literal::Integer(1)),
                span: Span { start: 15, end: 16 },
            },
        }),
        span: Span { start: 7, end: 16 },
    };
    assert_eq!(parser.parse_statement(), Ok(expected));
}

#[test]
fn test_parse_member_access_expression() {
    let input = "my_module.my_member";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let expected = Statement {
        kind: StatementKind::Expression(Expression {
            kind: ExpressionKind::MemberAccess(
                Box::new(Expression {
                    kind: ExpressionKind::Identifier("my_module".to_string()),
                    span: Span { start: 0, end: 9 },
                }),
                "my_member".to_string(),
            ),
            span: Span { start: 0, end: 19 },
        }),
        span: Span { start: 0, end: 19 },
    };
    assert_eq!(parser.parse_statement(), Ok(expected));
}

#[test]
fn test_parse_nested_member_access_expression() {
    let input = "a.b.c";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let expected = Statement {
        kind: StatementKind::Expression(Expression {
            kind: ExpressionKind::MemberAccess(
                Box::new(Expression {
                    kind: ExpressionKind::MemberAccess(
                        Box::new(Expression {
                            kind: ExpressionKind::Identifier("a".to_string()),
                            span: Span { start: 0, end: 1 },
                        }),
                        "b".to_string(),
                    ),
                    span: Span { start: 0, end: 3 },
                }),
                "c".to_string(),
            ),
            span: Span { start: 0, end: 5 },
        }),
        span: Span { start: 0, end: 5 },
    };
    assert_eq!(parser.parse_statement(), Ok(expected));
}
