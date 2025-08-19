// src/parser_array_tests.rs

use crate::ast::{
    ArrayLiteral, BaseType, Expression, ExpressionKind, Literal, SimpleType, Statement,
    StatementKind, Type, VariableStatement,
};
use crate::lexer::{Lexer, Span};
use crate::parser::Parser;

#[test]
fn test_parse_array_type() {
    let input = "val x: int[] = []";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let expected = Statement {
        kind: StatementKind::Variable(VariableStatement {
            public: false,
            mutable: false,
            name: "x".to_string(),
            type_annotation: Some(Type::Simple(SimpleType {
                base: BaseType::Array(Box::new(Type::Simple(SimpleType {
                    base: BaseType::User("int".to_string()),
                }))),
            })),
            value: Expression {
                kind: ExpressionKind::ArrayLiteral(Box::new(ArrayLiteral::List(vec![]))),
                span: Span { start: 15, end: 17 },
            },
        }),
        span: Span { start: 0, end: 17 },
    };
    assert_eq!(parser.parse_statement(), Ok(expected));
}

#[test]
fn test_parse_sized_array_literal() {
    let input = "[1; 5]";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let expected = Statement {
        kind: StatementKind::Expression(Expression {
            kind: ExpressionKind::ArrayLiteral(Box::new(ArrayLiteral::Sized {
                value: Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Integer(1)),
                    span: Span { start: 1, end: 2 },
                }),
                size: Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Integer(5)),
                    span: Span { start: 4, end: 5 },
                }),
            })),
            span: Span { start: 0, end: 6 },
        }),
        span: Span { start: 0, end: 6 },
    };
    assert_eq!(parser.parse_statement(), Ok(expected));
}

#[test]
fn test_parse_multidimensional_array_type() {
    let input = "val x: int[][] = []";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let expected = Statement {
        kind: StatementKind::Variable(VariableStatement {
            public: false,
            mutable: false,
            name: "x".to_string(),
            type_annotation: Some(Type::Simple(SimpleType {
                base: BaseType::Array(Box::new(Type::Simple(SimpleType {
                    base: BaseType::Array(Box::new(Type::Simple(SimpleType {
                        base: BaseType::User("int".to_string()),
                    }))),
                }))),
            })),
            value: Expression {
                kind: ExpressionKind::ArrayLiteral(Box::new(ArrayLiteral::List(vec![]))),
                span: Span { start: 17, end: 19 },
            },
        }),
        span: Span { start: 0, end: 19 },
    };
    assert_eq!(parser.parse_statement(), Ok(expected));
}

#[test]
fn test_parse_array_literal() {
    let input = "[1, 2, 3]";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let expected = Statement {
        kind: StatementKind::Expression(Expression {
            kind: ExpressionKind::ArrayLiteral(Box::new(ArrayLiteral::List(vec![
                Expression {
                    kind: ExpressionKind::Literal(Literal::Integer(1)),
                    span: Span { start: 1, end: 2 },
                },
                Expression {
                    kind: ExpressionKind::Literal(Literal::Integer(2)),
                    span: Span { start: 4, end: 5 },
                },
                Expression {
                    kind: ExpressionKind::Literal(Literal::Integer(3)),
                    span: Span { start: 7, end: 8 },
                },
            ]))),
            span: Span { start: 0, end: 9 },
        }),
        span: Span { start: 0, end: 9 },
    };
    assert_eq!(parser.parse_statement(), Ok(expected));
}

#[test]
fn test_parse_index_expression() {
    let input = "my_array[0]";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let expected = Statement {
        kind: StatementKind::Expression(Expression {
            kind: ExpressionKind::Index(
                Box::new(Expression {
                    kind: ExpressionKind::Identifier("my_array".to_string()),
                    span: Span { start: 0, end: 8 },
                }),
                Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Integer(0)),
                    span: Span { start: 9, end: 10 },
                }),
            ),
            span: Span { start: 0, end: 11 },
        }),
        span: Span { start: 0, end: 11 },
    };
    assert_eq!(parser.parse_statement(), Ok(expected));
}
