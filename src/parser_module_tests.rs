use crate::ast::{Block, Expression, ExpressionKind, Literal, Module, Statement, StatementKind};
use crate::lexer::{Lexer, Span};
use crate::parser::Parser;

#[test]
fn test_parse_module_statement() {
    let input = "mod my_module { 1 }";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let expected = Statement {
        kind: StatementKind::Module(Module {
            name: "my_module".to_string(),
            body: Block {
                statements: vec![],
                expression: Some(Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Integer(1)),
                    span: Span { start: 16, end: 17 },
                })),
            },
        }),
        span: Span { start: 0, end: 19 },
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
