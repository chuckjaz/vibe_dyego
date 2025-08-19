use crate::ast::{
    BaseType, BinaryOperator, Block, Expression, ExpressionKind, ForLoop, FunctionDefinition,
    FunctionSignature, IfElse, Literal, Mutability, ObjectType, ObjectTypeDeclaration, Parameter,
    SimpleType, Statement, StatementKind, Tuple, TupleType, Type, UnaryOperator, ValueField,
    ValueType, ValueTypeDeclaration, VariableStatement, WhenBranch, WhenExpression, WhileLoop,
};
use crate::lexer::{Lexer, Span};
use crate::parser::Parser;

#[test]
fn test_parse_for_statement() {
    let input = "for i in 1 { 1 }";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let expected = Statement {
        kind: StatementKind::ForLoop(Box::new(ForLoop {
            iterator: "i".to_string(),
            iterable: Expression {
                kind: ExpressionKind::Literal(Literal::Integer(1)),
                span: Span { start: 9, end: 10 },
            },
            body: Block {
                statements: vec![],
                expression: Some(Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Integer(1)),
                    span: Span { start: 13, end: 14 },
                })),
            },
        })),
        span: Span { start: 0, end: 16 },
    };
    assert_eq!(parser.parse_statement(), Ok(expected));
}

#[test]
fn test_parse_variable_with_object_type() {
    let input = "val x: object(val a: i32) { fun b(): i32 } = 1";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let expected = Statement {
        kind: StatementKind::Variable(VariableStatement {
            public: false,
            mutable: false,
            name: "x".to_string(),
            type_annotation: Some(Type::Simple(SimpleType {
                base: BaseType::Object(ObjectType {
                    fields: vec![ValueField {
                        mutability: Mutability::Val,
                        name: "a".to_string(),
                        type_annotation: Type::Simple(SimpleType {
                            base: BaseType::User("i32".to_string()),
                        }),
                    }],
                    functions: vec![FunctionSignature {
                        name: "b".to_string(),
                        parameters: vec![],
                        return_type: Type::Simple(SimpleType {
                            base: BaseType::User("i32".to_string()),
                        }),
                    }],
                }),
            })),
            value: Expression {
                kind: ExpressionKind::Literal(Literal::Integer(1)),
                span: Span { start: 45, end: 46 },
            },
        }),
        span: Span { start: 0, end: 46 },
    };
    assert_eq!(parser.parse_statement(), Ok(expected));
}

#[test]
fn test_parse_variable_with_value_type() {
    let input = "val x: value(val a: i32) { fun b(): i32 } = 1";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let expected = Statement {
        kind: StatementKind::Variable(VariableStatement {
            public: false,
            mutable: false,
            name: "x".to_string(),
            type_annotation: Some(Type::Simple(SimpleType {
                base: BaseType::Value(ValueType {
                    fields: vec![ValueField {
                        mutability: Mutability::Val,
                        name: "a".to_string(),
                        type_annotation: Type::Simple(SimpleType {
                            base: BaseType::User("i32".to_string()),
                        }),
                    }],
                    functions: vec![FunctionSignature {
                        name: "b".to_string(),
                        parameters: vec![],
                        return_type: Type::Simple(SimpleType {
                            base: BaseType::User("i32".to_string()),
                        }),
                    }],
                }),
            })),
            value: Expression {
                kind: ExpressionKind::Literal(Literal::Integer(1)),
                span: Span { start: 44, end: 45 },
            },
        }),
        span: Span { start: 0, end: 45 },
    };
    assert_eq!(parser.parse_statement(), Ok(expected));
}

#[test]
fn test_parse_object_type_no_fields() {
    let input = "val x: object { fun b(): i32 } = 1";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let expected = Statement {
        kind: StatementKind::Variable(VariableStatement {
            public: false,
            mutable: false,
            name: "x".to_string(),
            type_annotation: Some(Type::Simple(SimpleType {
                base: BaseType::Object(ObjectType {
                    fields: vec![],
                    functions: vec![FunctionSignature {
                        name: "b".to_string(),
                        parameters: vec![],
                        return_type: Type::Simple(SimpleType {
                            base: BaseType::User("i32".to_string()),
                        }),
                    }],
                }),
            })),
            value: Expression {
                kind: ExpressionKind::Literal(Literal::Integer(1)),
                span: Span { start: 33, end: 34 },
            },
        }),
        span: Span { start: 0, end: 34 },
    };
    assert_eq!(parser.parse_statement(), Ok(expected));
}

#[test]
fn test_parse_value_type_no_fields() {
    let input = "val x: value { fun b(): i32 } = 1";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let expected = Statement {
        kind: StatementKind::Variable(VariableStatement {
            public: false,
            mutable: false,
            name: "x".to_string(),
            type_annotation: Some(Type::Simple(SimpleType {
                base: BaseType::Value(ValueType {
                    fields: vec![],
                    functions: vec![FunctionSignature {
                        name: "b".to_string(),
                        parameters: vec![],
                        return_type: Type::Simple(SimpleType {
                            base: BaseType::User("i32".to_string()),
                        }),
                    }],
                }),
            })),
            value: Expression {
                kind: ExpressionKind::Literal(Literal::Integer(1)),
                span: Span { start: 32, end: 33 },
            },
        }),
        span: Span { start: 0, end: 33 },
    };
    assert_eq!(parser.parse_statement(), Ok(expected));
}

#[test]
fn test_parse_object_type_empty_fields() {
    let input = "val x: object() { fun b(): i32 } = 1";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let expected = Statement {
        kind: StatementKind::Variable(VariableStatement {
            public: false,
            mutable: false,
            name: "x".to_string(),
            type_annotation: Some(Type::Simple(SimpleType {
                base: BaseType::Object(ObjectType {
                    fields: vec![],
                    functions: vec![FunctionSignature {
                        name: "b".to_string(),
                        parameters: vec![],
                        return_type: Type::Simple(SimpleType {
                            base: BaseType::User("i32".to_string()),
                        }),
                    }],
                }),
            })),
            value: Expression {
                kind: ExpressionKind::Literal(Literal::Integer(1)),
                span: Span { start: 35, end: 36 },
            },
        }),
        span: Span { start: 0, end: 36 },
    };
    assert_eq!(parser.parse_statement(), Ok(expected));
}

#[test]
fn test_parse_value_type_empty_fields() {
    let input = "val x: value() { fun b(): i32 } = 1";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let expected = Statement {
        kind: StatementKind::Variable(VariableStatement {
            public: false,
            mutable: false,
            name: "x".to_string(),
            type_annotation: Some(Type::Simple(SimpleType {
                base: BaseType::Value(ValueType {
                    fields: vec![],
                    functions: vec![FunctionSignature {
                        name: "b".to_string(),
                        parameters: vec![],
                        return_type: Type::Simple(SimpleType {
                            base: BaseType::User("i32".to_string()),
                        }),
                    }],
                }),
            })),
            value: Expression {
                kind: ExpressionKind::Literal(Literal::Integer(1)),
                span: Span { start: 34, end: 35 },
            },
        }),
        span: Span { start: 0, end: 35 },
    };
    assert_eq!(parser.parse_statement(), Ok(expected));
}

#[test]
fn test_parse_object_type_declaration_with_fields() {
    let input = "object Point(val x: i32, var y: i32) { 1 }";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let expected = Statement {
        kind: StatementKind::ObjectType(ObjectTypeDeclaration {
            public: false,
            name: "Point".to_string(),
            fields: vec![
                ValueField {
                    mutability: Mutability::Val,
                    name: "x".to_string(),
                    type_annotation: Type::Simple(SimpleType {
                        base: BaseType::User("i32".to_string()),
                    }),
                },
                ValueField {
                    mutability: Mutability::Var,
                    name: "y".to_string(),
                    type_annotation: Type::Simple(SimpleType {
                        base: BaseType::User("i32".to_string()),
                    }),
                },
            ],
            body: Block {
                statements: vec![],
                expression: Some(Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Integer(1)),
                    span: Span { start: 39, end: 40 },
                })),
            },
        }),
        span: Span { start: 0, end: 42 },
    };
    assert_eq!(parser.parse_statement(), Ok(expected));
}

#[test]
fn test_parse_while_statement() {
    let input = "while 1 { 1 }";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let expected = Statement {
        kind: StatementKind::WhileLoop(Box::new(WhileLoop {
            condition: Expression {
                kind: ExpressionKind::Literal(Literal::Integer(1)),
                span: Span { start: 6, end: 7 },
            },
            body: Block {
                statements: vec![],
                expression: Some(Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Integer(1)),
                    span: Span { start: 10, end: 11 },
                })),
            },
        })),
        span: Span { start: 0, end: 13 },
    };
    assert_eq!(parser.parse_statement(), Ok(expected));
}

#[test]
fn test_parse_error() {
    let input = "val x = ";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let error = parser.parse_statement().unwrap_err();
    assert_eq!(error.message, "Unexpected token: Eof");
}

#[test]
fn test_parse_tuple_expression() {
    let input = "(1, 2.0, \"three\")";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let expected = Statement {
        kind: StatementKind::Expression(Expression {
            kind: ExpressionKind::Tuple(Tuple {
                elements: vec![
                    Expression {
                        kind: ExpressionKind::Literal(Literal::Integer(1)),
                        span: Span { start: 1, end: 2 },
                    },
                    Expression {
                        kind: ExpressionKind::Literal(Literal::Float(2.0)),
                        span: Span { start: 4, end: 7 },
                    },
                    Expression {
                        kind: ExpressionKind::Literal(Literal::String("three".to_string())),
                        span: Span { start: 9, end: 16 },
                    },
                ],
            }),
            span: Span { start: 0, end: 17 },
        }),
        span: Span { start: 0, end: 17 },
    };
    assert_eq!(parser.parse_statement(), Ok(expected));
}

#[test]
fn test_parse_empty_tuple_expression() {
    let input = "()";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let expected = Statement {
        kind: StatementKind::Expression(Expression {
            kind: ExpressionKind::Tuple(Tuple { elements: vec![] }),
            span: Span { start: 0, end: 2 },
        }),
        span: Span { start: 0, end: 2 },
    };
    assert_eq!(parser.parse_statement(), Ok(expected));
}

#[test]
fn test_parse_single_element_tuple_expression() {
    let input = "(1,)";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let expected = Statement {
        kind: StatementKind::Expression(Expression {
            kind: ExpressionKind::Tuple(Tuple {
                elements: vec![Expression {
                    kind: ExpressionKind::Literal(Literal::Integer(1)),
                    span: Span { start: 1, end: 2 },
                }],
            }),
            span: Span { start: 0, end: 4 },
        }),
        span: Span { start: 0, end: 4 },
    };
    assert_eq!(parser.parse_statement(), Ok(expected));
}

#[test]
fn test_parse_variable_with_tuple_type() {
    let input = "val x: (i32, f64) = (1, 2.0)";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let expected = Statement {
        kind: StatementKind::Variable(VariableStatement {
            public: false,
            mutable: false,
            name: "x".to_string(),
            type_annotation: Some(Type::Simple(SimpleType {
                base: BaseType::Tuple(TupleType {
                    types: vec![
                        Type::Simple(SimpleType {
                            base: BaseType::User("i32".to_string()),
                        }),
                        Type::Simple(SimpleType {
                            base: BaseType::User("f64".to_string()),
                        }),
                    ],
                }),
            })),
            value: Expression {
                kind: ExpressionKind::Tuple(Tuple {
                    elements: vec![
                        Expression {
                            kind: ExpressionKind::Literal(Literal::Integer(1)),
                            span: Span { start: 21, end: 22 },
                        },
                        Expression {
                            kind: ExpressionKind::Literal(Literal::Float(2.0)),
                            span: Span { start: 24, end: 27 },
                        },
                    ],
                }),
                span: Span { start: 20, end: 28 },
            },
        }),
        span: Span { start: 0, end: 28 },
    };
    assert_eq!(parser.parse_statement(), Ok(expected));
}

#[test]
fn test_parse_function_with_tuple_type_parameter() {
    let input = "fun a(x: (i32, f64)) {}";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let expected = Statement {
        kind: StatementKind::Function(FunctionDefinition {
            public: false,
            name: "a".to_string(),
            parameters: vec![Parameter {
                name: "x".to_string(),
                type_annotation: Type::Simple(SimpleType {
                    base: BaseType::Tuple(TupleType {
                        types: vec![
                            Type::Simple(SimpleType {
                                base: BaseType::User("i32".to_string()),
                            }),
                            Type::Simple(SimpleType {
                                base: BaseType::User("f64".to_string()),
                            }),
                        ],
                    }),
                }),
            }],
            return_type: None,
            body: Block {
                statements: vec![],
                expression: None,
            },
        }),
        span: Span { start: 0, end: 23 },
    };
    assert_eq!(parser.parse_statement(), Ok(expected));
}

#[test]
fn test_parse_float() {
    let input = "123.45";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let expected = Statement {
        kind: StatementKind::Expression(Expression {
            kind: ExpressionKind::Literal(Literal::Float(123.45)),
            span: Span { start: 0, end: 6 },
        }),
        span: Span { start: 0, end: 6 },
    };
    assert_eq!(parser.parse_statement(), Ok(expected));
}

#[test]
fn test_parse_integer() {
    let input = "123";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let expected = Statement {
        kind: StatementKind::Expression(Expression {
            kind: ExpressionKind::Literal(Literal::Integer(123)),
            span: Span { start: 0, end: 3 },
        }),
        span: Span { start: 0, end: 3 },
    };
    assert_eq!(parser.parse_statement(), Ok(expected));
}

#[test]
fn test_parse_addition() {
    let input = "1 + 2";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let expected = Statement {
        kind: StatementKind::Expression(Expression {
            kind: ExpressionKind::Binary(
                Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Integer(1)),
                    span: Span { start: 0, end: 1 },
                }),
                BinaryOperator::Add,
                Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Integer(2)),
                    span: Span { start: 4, end: 5 },
                }),
            ),
            span: Span { start: 0, end: 5 },
        }),
        span: Span { start: 0, end: 5 },
    };
    assert_eq!(parser.parse_statement(), Ok(expected));
}

#[test]
fn test_parse_precedence() {
    let input = "1 + 2 * 3";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let expected = Statement {
        kind: StatementKind::Expression(Expression {
            kind: ExpressionKind::Binary(
                Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Integer(1)),
                    span: Span { start: 0, end: 1 },
                }),
                BinaryOperator::Add,
                Box::new(Expression {
                    kind: ExpressionKind::Binary(
                        Box::new(Expression {
                            kind: ExpressionKind::Literal(Literal::Integer(2)),
                            span: Span { start: 4, end: 5 },
                        }),
                        BinaryOperator::Multiply,
                        Box::new(Expression {
                            kind: ExpressionKind::Literal(Literal::Integer(3)),
                            span: Span { start: 8, end: 9 },
                        }),
                    ),
                    span: Span { start: 4, end: 9 },
                }),
            ),
            span: Span { start: 0, end: 9 },
        }),
        span: Span { start: 0, end: 9 },
    };
    assert_eq!(parser.parse_statement(), Ok(expected));
}

#[test]
fn test_parse_parentheses() {
    let input = "(1 + 2) * 3";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let expected = Statement {
        kind: StatementKind::Expression(Expression {
            kind: ExpressionKind::Binary(
                Box::new(Expression {
                    kind: ExpressionKind::GroupedExpression(Box::new(Expression {
                        kind: ExpressionKind::Binary(
                            Box::new(Expression {
                                kind: ExpressionKind::Literal(Literal::Integer(1)),
                                span: Span { start: 1, end: 2 },
                            }),
                            BinaryOperator::Add,
                            Box::new(Expression {
                                kind: ExpressionKind::Literal(Literal::Integer(2)),
                                span: Span { start: 5, end: 6 },
                            }),
                        ),
                        span: Span { start: 1, end: 6 },
                    })),
                    span: Span { start: 0, end: 7 },
                }),
                BinaryOperator::Multiply,
                Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Integer(3)),
                    span: Span { start: 10, end: 11 },
                }),
            ),
            span: Span { start: 0, end: 11 },
        }),
        span: Span { start: 0, end: 11 },
    };
    assert_eq!(parser.parse_statement(), Ok(expected));
}

#[test]
fn test_parse_if_expression() {
    let input = "if 1 { 2 } else { 3 }";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let expected = Statement {
        kind: StatementKind::Expression(Expression {
            kind: ExpressionKind::IfElse(Box::new(IfElse {
                condition: Expression {
                    kind: ExpressionKind::Literal(Literal::Integer(1)),
                    span: Span { start: 3, end: 4 },
                },
                then_branch: Block {
                    statements: vec![],
                    expression: Some(Box::new(Expression {
                        kind: ExpressionKind::Literal(Literal::Integer(2)),
                        span: Span { start: 7, end: 8 },
                    })),
                },
                else_branch: Some(Block {
                    statements: vec![],
                    expression: Some(Box::new(Expression {
                        kind: ExpressionKind::Literal(Literal::Integer(3)),
                        span: Span { start: 18, end: 19 },
                    })),
                }),
            })),
            span: Span { start: 0, end: 21 },
        }),
        span: Span { start: 0, end: 21 },
    };
    assert_eq!(parser.parse_statement(), Ok(expected));
}

#[test]
fn test_parse_unary_negation() {
    let input = "-10";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let expected = Statement {
        kind: StatementKind::Expression(Expression {
            kind: ExpressionKind::Unary(
                UnaryOperator::Negate,
                Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Integer(10)),
                    span: Span { start: 1, end: 3 },
                }),
            ),
            span: Span { start: 0, end: 3 },
        }),
        span: Span { start: 0, end: 3 },
    };
    assert_eq!(parser.parse_statement(), Ok(expected));
}

#[test]
fn test_parse_binary_precedence() {
    let input = "1 + 2 * 3 == 4 / 5 - 6 && 7 > 8 || 9 < 10";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let expected = Statement {
        kind: StatementKind::Expression(Expression {
            kind: ExpressionKind::Binary(
                Box::new(Expression {
                    kind: ExpressionKind::Binary(
                        Box::new(Expression {
                            kind: ExpressionKind::Binary(
                                Box::new(Expression {
                                    kind: ExpressionKind::Binary(
                                        Box::new(Expression {
                                            kind: ExpressionKind::Literal(Literal::Integer(1)),
                                            span: Span { start: 0, end: 1 },
                                        }),
                                        BinaryOperator::Add,
                                        Box::new(Expression {
                                            kind: ExpressionKind::Binary(
                                                Box::new(Expression {
                                                    kind: ExpressionKind::Literal(
                                                        Literal::Integer(2),
                                                    ),
                                                    span: Span { start: 4, end: 5 },
                                                }),
                                                BinaryOperator::Multiply,
                                                Box::new(Expression {
                                                    kind: ExpressionKind::Literal(
                                                        Literal::Integer(3),
                                                    ),
                                                    span: Span { start: 8, end: 9 },
                                                }),
                                            ),
                                            span: Span { start: 4, end: 9 },
                                        }),
                                    ),
                                    span: Span { start: 0, end: 9 },
                                }),
                                BinaryOperator::Equality,
                                Box::new(Expression {
                                    kind: ExpressionKind::Binary(
                                        Box::new(Expression {
                                            kind: ExpressionKind::Binary(
                                                Box::new(Expression {
                                                    kind: ExpressionKind::Literal(
                                                        Literal::Integer(4),
                                                    ),
                                                    span: Span { start: 13, end: 14 },
                                                }),
                                                BinaryOperator::Divide,
                                                Box::new(Expression {
                                                    kind: ExpressionKind::Literal(
                                                        Literal::Integer(5),
                                                    ),
                                                    span: Span { start: 17, end: 18 },
                                                }),
                                            ),
                                            span: Span { start: 13, end: 18 },
                                        }),
                                        BinaryOperator::Subtract,
                                        Box::new(Expression {
                                            kind: ExpressionKind::Literal(Literal::Integer(6)),
                                            span: Span { start: 21, end: 22 },
                                        }),
                                    ),
                                    span: Span { start: 13, end: 22 },
                                }),
                            ),
                            span: Span { start: 0, end: 22 },
                        }),
                        BinaryOperator::And,
                        Box::new(Expression {
                            kind: ExpressionKind::Binary(
                                Box::new(Expression {
                                    kind: ExpressionKind::Literal(Literal::Integer(7)),
                                    span: Span { start: 26, end: 27 },
                                }),
                                BinaryOperator::GreaterThan,
                                Box::new(Expression {
                                    kind: ExpressionKind::Literal(Literal::Integer(8)),
                                    span: Span { start: 30, end: 31 },
                                }),
                            ),
                            span: Span { start: 26, end: 31 },
                        }),
                    ),
                    span: Span { start: 0, end: 31 },
                }),
                BinaryOperator::Or,
                Box::new(Expression {
                    kind: ExpressionKind::Binary(
                        Box::new(Expression {
                            kind: ExpressionKind::Literal(Literal::Integer(9)),
                            span: Span { start: 35, end: 36 },
                        }),
                        BinaryOperator::LessThan,
                        Box::new(Expression {
                            kind: ExpressionKind::Literal(Literal::Integer(10)),
                            span: Span { start: 39, end: 41 },
                        }),
                    ),
                    span: Span { start: 35, end: 41 },
                }),
            ),
            span: Span { start: 0, end: 41 },
        }),
        span: Span { start: 0, end: 41 },
    };
    assert_eq!(parser.parse_statement(), Ok(expected));
}

#[test]
fn test_parse_variable_statement() {
    let input = "val x = 10";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let expected = Statement {
        kind: StatementKind::Variable(VariableStatement {
            public: false,
            mutable: false,
            name: "x".to_string(),
            type_annotation: None,
            value: Expression {
                kind: ExpressionKind::Literal(Literal::Integer(10)),
                span: Span { start: 8, end: 10 },
            },
        }),
        span: Span { start: 0, end: 10 },
    };
    assert_eq!(parser.parse_statement(), Ok(expected));
}

#[test]
fn test_parse_function_declaration() {
    let input = "fun my_func(a: i32, b: f64): bool { 1 }";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let expected = Statement {
        kind: StatementKind::Function(FunctionDefinition {
            public: false,
            name: "my_func".to_string(),
            parameters: vec![
                Parameter {
                    name: "a".to_string(),
                    type_annotation: Type::Simple(SimpleType {
                        base: BaseType::User("i32".to_string()),
                    }),
                },
                Parameter {
                    name: "b".to_string(),
                    type_annotation: Type::Simple(SimpleType {
                        base: BaseType::User("f64".to_string()),
                    }),
                },
            ],
            return_type: Some(Type::Simple(SimpleType {
                base: BaseType::User("bool".to_string()),
            })),
            body: Block {
                statements: vec![],
                expression: Some(Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Integer(1)),
                    span: Span { start: 36, end: 37 },
                })),
            },
        }),
        span: Span { start: 0, end: 39 },
    };
    assert_eq!(parser.parse_statement(), Ok(expected));
}

#[test]
fn test_parse_value_type_declaration_with_fields() {
    let input = "value Point(val x: i32, var y: i32) { 1 }";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let expected = Statement {
        kind: StatementKind::ValueType(ValueTypeDeclaration {
            public: false,
            name: "Point".to_string(),
            fields: vec![
                ValueField {
                    mutability: Mutability::Val,
                    name: "x".to_string(),
                    type_annotation: Type::Simple(SimpleType {
                        base: BaseType::User("i32".to_string()),
                    }),
                },
                ValueField {
                    mutability: Mutability::Var,
                    name: "y".to_string(),
                    type_annotation: Type::Simple(SimpleType {
                        base: BaseType::User("i32".to_string()),
                    }),
                },
            ],
            body: Block {
                statements: vec![],
                expression: Some(Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Integer(1)),
                    span: Span { start: 38, end: 39 },
                })),
            },
        }),
        span: Span { start: 0, end: 41 },
    };
    assert_eq!(parser.parse_statement(), Ok(expected));
}

#[test]
fn test_parse_value_type_declaration_no_fields() {
    let input = "value Point { 1 }";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let expected = Statement {
        kind: StatementKind::ValueType(ValueTypeDeclaration {
            public: false,
            name: "Point".to_string(),
            fields: vec![],
            body: Block {
                statements: vec![],
                expression: Some(Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Integer(1)),
                    span: Span { start: 14, end: 15 },
                })),
            },
        }),
        span: Span { start: 0, end: 17 },
    };
    assert_eq!(parser.parse_statement(), Ok(expected));
}

#[test]
fn test_parse_value_type_declaration_empty_fields() {
    let input = "value Point() { 1 }";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let expected = Statement {
        kind: StatementKind::ValueType(ValueTypeDeclaration {
            public: false,
            name: "Point".to_string(),
            fields: vec![],
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
fn test_parse_when_expression() {
    let input = "when x { 1 => 2, 3 => 4 }";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let expected = Statement {
        kind: StatementKind::Expression(Expression {
            kind: ExpressionKind::When(Box::new(WhenExpression {
                expression: Expression {
                    kind: ExpressionKind::Identifier("x".to_string()),
                    span: Span { start: 5, end: 6 },
                },
                branches: vec![
                    WhenBranch {
                        condition: Expression {
                            kind: ExpressionKind::Literal(Literal::Integer(1)),
                            span: Span { start: 9, end: 10 },
                        },
                        result: Expression {
                            kind: ExpressionKind::Literal(Literal::Integer(2)),
                            span: Span { start: 14, end: 15 },
                        },
                    },
                    WhenBranch {
                        condition: Expression {
                            kind: ExpressionKind::Literal(Literal::Integer(3)),
                            span: Span { start: 17, end: 18 },
                        },
                        result: Expression {
                            kind: ExpressionKind::Literal(Literal::Integer(4)),
                            span: Span { start: 22, end: 23 },
                        },
                    },
                ],
            })),
            span: Span { start: 0, end: 25 },
        }),
        span: Span { start: 0, end: 25 },
    };
    assert_eq!(parser.parse_statement(), Ok(expected));
}
