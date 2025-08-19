use crate::{
    ast::{BaseType, ExpressionKind, SimpleType, StatementKind, Type},
    lexer::Lexer,
    parser::Parser,
};

#[test]
fn test_lambda_expression() {
    let input = "{ x: int, y: int -> x + y } -> int";
    let l = Lexer::new(input);
    let mut p = Parser::new(l);
    let expr = p.parse_expression().unwrap();

    let lambda = match expr.kind {
        ExpressionKind::Lambda(lambda) => lambda,
        _ => panic!("Expected lambda expression"),
    };

    assert_eq!(lambda.parameters.len(), 2);
    assert_eq!(lambda.parameters[0].name, "x");
    assert_eq!(
        lambda.parameters[0].type_annotation,
        Type::Simple(SimpleType {
            base: BaseType::User("int".to_string())
        })
    );
    assert_eq!(lambda.parameters[1].name, "y");
    assert_eq!(
        lambda.parameters[1].type_annotation,
        Type::Simple(SimpleType {
            base: BaseType::User("int".to_string())
        })
    );

    assert_eq!(
        lambda.return_type,
        Some(Type::Simple(SimpleType {
            base: BaseType::User("int".to_string())
        }))
    );

    let body = lambda.body;
    assert_eq!(body.statements.len(), 0);
    assert!(body.expression.is_some());
}

#[test]
fn test_function_type() {
    let input = "fun(int, int) -> int";
    let l = Lexer::new(input);
    let mut p = Parser::new(l);
    let type_ = p.parse_type().unwrap();

    let function_type = match type_ {
        Type::Simple(SimpleType {
            base: BaseType::Function(ft),
        }) => ft,
        _ => panic!("Expected function type"),
    };

    assert_eq!(function_type.parameters.len(), 2);
    assert_eq!(
        function_type.parameters[0],
        Type::Simple(SimpleType {
            base: BaseType::User("int".to_string())
        })
    );
    assert_eq!(
        function_type.parameters[1],
        Type::Simple(SimpleType {
            base: BaseType::User("int".to_string())
        })
    );

    assert_eq!(
        *function_type.return_type,
        Type::Simple(SimpleType {
            base: BaseType::User("int".to_string())
        })
    );
}

#[test]
fn test_variable_declaration_with_lambda() {
    let input = "val add = { x: int, y: int -> x + y } -> int";
    let l = Lexer::new(input);
    let mut p = Parser::new(l);
    let stmt = p.parse_statement().unwrap();

    let var_stmt = match stmt.kind {
        StatementKind::Variable(vs) => vs,
        _ => panic!("Expected variable statement"),
    };

    assert!(!var_stmt.mutable);
    assert_eq!(var_stmt.name, "add");
    assert!(var_stmt.type_annotation.is_none());

    match var_stmt.value.kind {
        ExpressionKind::Lambda(_) => (),
        _ => panic!("Expected lambda expression"),
    };
}

#[test]
fn test_variable_declaration_with_function_type() {
    let input = "val add: fun(int, int) -> int = { x: int, y: int -> x + y } -> int";
    let l = Lexer::new(input);
    let mut p = Parser::new(l);
    let stmt = p.parse_statement().unwrap();

    let var_stmt = match stmt.kind {
        StatementKind::Variable(vs) => vs,
        _ => panic!("Expected variable statement"),
    };

    assert!(!var_stmt.mutable);
    assert_eq!(var_stmt.name, "add");
    assert!(var_stmt.type_annotation.is_some());

    let type_annotation = var_stmt.type_annotation.unwrap();
    let function_type = match type_annotation {
        Type::Simple(SimpleType {
            base: BaseType::Function(ft),
        }) => ft,
        _ => panic!("Expected function type"),
    };

    assert_eq!(function_type.parameters.len(), 2);
}
