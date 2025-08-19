use super::{Lexer, TokenKind};

#[test]
fn test_next_token() {
    let input = "=+(){},[]";
    let mut lexer = Lexer::new(input);

    let tokens = vec![
        TokenKind::Equal,
        TokenKind::Plus,
        TokenKind::LParen,
        TokenKind::RParen,
        TokenKind::LBrace,
        TokenKind::RBrace,
        TokenKind::Comma,
        TokenKind::LBracket,
        TokenKind::RBracket,
        TokenKind::Eof,
    ];

    for token_kind in tokens {
        let token = lexer.next_token();
        assert_eq!(token.kind, token_kind);
    }
}

#[test]
fn test_string_literal() {
    let input = "\"hello world\"";
    let mut lexer = Lexer::new(input);
    let token = lexer.next_token();
    assert_eq!(token.kind, TokenKind::String("hello world".to_string()));
}

#[test]
fn test_integer_literal() {
    let input = "123";
    let mut lexer = Lexer::new(input);
    let token = lexer.next_token();
    assert_eq!(token.kind, TokenKind::Integer(123));
}

#[test]
fn test_float_literal() {
    let input = "123.45";
    let mut lexer = Lexer::new(input);
    let token = lexer.next_token();
    assert_eq!(token.kind, TokenKind::Float(123.45));
}

#[test]
fn test_identifier() {
    let input = "my_var";
    let mut lexer = Lexer::new(input);
    let token = lexer.next_token();
    assert_eq!(token.kind, TokenKind::Identifier("my_var".to_string()));
}

#[test]
fn test_keywords() {
    let input = "fun if else val var value when for while in";
    let mut lexer = Lexer::new(input);

    let keywords = vec![
        TokenKind::Fun,
        TokenKind::If,
        TokenKind::Else,
        TokenKind::Val,
        TokenKind::Var,
        TokenKind::Value,
        TokenKind::When,
        TokenKind::For,
        TokenKind::While,
        TokenKind::In,
    ];

    for keyword in keywords {
        let token = lexer.next_token();
        assert_eq!(token.kind, keyword);
    }
}
