use crate::ast::{
    BaseType, BinaryOperator, Block, Expression, ExpressionKind, FunctionDefinition, IfElse,
    Literal, Mutability, Parameter, SimpleType, Statement, StatementKind, Tuple, TupleType, Type,
    UnaryOperator, ValueField, ValueTypeDeclaration, VariableStatement, WhenBranch,
    WhenExpression,
};

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, PartialEq, Clone)]
#[allow(clippy::upper_case_acronyms)]
pub enum TokenKind {
    // Parameterized values
    Identifier(String),
    Integer(u64),
    Float(f64),
    String(String),

    // Operators
    AmpersandAmpersand,
    Asterisk,
    Bang,
    BangEqual,
    Colon,
    Comma,
    Equal,
    EqualEqual,
    GreaterThan,
    GreaterThanEqual,
    LBrace,
    LParen,
    LessThan,
    LessThanEqual,
    Minus,
    Percent,
    PipePipe,
    Plus,
    RArrow,
    RBrace,
    RParen,
    Slash,

    // Reserved words
    Else,
    Eof,
    Fun,
    If,
    Illegal,
    Val,
    Value,
    Var,
    When,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

pub struct Lexer<'a> {
    input: &'a str,
    position: usize,
    read_position: usize,
    ch: u8,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut l = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: 0,
        };
        l.read_char();
        l
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = 0;
        } else {
            self.ch = self.input.as_bytes()[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&self) -> u8 {
        if self.read_position >= self.input.len() {
            0
        } else {
            self.input.as_bytes()[self.read_position]
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let start = self.position;
        let kind = match self.ch {
            b'+' => TokenKind::Plus,
            b'-' => TokenKind::Minus,
            b'*' => TokenKind::Asterisk,
            b'/' => TokenKind::Slash,
            b'%' => TokenKind::Percent,
            b'<' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    TokenKind::LessThanEqual
                } else {
                    TokenKind::LessThan
                }
            }
            b'>' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    TokenKind::GreaterThanEqual
                } else {
                    TokenKind::GreaterThan
                }
            }
            b'=' => {
                if self.peek_char() == b'>' {
                    self.read_char();
                    TokenKind::RArrow
                } else if self.peek_char() == b'=' {
                    self.read_char();
                    TokenKind::EqualEqual
                } else {
                    TokenKind::Equal
                }
            }
            b'!' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    TokenKind::BangEqual
                } else {
                    TokenKind::Bang
                }
            }
            b'&' => {
                if self.peek_char() == b'&' {
                    self.read_char();
                    TokenKind::AmpersandAmpersand
                } else {
                    TokenKind::Illegal
                }
            }
            b'|' => {
                if self.peek_char() == b'|' {
                    self.read_char();
                    TokenKind::PipePipe
                } else {
                    TokenKind::Illegal
                }
            }
            b'(' => TokenKind::LParen,
            b')' => TokenKind::RParen,
            b'{' => TokenKind::LBrace,
            b'}' => TokenKind::RBrace,
            b':' => TokenKind::Colon,
            b',' => TokenKind::Comma,
            b'"' => {
                let kind = self.read_string();
                let end = self.position;
                return Token {
                    kind,
                    span: Span { start, end },
                };
            }
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                let kind = self.read_identifier();
                let end = self.position;
                return Token {
                    kind,
                    span: Span { start, end },
                };
            }
            b'0'..=b'9' => {
                let kind = self.read_number();
                let end = self.position;
                return Token {
                    kind,
                    span: Span { start, end },
                };
            }
            0 => TokenKind::Eof,
            _ => TokenKind::Illegal,
        };
        self.read_char();
        let end = self.position;
        Token {
            kind,
            span: Span { start, end },
        }
    }

    fn read_string(&mut self) -> TokenKind {
        let start = self.position + 1;
        self.read_char();
        while self.ch != b'"' && self.ch != 0 {
            self.read_char();
        }
        if self.ch == 0 {
            return TokenKind::Illegal;
        }
        let end = self.position;
        self.read_char();
        TokenKind::String(self.input[start..end].to_string())
    }

    fn read_number(&mut self) -> TokenKind {
        let start = self.position;
        let mut is_float = false;
        while self.ch.is_ascii_digit() {
            if self.peek_char() == b'.' {
                is_float = true;
            }
            self.read_char();
        }

        if is_float {
            if self.ch == b'.' {
                self.read_char();
                while self.ch.is_ascii_digit() {
                    self.read_char();
                }
                let number_str = &self.input[start..self.position];
                return TokenKind::Float(number_str.parse().unwrap());
            }
        }

        let number_str = &self.input[start..self.position];
        if let Ok(val) = number_str.parse::<u64>() {
            return TokenKind::Integer(val);
        }
        TokenKind::Illegal
    }

    fn read_identifier(&mut self) -> TokenKind {
        let start = self.position;
        while self.ch.is_ascii_alphanumeric() || self.ch == b'_' {
            self.read_char();
        }
        let ident = &self.input[start..self.position];
        match ident {
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "val" => TokenKind::Val,
            "var" => TokenKind::Var,
            "fun" => TokenKind::Fun,
            "value" => TokenKind::Value,
            "when" => TokenKind::When,
            _ => TokenKind::Identifier(ident.to_string()),
        }
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }
    }
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Token,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        let mut p = Parser {
            lexer,
            current_token: Token {
                kind: TokenKind::Eof,
                span: Span { start: 0, end: 0 },
            },
        };
        p.next_token();
        p
    }

    fn next_token(&mut self) {
        self.current_token = self.lexer.next_token();
    }

    pub fn parse_statement(&mut self) -> Result<Statement, String> {
        match self.current_token.kind {
            TokenKind::Val | TokenKind::Var => self.parse_variable_statement(),
            TokenKind::Fun => self.parse_function_statement(),
            TokenKind::Value => self.parse_value_type_declaration(),
            _ => self.parse_expression_statement(),
        }
    }

    fn require_token(&mut self, expected: TokenKind) -> Result<(), String> {
        if self.current_token.kind == expected {
            Ok(())
        } else {
            Err(format!(
                "Expected token {:?}, got {:?} at span {:?}",
                expected, self.current_token.kind, self.current_token.span
            ))
        }
    }

    fn expect_token(&mut self, expected: TokenKind) -> Result<(), String> {
        match self.require_token(expected.clone()) {
            Ok(()) => {
                self.next_token();
                Ok(())
            }
            Err(message) => Err(message),
        }
    }

    // function_statement ::= 'fun' identifier '(' function_parameters ')' (':' type)? block
    fn parse_function_statement(&mut self) -> Result<Statement, String> {
        let start_span = self.current_token.span;
        self.expect_token(TokenKind::Fun)?;

        let name = match self.current_token.kind.clone() {
            TokenKind::Identifier(name) => name,
            _ => {
                return Err(format!(
                    "Expected function name, got {:?}",
                    self.current_token.kind
                ))
            }
        };
        self.next_token(); // consume function name

        self.expect_token(TokenKind::LParen)?;

        let parameters = self.parse_function_parameters()?;

        let return_type = if self.current_token.kind == TokenKind::Colon {
            self.next_token(); // consume ':'
            Some(self.parse_type()?)
        } else {
            None
        };

        self.require_token(TokenKind::LBrace)?;

        let (body, body_span) = self.parse_block()?;
        let end_span = body_span;

        Ok(Statement {
            kind: StatementKind::Function(FunctionDefinition {
                name,
                parameters,
                return_type,
                body,
            }),
            span: Span {
                start: start_span.start,
                end: end_span.end,
            },
        })
    }

    // value_type_declaration ::= 'value' identifier ('(' (value_field (',' value_field)*)? ')')? block
    fn parse_value_type_declaration(&mut self) -> Result<Statement, String> {
        let start_span = self.current_token.span;
        self.expect_token(TokenKind::Value)?;

        let name = match self.current_token.kind.clone() {
            TokenKind::Identifier(name) => name,
            _ => {
                return Err(format!(
                    "Expected value type name, got {:?}",
                    self.current_token.kind
                ))
            }
        };
        self.next_token(); // consume value type name

        let mut fields = Vec::new();
        if self.current_token.kind == TokenKind::LParen {
            self.next_token(); // consume '('

            if self.current_token.kind != TokenKind::RParen {
                fields.push(self.parse_value_field()?);
                while self.current_token.kind == TokenKind::Comma {
                    self.next_token(); // consume ','
                    fields.push(self.parse_value_field()?);
                }
            }
            self.expect_token(TokenKind::RParen)?;
        }

        self.require_token(TokenKind::LBrace)?;

        let (body, body_span) = self.parse_block()?;
        let end_span = body_span;

        Ok(Statement {
            kind: StatementKind::ValueType(ValueTypeDeclaration {
                name,
                fields,
                body,
            }),
            span: Span {
                start: start_span.start,
                end: end_span.end,
            },
        })
    }

    // function_parameters ::= (parameter (',' parameter)*)?
    fn parse_function_parameters(&mut self) -> Result<Vec<Parameter>, String> {
        let mut params = Vec::new();
        if self.current_token.kind == TokenKind::RParen {
            self.next_token(); // consume ')'
            return Ok(params);
        }

        params.push(self.parse_parameter()?);

        while self.current_token.kind == TokenKind::Comma {
            self.next_token(); // consume ','
            params.push(self.parse_parameter()?);
        }

        self.expect_token(TokenKind::RParen)?;

        Ok(params)
    }

    // parameter ::= identifier ':' type
    fn parse_parameter(&mut self) -> Result<Parameter, String> {
        let name = match self.current_token.kind.clone() {
            TokenKind::Identifier(name) => name,
            _ => return Err(format!("Expected parameter name, got {:?}", self.current_token.kind)),
        };
        self.next_token(); // consume param name

        self.expect_token(TokenKind::Colon)?;

        let type_annotation = self.parse_type()?;

        Ok(Parameter {
            name,
            type_annotation,
        })
    }

    // value_field ::= ('val' | 'var') identifier ':' type
    fn parse_value_field(&mut self) -> Result<ValueField, String> {
        let mutability = match self.current_token.kind {
            TokenKind::Val => Mutability::Val,
            TokenKind::Var => Mutability::Var,
            _ => {
                return Err(format!(
                    "Expected 'val' or 'var' for value field, got {:?}",
                    self.current_token.kind
                ))
            }
        };
        self.next_token(); // consume 'val' or 'var'

        let name = match self.current_token.kind.clone() {
            TokenKind::Identifier(name) => name,
            _ => return Err(format!("Expected field name, got {:?}", self.current_token.kind)),
        };
        self.next_token(); // consume field name

        self.expect_token(TokenKind::Colon)?;

        let type_annotation = self.parse_type()?;

        Ok(ValueField {
            mutability,
            name,
            type_annotation,
        })
    }

    // type ::= identifier | tuple_type
    fn parse_type(&mut self) -> Result<Type, String> {
        let base_type = match self.current_token.kind.clone() {
            TokenKind::Identifier(name) => {
                self.next_token(); // consume type name
                BaseType::User(name)
            }
            TokenKind::LParen => return self.parse_tuple_type(),
            _ => return Err(format!("Expected type name, got {:?}", self.current_token.kind)),
        };

        Ok(Type::Simple(SimpleType {
            base: base_type,
            specifiers: vec![],
        }))
    }

    // tuple_type ::= '(' (type (',' type)*)? ','? ')'
    fn parse_tuple_type(&mut self) -> Result<Type, String> {
        self.expect_token(TokenKind::LParen)?;
        let mut types = Vec::new();
        if self.current_token.kind != TokenKind::RParen {
            types.push(self.parse_type()?);
            while self.current_token.kind == TokenKind::Comma {
                self.next_token();
                if self.current_token.kind == TokenKind::RParen {
                    break;
                }
                types.push(self.parse_type()?);
            }
        }
        self.expect_token(TokenKind::RParen)?;
        Ok(Type::Simple(SimpleType {
            base: BaseType::Tuple(TupleType { types }),
            specifiers: vec![],
        }))
    }

    // variable_statement ::= ('val' | 'var') identifier (':' type)? '=' expression
    fn parse_variable_statement(&mut self) -> Result<Statement, String> {
        let start_span = self.current_token.span;
        let mutable = self.current_token.kind == TokenKind::Var;
        self.next_token(); // consume 'val' or 'var'

        let name = match self.current_token.kind.clone() {
            TokenKind::Identifier(name) => name,
            _ => return Err(format!("Expected identifier, got {:?}", self.current_token.kind)),
        };
        self.next_token(); // consume identifier

        let type_annotation = if self.current_token.kind == TokenKind::Colon {
            self.next_token(); // consume ':'
            Some(self.parse_type()?)
        } else {
            None
        };

        self.expect_token(TokenKind::Equal)?;

        let value = self.parse_expression()?;
        let end_span = value.span;

        Ok(Statement {
            kind: StatementKind::Variable(VariableStatement {
                mutable,
                name,
                type_annotation,
                value,
            }),
            span: Span {
                start: start_span.start,
                end: end_span.end,
            },
        })
    }

    // expression_statement ::= expression
    fn parse_expression_statement(&mut self) -> Result<Statement, String> {
        let expr = self.parse_expression()?;
        let span = expr.span;
        Ok(Statement {
            kind: StatementKind::Expression(expr),
            span,
        })
    }

    // expression ::= logical_or
    pub fn parse_expression(&mut self) -> Result<Expression, String> {
        self.parse_logical_or()
    }

    // logical_or ::= logical_and ('||' logical_and)*
    fn parse_logical_or(&mut self) -> Result<Expression, String> {
        let mut left = self.parse_logical_and()?;
        while self.current_token.kind == TokenKind::PipePipe {
            let op = BinaryOperator::Or;
            self.next_token();
            let right = self.parse_logical_and()?;
            let span = Span {
                start: left.span.start,
                end: right.span.end,
            };
            left = Expression {
                kind: ExpressionKind::Binary(Box::new(left), op, Box::new(right)),
                span,
            };
        }
        Ok(left)
    }

    // logical_and ::= equality ('&&' equality)*
    fn parse_logical_and(&mut self) -> Result<Expression, String> {
        let mut left = self.parse_equality()?;
        while self.current_token.kind == TokenKind::AmpersandAmpersand {
            let op = BinaryOperator::And;
            self.next_token();
            let right = self.parse_equality()?;
            let span = Span {
                start: left.span.start,
                end: right.span.end,
            };
            left = Expression {
                kind: ExpressionKind::Binary(Box::new(left), op, Box::new(right)),
                span,
            };
        }
        Ok(left)
    }

    // equality ::= comparison (('==' | '!=') comparison)*
    fn parse_equality(&mut self) -> Result<Expression, String> {
        let mut left = self.parse_comparison()?;
        while self.current_token.kind == TokenKind::EqualEqual
            || self.current_token.kind == TokenKind::BangEqual
        {
            let op = match self.current_token.kind {
                TokenKind::EqualEqual => BinaryOperator::Equality,
                TokenKind::BangEqual => BinaryOperator::Inequality,
                _ => unreachable!(),
            };
            self.next_token();
            let right = self.parse_comparison()?;
            let span = Span {
                start: left.span.start,
                end: right.span.end,
            };
            left = Expression {
                kind: ExpressionKind::Binary(Box::new(left), op, Box::new(right)),
                span,
            };
        }
        Ok(left)
    }

    // comparison ::= term (('>' | '>=' | '<' | '<=') term)*
    fn parse_comparison(&mut self) -> Result<Expression, String> {
        let mut left = self.parse_term()?;
        while self.current_token.kind == TokenKind::GreaterThan
            || self.current_token.kind == TokenKind::GreaterThanEqual
            || self.current_token.kind == TokenKind::LessThan
            || self.current_token.kind == TokenKind::LessThanEqual
        {
            let op = match self.current_token.kind {
                TokenKind::GreaterThan => BinaryOperator::GreaterThan,
                TokenKind::GreaterThanEqual => BinaryOperator::GreaterThanOrEqual,
                TokenKind::LessThan => BinaryOperator::LessThan,
                TokenKind::LessThanEqual => BinaryOperator::LessThanOrEqual,
                _ => unreachable!(),
            };
            self.next_token();
            let right = self.parse_term()?;
            let span = Span {
                start: left.span.start,
                end: right.span.end,
            };
            left = Expression {
                kind: ExpressionKind::Binary(Box::new(left), op, Box::new(right)),
                span,
            };
        }
        Ok(left)
    }

    // term ::= factor (('+' | '-') factor)*
    fn parse_term(&mut self) -> Result<Expression, String> {
        let mut left = self.parse_factor()?;
        while self.current_token.kind == TokenKind::Plus || self.current_token.kind == TokenKind::Minus
        {
            let op = match self.current_token.kind {
                TokenKind::Plus => BinaryOperator::Add,
                TokenKind::Minus => BinaryOperator::Subtract,
                _ => unreachable!(),
            };
            self.next_token();
            let right = self.parse_factor()?;
            let span = Span {
                start: left.span.start,
                end: right.span.end,
            };
            left = Expression {
                kind: ExpressionKind::Binary(Box::new(left), op, Box::new(right)),
                span,
            };
        }
        Ok(left)
    }

    // factor ::= unary (('*' | '/' | '%') unary)*
    fn parse_factor(&mut self) -> Result<Expression, String> {
        let mut left = self.parse_unary()?;
        while self.current_token.kind == TokenKind::Asterisk
            || self.current_token.kind == TokenKind::Slash
            || self.current_token.kind == TokenKind::Percent
        {
            let op = match self.current_token.kind {
                TokenKind::Asterisk => BinaryOperator::Multiply,
                TokenKind::Slash => BinaryOperator::Divide,
                TokenKind::Percent => BinaryOperator::Modulo,
                _ => unreachable!(),
            };
            self.next_token();
            let right = self.parse_unary()?;
            let span = Span {
                start: left.span.start,
                end: right.span.end,
            };
            left = Expression {
                kind: ExpressionKind::Binary(Box::new(left), op, Box::new(right)),
                span,
            };
        }
        Ok(left)
    }

    // unary ::= ('-' | '!') unary | primary
    fn parse_unary(&mut self) -> Result<Expression, String> {
        if self.current_token.kind == TokenKind::Minus || self.current_token.kind == TokenKind::Bang {
            let start_span = self.current_token.span;
            let op = match self.current_token.kind {
                TokenKind::Minus => UnaryOperator::Negate,
                TokenKind::Bang => UnaryOperator::Not,
                _ => unreachable!(),
            };
            self.next_token();
            let expr = self.parse_unary()?;
            let end_span = expr.span;
            Ok(Expression {
                kind: ExpressionKind::Unary(op, Box::new(expr)),
                span: Span {
                    start: start_span.start,
                    end: end_span.end,
                },
            })
        } else {
            self.parse_primary()
        }
    }

    // primary ::= integer | float | '(' expression ')' | if_expression | identifier
    fn parse_primary(&mut self) -> Result<Expression, String> {
        let span = self.current_token.span;
        match self.current_token.kind.clone() {
            TokenKind::Integer(n) => {
                self.next_token();
                Ok(Expression {
                    kind: ExpressionKind::Literal(Literal::Integer(n)),
                    span,
                })
            }
            TokenKind::Float(n) => {
                self.next_token();
                Ok(Expression {
                    kind: ExpressionKind::Literal(Literal::Float(n)),
                    span,
                })
            }
            TokenKind::String(s) => {
                self.next_token();
                Ok(Expression {
                    kind: ExpressionKind::Literal(Literal::String(s)),
                    span,
                })
            }
            TokenKind::LParen => self.parse_paren_expression(),
            TokenKind::If => self.parse_if_expression(),
            TokenKind::When => self.parse_when_expression(),
            TokenKind::Identifier(name) => {
                self.next_token();
                Ok(Expression {
                    kind: ExpressionKind::Identifier(name),
                    span,
                })
            }
            _ => Err(format!("Unexpected token: {:?}", self.current_token.kind)),
        }
    }

    fn parse_paren_expression(&mut self) -> Result<Expression, String> {
        let start_span = self.current_token.span;
        self.expect_token(TokenKind::LParen)?;
        // Empty tuple
        if self.current_token.kind == TokenKind::RParen {
            let end_span = self.current_token.span;
            self.next_token();
            return Ok(Expression {
                kind: ExpressionKind::Tuple(Tuple { elements: vec![] }),
                span: Span {
                    start: start_span.start,
                    end: end_span.end,
                },
            });
        }
        let expr = self.parse_expression()?;
        if self.current_token.kind == TokenKind::Comma {
            self.next_token();
            let mut elements = vec![expr];
            while self.current_token.kind != TokenKind::RParen {
                elements.push(self.parse_expression()?);
                if self.current_token.kind == TokenKind::Comma {
                    self.next_token();
                } else {
                    break;
                }
            }
            let end_span = self.current_token.span;
            self.expect_token(TokenKind::RParen)?;
            Ok(Expression {
                kind: ExpressionKind::Tuple(Tuple { elements }),
                span: Span {
                    start: start_span.start,
                    end: end_span.end,
                },
            })
        } else {
            let end_span = self.current_token.span;
            self.expect_token(TokenKind::RParen)?;
            Ok(Expression {
                kind: ExpressionKind::GroupedExpression(Box::new(expr)),
                span: Span {
                    start: start_span.start,
                    end: end_span.end,
                },
            })
        }
    }

    // if_expression ::= 'if' expression block ('else' block)?
    fn parse_if_expression(&mut self) -> Result<Expression, String> {
        let start_span = self.current_token.span;
        self.expect_token(TokenKind::If)?;
        let condition = self.parse_expression()?;
        self.require_token(TokenKind::LBrace)?;
        let (then_branch, mut end_span) = self.parse_block()?;
        let mut else_branch = None;
        if self.current_token.kind == TokenKind::Else {
            self.next_token();
            self.require_token(TokenKind::LBrace)?;
            let (block, else_span) = self.parse_block()?;
            else_branch = Some(block);
            end_span = else_span;
        }
        Ok(Expression {
            kind: ExpressionKind::IfElse(Box::new(IfElse {
                condition,
                then_branch,
                else_branch,
            })),
            span: Span {
                start: start_span.start,
                end: end_span.end,
            },
        })
    }

    // when_expression ::= 'when' expression '{' (when_branch (',' when_branch)*)? '}'
    fn parse_when_expression(&mut self) -> Result<Expression, String> {
        let start_span = self.current_token.span;
        self.expect_token(TokenKind::When)?;
        let expression = self.parse_expression()?;
        self.expect_token(TokenKind::LBrace)?;

        let mut branches = Vec::new();
        if self.current_token.kind != TokenKind::RBrace {
            branches.push(self.parse_when_branch()?);
            while self.current_token.kind == TokenKind::Comma {
                self.next_token(); // consume ','
                if self.current_token.kind == TokenKind::RBrace {
                    break;
                }
                branches.push(self.parse_when_branch()?);
            }
        }

        let end_span = self.current_token.span;
        self.expect_token(TokenKind::RBrace)?;

        Ok(Expression {
            kind: ExpressionKind::When(Box::new(WhenExpression {
                expression,
                branches,
            })),
            span: Span {
                start: start_span.start,
                end: end_span.end,
            },
        })
    }

    // when_branch ::= expression '=>' expression
    fn parse_when_branch(&mut self) -> Result<WhenBranch, String> {
        let condition = self.parse_expression()?;
        self.expect_token(TokenKind::RArrow)?;
        let result = self.parse_expression()?;
        Ok(WhenBranch { condition, result })
    }

    // block ::= '{' statement* expression? '}'
    fn parse_block(&mut self) -> Result<(Block, Span), String> {
        let start_span = self.current_token.span;
        self.expect_token(TokenKind::LBrace)?;
        let mut statements = Vec::new();
        while self.current_token.kind != TokenKind::RBrace && self.current_token.kind != TokenKind::Eof {
            statements.push(self.parse_statement()?);
        }

        let end_span = self.current_token.span;
        self.expect_token(TokenKind::RBrace)?;

        let mut expression = None;
        if let Some(stmt) = statements.last() {
            if let StatementKind::Expression(_expr) = &stmt.kind {
                if let Statement {
                    kind: StatementKind::Expression(expr),
                    ..
                } = statements.pop().unwrap()
                {
                    expression = Some(Box::new(expr));
                }
            }
        }

        Ok((
            Block {
                statements,
                expression,
            },
            Span {
                start: start_span.start,
                end: end_span.end,
            },
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::{ExpressionKind, Lexer, Parser, Span, StatementKind};
    use crate::ast::{
        BaseType, BinaryOperator, Block, Expression, FunctionDefinition, IfElse, Literal,
        Mutability, Parameter, SimpleType, Statement, Tuple, TupleType, Type, UnaryOperator,
        ValueField, ValueTypeDeclaration, VariableStatement, WhenBranch, WhenExpression,
    };

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
                mutable: false,
                name: "x".to_string(),
                type_annotation: Some(Type::Simple(SimpleType {
                    base: BaseType::Tuple(TupleType {
                        types: vec![
                            Type::Simple(SimpleType {
                                base: BaseType::User("i32".to_string()),
                                specifiers: vec![],
                            }),
                            Type::Simple(SimpleType {
                                base: BaseType::User("f64".to_string()),
                                specifiers: vec![],
                            }),
                        ],
                    }),
                    specifiers: vec![],
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
                name: "a".to_string(),
                parameters: vec![Parameter {
                    name: "x".to_string(),
                    type_annotation: Type::Simple(SimpleType {
                        base: BaseType::Tuple(TupleType {
                            types: vec![
                                Type::Simple(SimpleType {
                                    base: BaseType::User("i32".to_string()),
                                    specifiers: vec![],
                                }),
                                Type::Simple(SimpleType {
                                    base: BaseType::User("f64".to_string()),
                                    specifiers: vec![],
                                }),
                            ],
                        }),
                        specifiers: vec![],
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
                name: "my_func".to_string(),
                parameters: vec![
                    Parameter {
                        name: "a".to_string(),
                        type_annotation: Type::Simple(SimpleType {
                            base: BaseType::User("i32".to_string()),
                            specifiers: vec![],
                        }),
                    },
                    Parameter {
                        name: "b".to_string(),
                        type_annotation: Type::Simple(SimpleType {
                            base: BaseType::User("f64".to_string()),
                            specifiers: vec![],
                        }),
                    },
                ],
                return_type: Some(Type::Simple(SimpleType {
                    base: BaseType::User("bool".to_string()),
                    specifiers: vec![],
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
                name: "Point".to_string(),
                fields: vec![
                    ValueField {
                        mutability: Mutability::Val,
                        name: "x".to_string(),
                        type_annotation: Type::Simple(SimpleType {
                            base: BaseType::User("i32".to_string()),
                            specifiers: vec![],
                        }),
                    },
                    ValueField {
                        mutability: Mutability::Var,
                        name: "y".to_string(),
                        type_annotation: Type::Simple(SimpleType {
                            base: BaseType::User("i32".to_string()),
                            specifiers: vec![],
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
}