use crate::ast::{BinaryOperator, Block, Expression, IfElse, Literal, Statement, UnaryOperator, VariableStatement, FunctionDefinition, Parameter, Type, SimpleType, BaseType, ValueTypeDeclaration, ValueField, Mutability};

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // Parameterized values
    Identifier(String),
    Integer(u64),
    Float(f64),

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
        let tok = match self.ch {
            b'+' => Token::Plus,
            b'-' => Token::Minus,
            b'*' => Token::Asterisk,
            b'/' => Token::Slash,
            b'%' => Token::Percent,
            b'<' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    Token::LessThanEqual
                } else {
                    Token::LessThan
                }
            }
            b'>' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    Token::GreaterThanEqual
                } else {
                    Token::GreaterThan
                }
            }
            b'=' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    Token::EqualEqual
                } else {
                    Token::Equal
                }
            }
            b'!' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    Token::BangEqual
                } else {
                    Token::Bang
                }
            }
            b'&' => {
                if self.peek_char() == b'&' {
                    self.read_char();
                    Token::AmpersandAmpersand
                } else {
                    Token::Illegal
                }
            }
            b'|' => {
                if self.peek_char() == b'|' {
                    self.read_char();
                    Token::PipePipe
                } else {
                    Token::Illegal
                }
            }
            b'(' => Token::LParen,
            b')' => Token::RParen,
            b'{' => Token::LBrace,
            b'}' => Token::RBrace,
            b':' => Token::Colon,
            b',' => Token::Comma,
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                return self.read_identifier();
            }
            b'0'..=b'9' => {
                return self.read_number();
            }
            0 => Token::Eof,
            _ => Token::Illegal,
        };
        self.read_char();
        tok
    }

    fn read_number(&mut self) -> Token {
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
                return Token::Float(number_str.parse().unwrap());
            }
        }

        let number_str = &self.input[start..self.position];
        if let Ok(val) = number_str.parse::<u64>() {
            return Token::Integer(val);
        }
        Token::Illegal
    }

    fn read_identifier(&mut self) -> Token {
        let start = self.position;
        while self.ch.is_ascii_alphanumeric() || self.ch == b'_' {
            self.read_char();
        }
        let ident = &self.input[start..self.position];
        match ident {
            "if" => Token::If,
            "else" => Token::Else,
            "val" => Token::Val,
            "var" => Token::Var,
            "fun" => Token::Fun,
            "value" => Token::Value,
            _ => Token::Identifier(ident.to_string()),
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
            current_token: Token::Eof,
        };
        p.next_token();
        p
    }

    fn next_token(&mut self) {
        self.current_token = self.lexer.next_token();
    }

    pub fn parse_statement(&mut self) -> Result<Statement, String> {
        match self.current_token {
            Token::Val | Token::Var => self.parse_variable_statement(),
            Token::Fun => self.parse_function_statement(),
            Token::Value => self.parse_value_type_declaration(),
            _ => self.parse_expression_statement(),
        }
    }

    fn require_token(&mut self, expected: Token) -> Result<(), String> {
        if self.current_token == expected {
            Ok(())
        } else {
            Err(format!(
                "Expected token {:?}, got {:?}",
                expected,
                self.current_token
            ))
        }

    }

    fn expect_token(&mut self, expected: Token) -> Result<(), String> {
        match self.require_token(expected) {
            Ok(()) => {
                self.next_token();
                Ok(())
            }
            Err(message) => Err(message)
        }
    }

    // function_statement ::= 'fun' identifier '(' function_parameters ')' (':' type)? block
    fn parse_function_statement(&mut self) -> Result<Statement, String> {
        self.expect_token(Token::Fun)?;

        let name = match self.current_token.clone() {
            Token::Identifier(name) => name,
            _ => return Err(format!("Expected function name, got {:?}", self.current_token)),
        };
        self.next_token(); // consume function name

        self.expect_token(Token::LParen)?;

        let parameters = self.parse_function_parameters()?;

        let return_type = if self.current_token == Token::Colon {
            self.next_token(); // consume ':'
            Some(self.parse_type()?)
        } else {
            None
        };

        self.require_token(Token::LBrace)?;

        let body = self.parse_block()?;

        Ok(Statement::Function(FunctionDefinition {
            name,
            parameters,
            return_type,
            body,
        }))
    }

    // value_type_declaration ::= 'value' identifier ('(' (value_field (',' value_field)*)? ')')? block
    fn parse_value_type_declaration(&mut self) -> Result<Statement, String> {
        self.expect_token(Token::Value)?;

        let name = match self.current_token.clone() {
            Token::Identifier(name) => name,
            _ => return Err(format!("Expected value type name, got {:?}", self.current_token)),
        };
        self.next_token(); // consume value type name

        let mut fields = Vec::new();
        if self.current_token == Token::LParen {
            self.next_token(); // consume '('

            if self.current_token != Token::RParen {
                fields.push(self.parse_value_field()?);
                while self.current_token == Token::Comma {
                    self.next_token(); // consume ','
                    fields.push(self.parse_value_field()?);
                }
            }
            self.expect_token(Token::RParen)?;
        }

        self.require_token(Token::LBrace)?;

        let body = self.parse_block()?;

        Ok(Statement::ValueType(ValueTypeDeclaration {
            name,
            fields,
            body,
        }))
    }

    // function_parameters ::= (parameter (',' parameter)*)?
    fn parse_function_parameters(&mut self) -> Result<Vec<Parameter>, String> {
        let mut params = Vec::new();
        if self.current_token == Token::RParen {
            self.next_token(); // consume ')'
            return Ok(params);
        }

        params.push(self.parse_parameter()?);

        while self.current_token == Token::Comma {
            self.next_token(); // consume ','
            params.push(self.parse_parameter()?);
        }

        self.expect_token(Token::RParen)?;

        Ok(params)
    }

    // parameter ::= identifier ':' type
    fn parse_parameter(&mut self) -> Result<Parameter, String> {
        let name = match self.current_token.clone() {
            Token::Identifier(name) => name,
            _ => return Err(format!("Expected parameter name, got {:?}", self.current_token)),
        };
        self.next_token(); // consume param name

        self.expect_token(Token::Colon)?;

        let type_annotation = self.parse_type()?;

        Ok(Parameter { name, type_annotation })
    }

    // value_field ::= ('val' | 'var') identifier ':' type
    fn parse_value_field(&mut self) -> Result<ValueField, String> {
        let mutability = match self.current_token {
            Token::Val => Mutability::Val,
            Token::Var => Mutability::Var,
            _ => return Err(format!("Expected 'val' or 'var' for value field, got {:?}", self.current_token)),
        };
        self.next_token(); // consume 'val' or 'var'

        let name = match self.current_token.clone() {
            Token::Identifier(name) => name,
            _ => return Err(format!("Expected field name, got {:?}", self.current_token)),
        };
        self.next_token(); // consume field name

        self.expect_token(Token::Colon)?;

        let type_annotation = self.parse_type()?;

        Ok(ValueField {
            mutability,
            name,
            type_annotation,
        })
    }

    // type ::= identifier
    fn parse_type(&mut self) -> Result<Type, String> {
        let base_type = match self.current_token.clone() {
            Token::Identifier(name) => BaseType::User(name),
            _ => return Err(format!("Expected type name, got {:?}", self.current_token)),
        };
        self.next_token(); // consume type name

        Ok(Type::Simple(SimpleType {
            base: base_type,
            specifiers: vec![],
        }))
    }

    // variable_statement ::= ('val' | 'var') identifier '=' expression
    fn parse_variable_statement(&mut self) -> Result<Statement, String> {
        let mutable = self.current_token == Token::Var;
        self.next_token(); // consume 'val' or 'var'

        let name = match self.current_token.clone() {
            Token::Identifier(name) => name,
            _ => return Err(format!("Expected identifier, got {:?}", self.current_token)),
        };
        self.next_token(); // consume identifier

        self.expect_token(Token::Equal)?;

        let value = self.parse_expression()?;

        Ok(Statement::Variable(VariableStatement {
            mutable,
            name,
            value,
        }))
    }

    // expression_statement ::= expression
    fn parse_expression_statement(&mut self) -> Result<Statement, String> {
        let expr = self.parse_expression()?;
        Ok(Statement::Expression(expr))
    }

    // expression ::= logical_or
    pub fn parse_expression(&mut self) -> Result<Expression, String> {
        self.parse_logical_or()
    }

    // logical_or ::= logical_and ('||' logical_and)*
    fn parse_logical_or(&mut self) -> Result<Expression, String> {
        let mut left = self.parse_logical_and()?;
        while self.current_token == Token::PipePipe {
            let op = BinaryOperator::Or;
            self.next_token();
            let right = self.parse_logical_and()?;
            left = Expression::Binary(Box::new(left), op, Box::new(right));
        }
        Ok(left)
    }

    // logical_and ::= equality ('&&' equality)*
    fn parse_logical_and(&mut self) -> Result<Expression, String> {
        let mut left = self.parse_equality()?;
        while self.current_token == Token::AmpersandAmpersand {
            let op = BinaryOperator::And;
            self.next_token();
            let right = self.parse_equality()?;
            left = Expression::Binary(Box::new(left), op, Box::new(right));
        }
        Ok(left)
    }

    // equality ::= comparison (('==' | '!=') comparison)*
    fn parse_equality(&mut self) -> Result<Expression, String> {
        let mut left = self.parse_comparison()?;
        while self.current_token == Token::EqualEqual || self.current_token == Token::BangEqual {
            let op = match self.current_token {
                Token::EqualEqual => BinaryOperator::Equality,
                Token::BangEqual => BinaryOperator::Inequality,
                _ => unreachable!(),
            };
            self.next_token();
            let right = self.parse_comparison()?;
            left = Expression::Binary(Box::new(left), op, Box::new(right));
        }
        Ok(left)
    }

    // comparison ::= term (('>' | '>=' | '<' | '<=') term)*
    fn parse_comparison(&mut self) -> Result<Expression, String> {
        let mut left = self.parse_term()?;
        while self.current_token == Token::GreaterThan
            || self.current_token == Token::GreaterThanEqual
            || self.current_token == Token::LessThan
            || self.current_token == Token::LessThanEqual
        {
            let op = match self.current_token {
                Token::GreaterThan => BinaryOperator::GreaterThan,
                Token::GreaterThanEqual => BinaryOperator::GreaterThanOrEqual,
                Token::LessThan => BinaryOperator::LessThan,
                Token::LessThanEqual => BinaryOperator::LessThanOrEqual,
                _ => unreachable!(),
            };
            self.next_token();
            let right = self.parse_term()?;
            left = Expression::Binary(Box::new(left), op, Box::new(right));
        }
        Ok(left)
    }

    // term ::= factor (('+' | '-') factor)*
    fn parse_term(&mut self) -> Result<Expression, String> {
        let mut left = self.parse_factor()?;
        while self.current_token == Token::Plus || self.current_token == Token::Minus {
            let op = match self.current_token {
                Token::Plus => BinaryOperator::Add,
                Token::Minus => BinaryOperator::Subtract,
                _ => unreachable!(),
            };
            self.next_token();
            let right = self.parse_factor()?;
            left = Expression::Binary(Box::new(left), op, Box::new(right));
        }
        Ok(left)
    }

    // factor ::= unary (('*' | '/' | '%') unary)*
    fn parse_factor(&mut self) -> Result<Expression, String> {
        let mut left = self.parse_unary()?;
        while self.current_token == Token::Asterisk || self.current_token == Token::Slash || self.current_token == Token::Percent {
            let op = match self.current_token {
                Token::Asterisk => BinaryOperator::Multiply,
                Token::Slash => BinaryOperator::Divide,
                Token::Percent => BinaryOperator::Modulo,
                _ => unreachable!(),
            };
            self.next_token();
            let right = self.parse_unary()?;
            left = Expression::Binary(Box::new(left), op, Box::new(right));
        }
        Ok(left)
    }

    // unary ::= ('-' | '!') unary | primary
    fn parse_unary(&mut self) -> Result<Expression, String> {
        if self.current_token == Token::Minus || self.current_token == Token::Bang {
            let op = match self.current_token {
                Token::Minus => UnaryOperator::Negate,
                Token::Bang => UnaryOperator::Not,
                _ => unreachable!(),
            };
            self.next_token();
            let expr = self.parse_unary()?;
            Ok(Expression::Unary(op, Box::new(expr)))
        } else {
            self.parse_primary()
        }
    }

    // primary ::= integer | float | '(' expression ')' | if_expression | identifier
    fn parse_primary(&mut self) -> Result<Expression, String> {
        match self.current_token.clone() {
            Token::Integer(n) => {
                self.next_token();
                Ok(Expression::Literal(Literal::Integer(n)))
            }
            Token::Float(n) => {
                self.next_token();
                Ok(Expression::Literal(Literal::Float(n)))
            }
            Token::LParen => {
                self.next_token();
                let expr = self.parse_expression()?;
                self.expect_token(Token::RParen)?;
                Ok(Expression::GroupedExpression(Box::new(expr)))
            }
            Token::If => self.parse_if_expression(),
            Token::Identifier(name) => {
                self.next_token();
                Ok(Expression::Identifier(name))
            }
            _ => Err(format!("Unexpected token: {:?}", self.current_token)),
        }
    }

    // if_expression ::= 'if' expression block ('else' block)?
    fn parse_if_expression(&mut self) -> Result<Expression, String> {
        self.expect_token(Token::If)?;
        let condition = self.parse_expression()?;
        self.require_token(Token::LBrace)?;
        let then_branch = self.parse_block()?;
        let mut else_branch = None;
        if self.current_token == Token::Else {
            self.next_token();
            self.require_token(Token::LBrace)?;
            else_branch = Some(self.parse_block()?);
        }
        Ok(Expression::IfElse(Box::new(IfElse {
            condition,
            then_branch,
            else_branch,
        })))
    }

    // block ::= '{' statement* expression? '}'
    fn parse_block(&mut self) -> Result<Block, String> {
        self.expect_token(Token::LBrace)?;
        let mut statements = Vec::new();
        while self.current_token != Token::RBrace && self.current_token != Token::Eof {
            statements.push(self.parse_statement()?);
        }

        self.expect_token(Token::RBrace)?;

        let mut expression = None;
        if let Some(Statement::Expression(_expr)) = statements.last() {
            if let Statement::Expression(expr) = statements.pop().unwrap() {
                expression = Some(Box::new(expr));
            }
        }

        Ok(Block { statements, expression })
    }
}

#[cfg(test)]
mod tests {
    use super::{Lexer, Parser};
    use crate::ast::{BinaryOperator, Block, Expression, IfElse, Literal, Statement, UnaryOperator, VariableStatement, FunctionDefinition, Parameter, Type, SimpleType, BaseType, ValueTypeDeclaration, ValueField, Mutability};

    #[test]
    fn test_parse_float() {
        let input = "123.45";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let expected = Statement::Expression(Expression::Literal(Literal::Float(123.45)));
        assert_eq!(parser.parse_statement(), Ok(expected));
    }

    #[test]
    fn test_parse_integer() {
        let input = "123";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let expected = Statement::Expression(Expression::Literal(Literal::Integer(123)));
        assert_eq!(parser.parse_statement(), Ok(expected));
    }

    #[test]
    fn test_parse_addition() {
        let input = "1 + 2";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let expected = Statement::Expression(Expression::Binary(
            Box::new(Expression::Literal(Literal::Integer(1))),
            BinaryOperator::Add,
            Box::new(Expression::Literal(Literal::Integer(2))),
        ));
        assert_eq!(parser.parse_statement(), Ok(expected));
    }

    #[test]
    fn test_parse_precedence() {
        let input = "1 + 2 * 3";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let expected = Statement::Expression(Expression::Binary(
            Box::new(Expression::Literal(Literal::Integer(1))),
            BinaryOperator::Add,
            Box::new(Expression::Binary(
                Box::new(Expression::Literal(Literal::Integer(2))),
                BinaryOperator::Multiply,
                Box::new(Expression::Literal(Literal::Integer(3))),
            )),
        ));
        assert_eq!(parser.parse_statement(), Ok(expected));
    }

    #[test]
    fn test_parse_parentheses() {
        let input = "(1 + 2) * 3";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let expected = Statement::Expression(Expression::Binary(
            Box::new(Expression::GroupedExpression(Box::new(Expression::Binary(
                Box::new(Expression::Literal(Literal::Integer(1))),
                BinaryOperator::Add,
                Box::new(Expression::Literal(Literal::Integer(2))),
            )))),
            BinaryOperator::Multiply,
            Box::new(Expression::Literal(Literal::Integer(3))),
        ));
        assert_eq!(parser.parse_statement(), Ok(expected));
    }

    #[test]
    fn test_parse_if_expression() {
        let input = "if 1 { 2 } else { 3 }";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let expected = Statement::Expression(Expression::IfElse(Box::new(IfElse {
            condition: Expression::Literal(Literal::Integer(1)),
            then_branch: Block {
                statements: vec![],
                expression: Some(Box::new(Expression::Literal(Literal::Integer(2)))),
            },
            else_branch: Some(Block {
                statements: vec![],
                expression: Some(Box::new(Expression::Literal(Literal::Integer(3)))),
            }),
        })));
        assert_eq!(parser.parse_statement(), Ok(expected));
    }

    #[test]
    fn test_parse_unary_negation() {
        let input = "-10";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let expected = Statement::Expression(Expression::Unary(
            UnaryOperator::Negate,
            Box::new(Expression::Literal(Literal::Integer(10))),
        ));
        assert_eq!(parser.parse_statement(), Ok(expected));
    }

    #[test]
    fn test_parse_binary_precedence() {
        let input = "1 + 2 * 3 == 4 / 5 - 6 && 7 > 8 || 9 < 10";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let expected = Statement::Expression(Expression::Binary(
            Box::new(Expression::Binary(
                Box::new(Expression::Binary(
                    Box::new(Expression::Binary(
                        Box::new(Expression::Literal(Literal::Integer(1))),
                        BinaryOperator::Add,
                        Box::new(Expression::Binary(
                            Box::new(Expression::Literal(Literal::Integer(2))),
                            BinaryOperator::Multiply,
                            Box::new(Expression::Literal(Literal::Integer(3))),
                        )),
                    )),
                    BinaryOperator::Equality,
                    Box::new(Expression::Binary(
                        Box::new(Expression::Binary(
                            Box::new(Expression::Literal(Literal::Integer(4))),
                            BinaryOperator::Divide,
                            Box::new(Expression::Literal(Literal::Integer(5))),
                        )),
                        BinaryOperator::Subtract,
                        Box::new(Expression::Literal(Literal::Integer(6))),
                    )),
                )),
                BinaryOperator::And,
                Box::new(Expression::Binary(
                    Box::new(Expression::Literal(Literal::Integer(7))),
                    BinaryOperator::GreaterThan,
                    Box::new(Expression::Literal(Literal::Integer(8))),
                )),
            )),
            BinaryOperator::Or,
            Box::new(Expression::Binary(
                Box::new(Expression::Literal(Literal::Integer(9))),
                BinaryOperator::LessThan,
                Box::new(Expression::Literal(Literal::Integer(10))),
            )),
        ));
        assert_eq!(parser.parse_statement(), Ok(expected));
    }

    #[test]
    fn test_parse_variable_statement() {
        let input = "val x = 10";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let expected = Statement::Variable(VariableStatement {
            mutable: false,
            name: "x".to_string(),
            value: Expression::Literal(Literal::Integer(10)),
        });
        assert_eq!(parser.parse_statement(), Ok(expected));
    }

    #[test]
    fn test_parse_function_declaration() {
        let input = "fun my_func(a: i32, b: f64): bool { 1 }";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let expected = Statement::Function(FunctionDefinition {
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
                expression: Some(Box::new(Expression::Literal(Literal::Integer(1)))),
            },
        });
        assert_eq!(parser.parse_statement(), Ok(expected));
    }

    #[test]
    fn test_parse_value_type_declaration_with_fields() {
        let input = "value Point(val x: i32, var y: i32) { 1 }";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let expected = Statement::ValueType(ValueTypeDeclaration {
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
                expression: Some(Box::new(Expression::Literal(Literal::Integer(1)))),
            },
        });
        assert_eq!(parser.parse_statement(), Ok(expected));
    }

    #[test]
    fn test_parse_value_type_declaration_no_fields() {
        let input = "value Point { 1 }";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let expected = Statement::ValueType(ValueTypeDeclaration {
            name: "Point".to_string(),
            fields: vec![],
            body: Block {
                statements: vec![],
                expression: Some(Box::new(Expression::Literal(Literal::Integer(1)))),
            },
        });
        assert_eq!(parser.parse_statement(), Ok(expected));
    }

    #[test]
    fn test_parse_value_type_declaration_empty_fields() {
        let input = "value Point() { 1 }";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let expected = Statement::ValueType(ValueTypeDeclaration {
            name: "Point".to_string(),
            fields: vec![],
            body: Block {
                statements: vec![],
                expression: Some(Box::new(Expression::Literal(Literal::Integer(1)))),
            },
        });
        assert_eq!(parser.parse_statement(), Ok(expected));
    }
}