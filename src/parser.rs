use crate::ast::{
    BaseType, BinaryOperator, Block, Expression, ExpressionKind, ForLoop, FunctionDefinition,
    IfElse, Literal, Mutability, ObjectTypeDeclaration, Parameter, SimpleType, Statement,
    StatementKind, Tuple, TupleType, Type, UnaryOperator, ValueField, ValueTypeDeclaration,
    VariableStatement, WhenBranch, WhenExpression, WhileLoop,
};
use crate::lexer::{Error, Lexer, Span, Token, TokenKind};

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

    pub fn parse_statement(&mut self) -> Result<Statement, Error> {
        match self.current_token.kind {
            TokenKind::Val | TokenKind::Var => self.parse_variable_statement(),
            TokenKind::Fun => self.parse_function_statement(),
            TokenKind::Value => self.parse_value_type_declaration(),
            TokenKind::Object => self.parse_object_type_declaration(),
            TokenKind::While => self.parse_while_statement(),
            TokenKind::For => self.parse_for_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn require_token(&mut self, expected: TokenKind) -> Result<(), Error> {
        if self.current_token.kind == expected {
            Ok(())
        } else {
            Err(Error {
                message: format!("Expected token {:?}, got {:?}", expected, self.current_token.kind),
                span: self.current_token.span,
            })
        }
    }

    fn expect_token(&mut self, expected: TokenKind) -> Result<(), Error> {
        self.require_token(expected)?;
        self.next_token();
        Ok(())
    }

    // function_statement ::= 'fun' identifier '(' function_parameters ')' (':' type)? block
    fn parse_function_statement(&mut self) -> Result<Statement, Error> {
        let start_span = self.current_token.span;
        self.expect_token(TokenKind::Fun)?;

        let name = match self.current_token.kind.clone() {
            TokenKind::Identifier(name) => name,
            _ => {
                return Err(Error {
                    message: format!("Expected function name, got {:?}", self.current_token.kind),
                    span: self.current_token.span,
                })
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

    // object_type_declaration ::= 'object' identifier ('(' (value_field (',' value_field)*)? ')')? block
    fn parse_object_type_declaration(&mut self) -> Result<Statement, Error> {
        let start_span = self.current_token.span;
        self.expect_token(TokenKind::Object)?;

        let name = match self.current_token.kind.clone() {
            TokenKind::Identifier(name) => name,
            _ => {
                return Err(Error {
                    message: format!(
                        "Expected object type name, got {:?}",
                        self.current_token.kind
                    ),
                    span: self.current_token.span,
                })
            }
        };
        self.next_token(); // consume object type name

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
            kind: StatementKind::ObjectType(ObjectTypeDeclaration {
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

    // value_type_declaration ::= 'value' identifier ('(' (value_field (',' value_field)*)? ')')? block
    fn parse_value_type_declaration(&mut self) -> Result<Statement, Error> {
        let start_span = self.current_token.span;
        self.expect_token(TokenKind::Value)?;

        let name = match self.current_token.kind.clone() {
            TokenKind::Identifier(name) => name,
            _ => {
                return Err(Error {
                    message: format!("Expected value type name, got {:?}", self.current_token.kind),
                    span: self.current_token.span,
                })
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
    fn parse_function_parameters(&mut self) -> Result<Vec<Parameter>, Error> {
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
    fn parse_parameter(&mut self) -> Result<Parameter, Error> {
        let name = match self.current_token.kind.clone() {
            TokenKind::Identifier(name) => name,
            _ => {
                return Err(Error {
                    message: format!("Expected parameter name, got {:?}", self.current_token.kind),
                    span: self.current_token.span,
                })
            }
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
    fn parse_value_field(&mut self) -> Result<ValueField, Error> {
        let mutability = match self.current_token.kind {
            TokenKind::Val => Mutability::Val,
            TokenKind::Var => Mutability::Var,
            _ => {
                return Err(Error {
                    message: format!(
                        "Expected 'val' or 'var' for value field, got {:?}",
                        self.current_token.kind
                    ),
                    span: self.current_token.span,
                })
            }
        };
        self.next_token(); // consume 'val' or 'var'

        let name = match self.current_token.kind.clone() {
            TokenKind::Identifier(name) => name,
            _ => {
                return Err(Error {
                    message: format!("Expected field name, got {:?}", self.current_token.kind),
                    span: self.current_token.span,
                })
            }
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
    fn parse_type(&mut self) -> Result<Type, Error> {
        let base_type = match self.current_token.kind.clone() {
            TokenKind::Identifier(name) => {
                self.next_token(); // consume type name
                BaseType::User(name)
            }
            TokenKind::LParen => return self.parse_tuple_type(),
            _ => {
                return Err(Error {
                    message: format!("Expected type name, got {:?}", self.current_token.kind),
                    span: self.current_token.span,
                })
            }
        };

        Ok(Type::Simple(SimpleType {
            base: base_type,
            specifiers: vec![],
        }))
    }

    // tuple_type ::= '(' (type (',' type)*)? ','? ')'
    fn parse_tuple_type(&mut self) -> Result<Type, Error> {
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
    fn parse_variable_statement(&mut self) -> Result<Statement, Error> {
        let start_span = self.current_token.span;
        let mutable = self.current_token.kind == TokenKind::Var;
        self.next_token(); // consume 'val' or 'var'

        let name = match self.current_token.kind.clone() {
            TokenKind::Identifier(name) => name,
            _ => {
                return Err(Error {
                    message: format!("Expected identifier, got {:?}", self.current_token.kind),
                    span: self.current_token.span,
                })
            }
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
    fn parse_expression_statement(&mut self) -> Result<Statement, Error> {
        let expr = self.parse_expression()?;
        let span = expr.span;
        Ok(Statement {
            kind: StatementKind::Expression(expr),
            span,
        })
    }

    // while_statement ::= 'while' expression block
    fn parse_while_statement(&mut self) -> Result<Statement, Error> {
        let start_span = self.current_token.span;
        self.expect_token(TokenKind::While)?;
        let condition = self.parse_expression()?;
        self.require_token(TokenKind::LBrace)?;
        let (body, end_span) = self.parse_block()?;
        Ok(Statement {
            kind: StatementKind::WhileLoop(Box::new(WhileLoop { condition, body })),
            span: Span {
                start: start_span.start,
                end: end_span.end,
            },
        })
    }

    // for_statement ::= 'for' identifier 'in' expression block
    fn parse_for_statement(&mut self) -> Result<Statement, Error> {
        let start_span = self.current_token.span;
        self.expect_token(TokenKind::For)?;
        let iterator = match self.current_token.kind.clone() {
            TokenKind::Identifier(name) => {
                self.next_token();
                name
            }
            _ => {
                return Err(Error {
                    message: format!("Expected identifier, got {:?}", self.current_token.kind),
                    span: self.current_token.span,
                })
            }
        };
        self.expect_token(TokenKind::In)?;
        let iterable = self.parse_expression()?;
        self.require_token(TokenKind::LBrace)?;
        let (body, end_span) = self.parse_block()?;
        Ok(Statement {
            kind: StatementKind::ForLoop(Box::new(ForLoop {
                iterator,
                iterable,
                body,
            })),
            span: Span {
                start: start_span.start,
                end: end_span.end,
            },
        })
    }

    // expression ::= logical_or
    pub fn parse_expression(&mut self) -> Result<Expression, Error> {
        self.parse_logical_or()
    }

    // logical_or ::= logical_and ('||' logical_and)*
    fn parse_logical_or(&mut self) -> Result<Expression, Error> {
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
    fn parse_logical_and(&mut self) -> Result<Expression, Error> {
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
    fn parse_equality(&mut self) -> Result<Expression, Error> {
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
    fn parse_comparison(&mut self) -> Result<Expression, Error> {
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
    fn parse_term(&mut self) -> Result<Expression, Error> {
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
    fn parse_factor(&mut self) -> Result<Expression, Error> {
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
    fn parse_unary(&mut self) -> Result<Expression, Error> {
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
    fn parse_primary(&mut self) -> Result<Expression, Error> {
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
            _ => Err(Error {
                message: format!("Unexpected token: {:?}", self.current_token.kind),
                span,
            }),
        }
    }

    fn parse_paren_expression(&mut self) -> Result<Expression, Error> {
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
    fn parse_if_expression(&mut self) -> Result<Expression, Error> {
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
    fn parse_when_expression(&mut self) -> Result<Expression, Error> {
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
    fn parse_when_branch(&mut self) -> Result<WhenBranch, Error> {
        let condition = self.parse_expression()?;
        self.expect_token(TokenKind::RArrow)?;
        let result = self.parse_expression()?;
        Ok(WhenBranch { condition, result })
    }

    // block ::= '{' statement* expression? '}'
    fn parse_block(&mut self) -> Result<(Block, Span), Error> {
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
#[path = "parser_tests.rs"]
mod parser_tests;