#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, PartialEq)]
pub struct Error {
    pub message: String,
    pub span: Span,
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
    Arrow,
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
    RBracket,
    RParen,
    Semicolon,
    LBracket,
    Slash,

    // Reserved words
    Else,
    Eof,
    For,
    Fun,
    If,
    In,
    Illegal,
    Object,
    Val,
    Value,
    Var,
    When,
    While,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

pub struct Lexer<'a> {
    input: &'a str,
    pub line_starts: Vec<usize>,
    position: usize,
    read_position: usize,
    ch: u8,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut l = Lexer {
            input,
            line_starts: vec![0],
            position: 0,
            read_position: 0,
            ch: 0,
        };
        l.line_starts.extend(
            input
                .char_indices()
                .filter(|&(_, c)| c == '\n')
                .map(|(i, _)| i + 1),
        );
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
            b'-' => {
                if self.peek_char() == b'>' {
                    self.read_char();
                    TokenKind::Arrow
                } else {
                    TokenKind::Minus
                }
            }
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
            b'[' => TokenKind::LBracket,
            b']' => TokenKind::RBracket,
            b'{' => TokenKind::LBrace,
            b'}' => TokenKind::RBrace,
            b':' => TokenKind::Colon,
            b';' => TokenKind::Semicolon,
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
            "object" => TokenKind::Object,
            "when" => TokenKind::When,
            "for" => TokenKind::For,
            "while" => TokenKind::While,
            "in" => TokenKind::In,
            _ => TokenKind::Identifier(ident.to_string()),
        }
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }
    }
}

#[cfg(test)]
#[path = "lexer_test.rs"]
mod lexer_tests;
