mod ast;
mod parser;

use parser::{Lexer, Parser};

fn main() {
    let input = "1 + 2 * 3";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    match parser.parse_expression() {
        Ok(expr) => println!("{:?}", expr),
        Err(e) => println!("Error: {}", e),
    }
}