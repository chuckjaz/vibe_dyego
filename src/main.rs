mod ast;
mod lexer;
mod parser;

use crate::lexer::{Error, Lexer};
use crate::parser::Parser;

fn report_error(input: &str, line_starts: &[usize], error: Error) {
    let line = line_starts
        .iter()
        .rposition(|&s| s <= error.span.start)
        .unwrap_or(0);
    let col = error.span.start - line_starts[line];
    println!(
        "Error: {} at line {}, column {}",
        error.message,
        line + 1,
        col + 1
    );
    let line_str = input.lines().nth(line).unwrap_or("");
    println!("{}", line_str);
    for _ in 0..col {
        print!(" ");
    }
    println!("^");
}

fn main() {
    let input = "1 + 2 * ";
    let lexer = Lexer::new(input);
    let line_starts = lexer.line_starts.clone();
    let mut parser = Parser::new(lexer);
    match parser.parse_expression() {
        Ok(expr) => println!("{:?}", expr),
        Err(e) => report_error(input, &line_starts, e),
    }
}