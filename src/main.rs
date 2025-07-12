extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::Parser;

pub mod ast;
pub mod parser;
pub mod parser_test;

#[derive(Parser)]
#[grammar = "dyego.pest"]
pub struct DyegoParser;

fn main() {
    let source = "val a = 10";
    let result = DyegoParser::parse(Rule::program, source).expect("parse failed").next().unwrap();
    let ast = parser::parse_program(result);
    println!("{:#?}", ast);
}


