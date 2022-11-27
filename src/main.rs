extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::Parser;
use std::env::args;
use tokio::fs::read_to_string;

#[derive(Parser)]
#[grammar = "syntax.pest"]
struct IdentParser;

#[tokio::main]
async fn main() {
  let content = read_to_string(args().nth(1).unwrap()).await.unwrap();
  let pairs = IdentParser::parse(Rule::file, &content).unwrap_or_else(|e| panic!("{}", e)); 
  for pair in pairs {
    println!("Rule:{:?}", pair.as_rule());
    println!("Span:{:?}", pair.as_span());
    println!("Text:{}", pair.as_str());
  }
}

