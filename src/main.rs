extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::Parser;
use std::{env::args, any::Any, collections::HashMap};
use tokio::fs::read_to_string;

#[derive(Parser)]
#[grammar = "syntax.pest"]
struct IdentParser;

#[tokio::main]
async fn main() {
  let content = read_to_string(args().nth(1).unwrap()).await.unwrap();
  let pairs = IdentParser::parse(Rule::file, &content).unwrap_or_else(|e| panic!("{}", e)); 
  let functions: HashMap<String, dyn Any + 'Sized> = HashMap::new();
  let variables: HashMap<String, dyn Any> = HashMap::new();

  for pair in pairs.clone() {
    println!("Rule:{:?}", pair.as_rule());
    println!("Span:{:?}", pair.as_span());
    println!("Text:{}", pair.as_str());
  }

  for pair in pairs {
    match pair.as_rule() {
      Rule::file => {},
      Rule::comment => {},
      Rule::function => {
        
      }
    }
  }
}

