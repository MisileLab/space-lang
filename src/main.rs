extern crate pest;
#[macro_use]
extern crate pest_derive;

use inkwell::context::Context;
use pest::Parser;
use std::env::args;
use tokio::fs::read_to_string;
use fraction::{DynaInt, DynaDecimal};

#[derive(Parser)]
#[grammar = "syntax.pest"]
struct IdentParser;

pub enum Operator { Equal, NotEqual, And, Or }
pub enum Types { String(String), Float(DynaDecimal<usize, u8>), Int(DynaInt<u32, u64>), Bool(bool) }

#[tokio::main]
async fn main() {
  let content = read_to_string(args().nth(1).unwrap()).await.unwrap();
  let pairs = IdentParser::parse(Rule::file, &content).unwrap_or_else(|e| panic!("{}", e)); 

  for pair in pairs.clone() {
    println!("Rule:{:?}", pair.as_rule());
    println!("Span:{:?}", pair.as_span());
    println!("Text:{}", pair.as_str());
  }
}

