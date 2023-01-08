extern crate pest;
#[macro_use]
extern crate pest_derive;

// use inkwell::context::Context;
use pest::Parser;
use std::env::args;
use tokio::fs::read_to_string;
use fraction::{DynaInt, DynaDecimal};

#[derive(Parser)]
#[grammar = "syntax.pest"]
struct IdentParser;

pub enum Operator { Equal, NotEqual, And, Or }
pub enum Types { String(String), Float(DynaDecimal<usize, u8>), Int(DynaInt<u32, u64>), Bool(bool) }
pub enum TypeOrAstTree { Type(Types), AstNode(AstTree) }
pub enum Cond { 
  Bool(Types),
  Variable(String), 
  Statements {
    statement1: Box<AstTree>, 
    operator: Operator, 
    statement2: Box<AstTree>
  } 
}
pub enum AstTree {
  Function {name: Box<AstTree>, scope: Box<AstTree>},
  FunctionCall {name: Box<AstTree>, args: Box<Vec<TypeOrAstTree>>},
  Scope {summary: Box<AstTree>},
  ScopeSummary {statements: Box<Vec<AstTree>>},
  Arg {name: Box<AstTree>, argtype: Types},
  VariableImmu {variable: Box<AstTree>},
  VariableMu {variable: Box<AstTree>},
  Variable {name: Box<AstTree>, statement: Box<AstTree>},
  RepeatWhile {cond: Box<AstTree>},
  Cond {cond: Cond},
  Operator {op: Operator},
  Types {t: Types},
  LambdaFun {scope: Box<AstTree>},
  End {scope: Box<AstTree>},
  Name {name: String},
  Statement {st: Box<AstTree>}
}

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

