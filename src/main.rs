#![feature(allocator_api)]

use std::{
  fs, 
  env, 
  path::Path,
  collections::HashMap,
  mem::discriminant,
  any::Any,
  iter::zip
};

use inkwell::OptimizationLevel;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction, UnsafeFunctionPointer};
use inkwell::module::Module;
use inkwell::types::{BasicTypeEnum, VoidType, BasicType, BasicMetadataTypeEnum};
use inkwell::values::{FloatValue, IntValue, StructValue};

#[derive(Debug)]
pub struct Lexer { pub contents: Vec<char>, path: String }

#[derive(Debug, PartialEq, Clone)]
enum TokenKind {
  None,
  Newline,
  List,
  Dict,
  Boolean(bool),
  VariableCall,
  Module{ tokens: Vec<Token>, path: String },
  Function{ name: String, args: HashMap<String, Value>, scope: Box<Token>, rettype: Value },
  FunctionCall(Vec<Value>),
  Scope(Vec<Token>),
  String(String),
  End(String),
  Assign,
  Variable{ mutable: bool },
  Identifier,
  Integer(String),
  Float(String),
  If,
  Condition(Cond)
}

#[derive(Debug, PartialEq, Clone)]
enum Cond {
  Equal,
  NotEqual,
  Or,
  And
}

#[derive(Debug, PartialEq, Clone)]
enum Value {
  Integer,
  Float,
  String,
  Void,
  Boolean,
  List,
  Dict,
  Any
}

#[derive(Debug)]
enum ActualValue<'a> {
  Integer(IntValue<'a>),
  Float(FloatValue<'a>),
  String(StructValue<'a>),
  Boolean(bool),
  List(Vec<Box<dyn Any>>),
  Dict(HashMap<Box<dyn Any>, Box<dyn Any>>)
}

#[derive(Debug, Clone)]
enum BasicTypeEnumExtended<'a> {
  BasicEnum(BasicTypeEnum<'a>),
  Void(VoidType<'a>)
}

impl Default for TokenKind {
    fn default() -> Self {
      TokenKind::None
    }
}

#[derive(Debug, PartialEq, Default, Clone)]
pub struct Token { kind: TokenKind, value: String }

impl Lexer {
  pub fn new(contents: String, path: String) -> Self { Self { contents: contents.chars().collect(), path } }
  #[allow(clippy::expect_fun_call)]
  pub fn get_counts(&self, limit: usize, mut count: usize) -> HashMap<usize, usize>{
    let mut scoper = HashMap::new();
    while limit > count {
      if self.current_char(count) == '{' {
        let counter = count;
        count += 1;
        while self.current_char(count) != '}' {
          if cfg!(debug_assertions) {
            println!("{}: {}", count, self.current_char(count))
          }
          if self.current_char(count) == '{' {
            scoper.extend(self.get_counts(limit, count));
          }
          count += 1;
        }
        scoper.insert(counter, count);
      }
      count += 1;
    }
    scoper
  }
  pub fn lex(&mut self, mut count: usize, limit: usize, variables: Option<Vec<Token>>) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::new();
    for i in variables.clone().unwrap_or_default().into_iter() {
      tokens.push(i)
    }

    let scoper: HashMap<usize, usize> = self.get_counts(limit, count);

    while limit > count {
      let c = self.current_char(count);

      match c {
        '/' => {
          count += 1;
          if self.current_char(count) == '/' {
            while self.current_char(count) != '\n' {
              count += 1;
            }
          } else if self.current_char(count) == '*' {
            while self.current_char(count) != '*' || self.current_char(count+1) != '/' {
              count += 1;
            }
          } else if self.current_char(count) != '\n' {
            panic!("SyntaxError");
          }
        },
        '\n' => {
          tokens.push( Token { kind: TokenKind::Newline, value: "\n".to_string() } );
          count += 1;
        },
        '=' => {
          count += 1;
          if self.current_char(count) != '=' {
            tokens.push( Token { kind: TokenKind::Assign, value: "=".to_string() } );
          } else {
            tokens.push( Token { kind: TokenKind::Condition(Cond::Equal), value: "==".to_string()} );
          }
          count += 1;
        },
        '!' => {
          count += 1;
          if self.current_char(count) == '=' {
            tokens.push( Token { kind: TokenKind::Condition(Cond::NotEqual), value: "!=".to_string() } )
          }
        },
        '\'' | '\"' => {
          count += 1;

          let mut buffer = String::new();

          while self.current_char(count) != c {
            buffer.push(self.current_char(count));
            count += 1;
          }

          tokens.push( Token { kind: TokenKind::String(buffer.clone()), value: buffer});

          count += 1;
        },
        '{' => {
          let close = scoper.get(&count).unwrap();
          count += 1;
          
          let mut buffer = String::new();

          while count != *close {
            buffer.push(self.current_char(count));
            count += 1;
          }

          tokens.push( Token { kind: TokenKind::Scope(self.lex(count - buffer.len(), count, variables.clone())), value: buffer} )
        },
        _ if c.is_numeric() => {
          let mut buffer = String::new();
          buffer.push(c);
          
          count += 1;

          loop {
            if !self.current_char(count).is_numeric() { break; }

            buffer.push(self.current_char(count));
            count += 1;
          }

          if self.current_char(count) == '.' {
            buffer.push('.');
            count += 1;
            loop {
              if !self.current_char(count).is_numeric() { break; }
              
              buffer.push(self.current_char(count));
              count += 1;
            }
            tokens.push(Token { kind: TokenKind::Float(buffer.clone().parse().unwrap()), value: buffer });
          } else {
            tokens.push(Token { kind: TokenKind::Integer(buffer.clone().parse().unwrap()), value: buffer });
          }
        }
        _ if c.is_alphabetic() => {
          let mut buffer = String::new();

          buffer.push(c);
          
          count += 1;

          while self.current_char(count).is_alphabetic() {
            buffer.push(self.current_char(count));
            count += 1;
          }

          let kind: Option<TokenKind> = match buffer.as_str() {
            "unmut" => Some(TokenKind::Variable { mutable: false }),
            "mut" => Some(TokenKind::Variable { mutable: true } ),
            "end" => {
              buffer.clear();
              count += 1;
              let mut argname = String::new();

              while self.current_char(count).is_alphabetic() || self.current_char(count).is_numeric() {
                argname.push(self.current_char(count));
                count += 1;
              }

              Some(TokenKind::End(argname))
            },
            "fun" => {
              buffer.clear();
              let mut args: HashMap<String, Value> = HashMap::new();
              let mut fname = String::new();
              while self.current_char(count) != '(' {
                if self.current_char(count) != ' ' { fname.push(self.current_char(count)); }
                count += 1;
              }
              count += 1;
              while self.current_char(count) != ')' {

                if self.current_char(count) != '\n' { 
                  if self.current_char_no_space(count) == ':' {
                    let name: String = buffer.clone().chars().filter(|x| x != &' ').collect();
                    buffer.clear();
                    count += 1;
  
                    while self.current_char(count) != ')' && self.current_char(count) != ',' {
                      if self.current_char(count) != ' ' { buffer.push(self.current_char(count)); }
                      count += 1;
                    }
  
                    if !name.is_empty() && !buffer.is_empty() { 
                      let value = match buffer.as_str().chars().filter(|x| x != &' ').collect::<String>().as_str() {
                        "integer" => Value::Integer,
                        "float" => Value::Float,
                        "string" => Value::String,
                        "void" => panic!("No void argument"),
                        "boolean" => Value::Boolean,
                        "list" => Value::List,
                        "dict" => Value::Dict,
                        _ => panic!("SyntaxError {}", buffer.clone())
                      };

                      args.insert(name, value);
                    }
                  } else { 
                    if self.current_char(count) != ',' || self.current_char(count) == '('{ buffer.push(self.current_char(count)); }
                    count += 1;
                  }
                } else {
                  count += 1;
                }
              }
              count += 1;

              let mut buffer2 = String::new();
              
              while buffer2.as_str() != "return" {
                if self.current_char(count) != ' ' { buffer2.push(self.current_char(count)); }
                count += 1;
              }

              count += 1;
              buffer2.clear();

              while self.current_char(count) != ' ' && self.current_char(count) != '\n' {
                buffer2.push(self.current_char(count));
                count += 1;
              }

              while self.current_char(count) == '{' || self.current_char(count) == '\n' {
                count += 1;
              }

              let mut buffer3 = String::new();
    
              while self.current_char(count) != '}' {
                if self.current_char(count) != '{' { buffer3.push(self.current_char(count)); }
                count += 1;
              }

              self.lex(count - buffer3.len(), count, None);

              buffer = buffer3.clone();

              Some(TokenKind::Function {
                name: fname, 
                args, 
                scope: Box::new(Token {kind: TokenKind::Scope(self.lex(count - buffer3.len(), count, None)), 
                  value: buffer3
                }),
                rettype: match buffer2.as_str() {
                  "integer" => Value::Integer,
                  "float" => Value::Float,
                  "string" => Value::String,
                  "void" => Value::Void,
                  "any" => Value::Any,
                  "list" => Value::List,
                  "dict" => Value::Dict,
                  _ => panic!("SyntaxError {}", buffer2.clone())
                }
              })
            },
            "if" => Some(TokenKind::If),
            "and" => Some(TokenKind::Condition(Cond::And)),
            "or" => Some(TokenKind::Condition(Cond::Or)),
            "true" => Some(TokenKind::Boolean(true)),
            "false" => Some(TokenKind::Boolean(false)),
            "list" => Some(TokenKind::List),
            "dict" => Some(TokenKind::Dict),
            "import" => {
              let mut file = String::new();
              while self.current_char(count).is_alphabetic() || ((self.current_char(count) == '/' || self.current_char(count) == '.') && self.current_char(count + 1).is_alphabetic()) || self.current_char(count) == ' ' {
                if self.current_char(count) != ' ' { file.push(self.current_char(count)); }
                count += 1;
              }
              let path = Path::new(&format!("{}/{}", self.path, &file)).to_str().unwrap().to_string();
              let contents = fs::read_to_string(&path).unwrap_or_else(|_| panic!("Couldn't read this file, result is {}", file));
              Some(TokenKind::Module{tokens: Lexer::new(contents.clone(), self.path.clone()).lex(0, contents.len(), None), path})
            }
            a if tokens.clone().into_iter().any(|x| (x.kind == TokenKind::Identifier && x.value == a)) => {
              Some(TokenKind::VariableCall)
            },
            _ => {
              if discriminant(&get_token_kind(&tokens)) == discriminant(&TokenKind::Variable { mutable: false }) {
                Some(TokenKind::Identifier)
              } else if self.current_char_no_space(count) == '(' {
                count += 1;
                let mut buffer2 = String::new();
                if tokens.iter().last().clone().is_some() {
                  let kind = tokens.iter().last().unwrap().kind.clone();
                  if kind != TokenKind::Newline && (
                    kind != TokenKind::Assign && discriminant(&kind) != discriminant(&TokenKind::Condition(Cond::Equal)) && 
                    discriminant(&kind) != discriminant(&TokenKind::End("".to_string())) && kind != TokenKind::Identifier &&
                    kind != TokenKind::If && discriminant(&kind) != discriminant(&TokenKind::Module{tokens: Vec::new(), path: "".to_string()}) &&
                    kind != TokenKind::None
                  ) {
                    buffer2.push_str((tokens.iter().last().unwrap().value.clone() + ".").as_str());
                  }
                }
                let mut args = Vec::new();
                while self.current_char(count) != ')' {
                  let originalcount = count;
                  while self.current_char(count) != ',' {
                    count += 1;
                  }
                  let _org = match self.lex(originalcount, count, variables.clone())[0].kind {
                    TokenKind::Boolean(_) => Value::Boolean,
                    TokenKind::Dict => Value::Dict,
                    TokenKind::Float(_) => Value::Float,
                    TokenKind::Integer(_) => Value::Integer,
                    TokenKind::List => Value::List,
                    TokenKind::String(_) => Value::String,
                    _ => unreachable!()
                  };
                  args.push(_org);
                  count += 1;
                }
                Some(TokenKind::FunctionCall(args))
              } else { None }
            }
          };
          
          if let Some(k) = kind { 
            tokens.push(Token { kind: k, value: buffer }); 
          } else {
            tokens.push(Token { kind: TokenKind::Identifier, value: buffer} );
          }
        },
        _ => {
          count += 1;
        }
      }
    }
    tokens
  }

  fn current_char(&self, counter: usize) -> char { *self.contents.get(counter).unwrap() }
  fn current_char_no_space(&self, mut counter: usize) -> char { 
    while self.current_char(counter) == ' ' || self.current_char(counter) == '\n' {
      counter -= 1;
    }
    self.current_char(counter)
  }
}

fn get_token_kind(tokens: &[Token]) -> TokenKind {
  tokens.last().unwrap_or(&Token::default()).kind.clone()
}

struct CodeGen<'ctx> {
  context: &'ctx Context,
  module: Module<'ctx>,
  builder: Builder<'ctx>,
  execution_engine: ExecutionEngine<'ctx>,
}

type MainFun = unsafe extern "C" fn();
type Anyfunc = unsafe extern "C" fn (Option<Value>, Option<Value>, Option<Value>, Option<Value>, Option<Value>, Option<Value>, Option<Value>, Option<Value>, Option<Value>) -> Option<Value>;

impl<'ctx> CodeGen<'ctx> {
  fn compile(&self, tokens: Vec<Token>) -> Option<JitFunction<MainFun>> {
    let mut i: usize = 0;
    while tokens.clone().len() >= i {
      match tokens[i].clone().kind {
        TokenKind::None => {},
        TokenKind::Newline => {},
        TokenKind::List => {panic!("No list")},
        TokenKind::Dict => {panic!("No dict")},
        TokenKind::Boolean(_) => panic!("No bool"),
        TokenKind::VariableCall => todo!(),
        TokenKind::Module { tokens: _tokens, path: _ } => {self.compile(_tokens);},
        TokenKind::Function { name, args, scope, rettype } => {
          let mut args2 = Vec::new();
          let mut args3 = Vec::new();
          for (i, i2) in args {
            let typer: BasicMetadataTypeEnum = match i2 {
              Value::Any => {unimplemented!()},
              Value::Boolean => BasicMetadataTypeEnum::IntType(self.context.bool_type()),
              Value::Dict => {unimplemented!()},
              Value::List => {unimplemented!()},
              Value::Float => BasicMetadataTypeEnum::FloatType(self.context.f64_type()),
              Value::Integer => BasicMetadataTypeEnum::IntType(self.context.i64_type()),
              Value::String => {
                let mut strs: Vec<BasicTypeEnum> = Vec::new();
                for _ in 0..20 {
                  strs.push(BasicTypeEnum::IntType(self.context.i8_type()))
                }
                BasicMetadataTypeEnum::StructType(self.context.struct_type(&strs, false))
              },
              Value::Void => {panic!("No void type in function")}
            };
            args2.push(typer);
            args3.push(i);
          };

          let ret: BasicTypeEnumExtended = match rettype {
            Value::Any => {unimplemented!()},
            Value::Boolean => BasicTypeEnumExtended::BasicEnum(BasicTypeEnum::IntType(self.context.bool_type())),
            Value::Dict => {unimplemented!()},
            Value::List => {unimplemented!()},
            Value::Float => BasicTypeEnumExtended::BasicEnum(BasicTypeEnum::FloatType(self.context.f64_type())),
            Value::Integer => BasicTypeEnumExtended::BasicEnum(BasicTypeEnum::IntType(self.context.i64_type())),
            Value::String => {
              let mut strs: Vec<BasicTypeEnum> = Vec::new();
              for _ in 0..20 {
                strs.push(BasicTypeEnum::IntType(self.context.i8_type()))
              }
              BasicTypeEnumExtended::BasicEnum(BasicTypeEnum::StructType(self.context.struct_type(&strs, false)))
            },
            Value::Void => BasicTypeEnumExtended::Void(self.context.void_type())
          };

          let fn_type = match ret {
            BasicTypeEnumExtended::BasicEnum(a) => a.fn_type(&args2[..], false),
            BasicTypeEnumExtended::Void(v) => v.fn_type(&args2[..], false)
          };
          let function = self.module.add_function(&name, fn_type, None);
          self.builder.position_at_end(self.context.append_basic_block(function, "entry"));

          let mut variables: HashMap<String, ActualValue> = HashMap::new();
          for ((i, i2), i3) in zip(args3.iter().enumerate(), args2) {
            let _temp = function.get_nth_param(i.try_into().unwrap()).unwrap();
            let temp: ActualValue = match i3 {
              BasicMetadataTypeEnum::FloatType(_) => ActualValue::Float(_temp.into_float_value()),
              BasicMetadataTypeEnum::IntType(_) => ActualValue::Integer(_temp.into_int_value()),
              BasicMetadataTypeEnum::StructType(_) => ActualValue::String(_temp.into_struct_value()),
              _ => { unimplemented!(); }
            };
            variables.insert(i2.to_string(), temp);
          }
        },
        TokenKind::FunctionCall(_args) => {
          let mut args: Vec<Option<Value>> = Vec::new();
          for i in 0..8 {
            if _args.get(i).unwrap_or(&Value::Void) == &Value::Void {
              args.push(None);
            } else {
              args.push(Some(_args[i].clone()));
            }
          }
          unsafe {
            let fnlol: JitFunction<Anyfunc> = self.execution_engine.get_function(&tokens[i].clone().value).unwrap(); 
            fnlol.call(
              args[0].clone(), 
              args[1].clone(), 
              args[2].clone(), 
              args[3].clone(), 
              args[4].clone(),
              args[5].clone(), 
              args[6].clone(),
              args[7].clone(),
              args[8].clone()
            );
          }
        },
        TokenKind::Scope(_) => todo!(),
        TokenKind::String(_) => todo!(),
        TokenKind::End(_) => todo!(),
        TokenKind::Assign => unreachable!(),
        TokenKind::Variable { mutable } => todo!(),
        TokenKind::Identifier => todo!(),
        TokenKind::Integer(_) => todo!(),
        TokenKind::Float(_) => todo!(),
        TokenKind::If => todo!(),
        TokenKind::Condition(_) => todo!(),
      }
      i += 1;
    }
    unsafe { self.execution_engine.get_function("main").ok() }
  }
}

#[allow(clippy::expect_fun_call)]
#[tokio::main]
async fn main() {
  let file = env::args().nth(1).unwrap();
  let path = Path::new(&file).parent().unwrap().to_str().unwrap().to_string();
  let contents = fs::read_to_string(&file).expect(&format!("Couldn't read this file, result is {}", file));

  let mut lexer = Lexer::new(contents, path);
  let lex = lexer.lex(0, lexer.contents.len(), None);
  println!("{:#?}", &lex);

  println!("Ready!");
}
