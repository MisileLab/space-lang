use inkwell::{
  context::Context, 
  OptimizationLevel, module::Module
};

use std::{
  fs, 
  env, 
  path::Path,
  collections::HashMap,
  mem::discriminant,
  any::Any
};

#[derive(Debug)]
pub struct Lexer { pub contents: Vec<char>, path: String }

#[derive(Debug, PartialEq, Clone)]
enum TokenKind {
  None,
  VariableCall,
  Module{ tokens: Vec<Token>, path: String },
  Function{ name: String, args: HashMap<String, Value>, scope: Box<Token>, rettype: Value },
  FunctionCall(Vec<Token>),
  Scope(Vec<Token>),
  String(String),
  End(Box<Token>),
  Return(Value),
  Assign,
  Variable{ mutable: bool },
  Identifier,
  Integer(isize),
  Float(f64),
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
  Void
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
  pub fn lex(&mut self, mut count: usize, limit: usize, variables: Option<Vec<Token>>) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::new();
    for i in variables.unwrap_or(Vec::new()).into_iter() {
      tokens.push(i)
    }

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
        '=' => {
          if discriminant(&get_token_kind(&tokens)) == discriminant(&TokenKind::Variable { mutable: false }) {
            tokens.push( Token { kind: TokenKind::Assign, value: "=".to_string() } );
            count += 1;
          } else {
            count += 1;
            if self.current_char(count) == '=' {
              tokens.push( Token { kind: TokenKind::Condition(Cond::Equal), value: "==".to_string() } );
            }
          }
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
          count += 1;
          
          let mut buffer = String::new();

          while self.current_char(count) != '}' {
            buffer.push(self.current_char(count));
            count += 1;
          }

          tokens.push( Token { kind: TokenKind::Scope(self.lex(count - buffer.len(), count, None)), value: buffer} )
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

              println!("{:#?}", &argname);

              Some(TokenKind::End(Box::new(Token { 
                kind: TokenKind::End(Box::new(self.lex(count - argname.len(), count, Some(
                  tokens.clone().into_iter().filter(|x| x.kind == TokenKind::Identifier).collect::<Vec<Token>>()
                )).into_iter().last().unwrap())), value: buffer.clone()})))
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
                        "void" => Value::Void,
                        _ => panic!("SyntaxError {}", buffer.clone())
                      };

                      args.insert(name, value);
                      buffer.clear();
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

              self.lex(count - buffer3.len(), count, None); // this is infinite loop

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
                  _ => panic!("SyntaxError {}", buffer.clone())
                }
              })
            },
            "if" => Some(TokenKind::If),
            "and" => Some(TokenKind::Condition(Cond::And)),
            "or" => Some(TokenKind::Condition(Cond::Or)),
            "import" => {
              let mut file = String::new();
              while self.current_char(count).is_alphabetic() || ((self.current_char(count) == '/' || self.current_char(count) == '.') && self.current_char(count + 1).is_alphabetic()) || self.current_char(count) == ' ' {
                if self.current_char(count) != ' ' { file.push(self.current_char(count)); }
                count += 1;
              }
              let path = Path::new(&format!("{}/{}", self.path, &file)).to_str().unwrap().to_string();
              let contents = fs::read_to_string(&path).expect(&format!("Couldn't read this file, result is {}", file));
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
                let mut args = Vec::new();
                while self.current_char(count) != ')' {
                  if self.current_char(count + 1) == ')' { buffer2.push(self.current_char(count)); }
                  if self.current_char(count) == ',' || self.current_char(count + 1) == ')' {
                    if buffer2.chars().last().unwrap() == '\"' && buffer2.chars().next().unwrap() == '\"' {
                      args.push(Token { kind: TokenKind::String(remove_first_and_last(buffer2.clone())), value: buffer2.clone() });
                      buffer2.clear();
                    } else if buffer2.chars().find(|x| !x.is_numeric()).is_none() && buffer2.chars().find(|x| x == &'.').is_some() {
                      args.push(Token { kind: TokenKind::Float(buffer2.clone().parse().unwrap()), value: buffer2.clone() });
                      buffer2.clear();
                    } else if buffer2.chars().find(|x| !x.is_numeric()).is_none() {
                      args.push(Token { kind: TokenKind::Integer(buffer2.clone().parse().unwrap()), value: buffer2.clone() });
                      buffer2.clear();
                    }
                  } else {
                    buffer2.push(self.current_char(count));
                  }
                  count += 1;
                }
                Some(TokenKind::FunctionCall(args))
              } else { None }
            }
          };
          
          if let Some(k) = kind { 
            tokens.push(Token { kind: k, value: buffer }); 
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

struct Compiler<'a> {
  context: Context,
  module: HashMap<String, Module<'a>>
}

impl Compiler<'_> { 
  fn compile(&self, input: Vec<Token>, mut count: usize, variables: Option<HashMap<String, Token>>, bit: u8) {
    let mut variables = variables.unwrap_or_else(HashMap::new);
    let mut content = String::new();
    while input.get(count).is_some() {
      let token = input.get(count).unwrap();
      match token.clone().kind {
        TokenKind::Module { tokens, path: _ } => {
          self.compile(tokens, count, None);
        },
        TokenKind::Function { name, args, scope: _, rettype } => {
          let mut arguments = Vec::new();
          for (i, i2) in args.into_iter() {
            arguments.push(i2.clone());
          }
          let ret: Any = match rettype {
            Value::Void => {
              self.context.void_type()
            },
            Value::String => {
              vec![];
            },
            Value::Float => {
              self.context.f64_type()
            },
            Value::Integer => {
              self.context.i128_type()
            }
            _ => { todo!() }
          };
        },
        TokenKind::Variable {mutable: _} => {
          match input.get(count + 1).unwrap().kind {
            TokenKind::Identifier => {
              variables.insert(input.get(count + 1).unwrap().value.clone(), token.clone());
            },
            _ => {
              panic!("CompileError but like SyntaxError")
            }
          }
        },
        _ => { todo!(); }
      }
      count += 1;
    }
  }
}

fn get_modules(input: &[Token], modules: &mut Vec<Token>) {
  for i in input.iter() {
    if let TokenKind::Module { tokens, path: _} = &i.kind {
      if modules.iter().find(|x| x == &i).is_none() {
        modules.push(i.clone());
        get_modules(tokens, modules);
      }
    }
  }
}

fn remove_first_and_last(value: String) -> String {
  let mut chars = value.chars();
  let mut buffer = String::new();
  chars.next();
  chars.next_back();
  chars.into_iter().for_each(|x| buffer.push(x));
  buffer
}

#[allow(clippy::expect_fun_call)]
fn main() {
  let file = env::args().nth(1).unwrap();
  let release = true;
  let path = Path::new(&file).parent().unwrap().to_str().unwrap().to_string();
  let contents = fs::read_to_string(&file).expect(&format!("Couldn't read this file, result is {}", file));

  let mut lexer = Lexer::new(contents, path);
  let contextlol = lexer.lex(0, lexer.contents.len(), None);
  let mut modules: Vec<Token> = Vec::new();
  println!("{:#?}", &contextlol);
  get_modules(&contextlol, &mut modules);

  let level = if release {
    OptimizationLevel::Aggressive
  } else {
    OptimizationLevel::None
  };

  let context = Context::create();
  let mut module = HashMap::new();
  module.insert(file.clone(), context.create_module(&file));

  for i in modules {
    if let TokenKind::Module { tokens: _, path } = i.kind {
      module.insert(file.clone(), context.create_module(&path));
    }
  }

  let main_func = contextlol.iter().find(|x| { 
    if let TokenKind::Function {name, args: _, scope: _, rettype: _} = &x.kind {
      name == "main"
    } else {
      false
    }
  }).unwrap();

  println!("{:#?}", main_func);
}
