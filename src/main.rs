#![feature(let_else)]

use std::{
  fs, 
  env, 
  path::Path,
  collections::HashMap,
  mem::discriminant,
  any::Any,
  ptr::read
};

use evcxr::EvalContext;

#[derive(Debug)]
pub struct Lexer { pub contents: Vec<char>, path: String }

#[derive(Debug, PartialEq, Clone)]
enum TokenKind {
  None,
  Boolean(bool),
  VariableCall,
  Module{ tokens: Vec<Token>, path: String },
  Function{ name: String, args: HashMap<String, Value>, scope: Box<Token>, rettype: Value },
  FunctionCall(Vec<Token>),
  Scope(Vec<Token>),
  String(String),
  End(Box<Token>),
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
  Void,
  Boolean,
  Any
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
                        "void" => panic!("No void argument"),
                        "boolean" => Value::Boolean,
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
                  "any" => Value::Any,
                  _ => panic!("SyntaxError {}", buffer.clone())
                }
              })
            },
            "if" => Some(TokenKind::If),
            "and" => Some(TokenKind::Condition(Cond::And)),
            "or" => Some(TokenKind::Condition(Cond::Or)),
            "true" => Some(TokenKind::Boolean(true)),
            "false" => Some(TokenKind::Boolean(false)),
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

fn check(mut input: Vec<Token>, mut count: usize, variables: Option<HashMap<String, Token>>) -> Vec<Token>{
  let mut variables = variables.unwrap_or_else(HashMap::new);
  while input.get(count).is_some() {
    match input.get(count).clone().unwrap().kind.clone() {
      TokenKind::Module { tokens, path: _ } => {
        let mut other = tokens.clone();
        other.extend(input.clone());
        input = other;
      },
      TokenKind::Variable {mutable: _} => {
        match input.get(count + 1).unwrap().kind {
          TokenKind::Identifier => {
            if variables.iter().filter(
              |(x, y)| input.get(count + 1).unwrap().value == **x && if let TokenKind::Variable { mutable } = y.kind {
                !mutable
              } else {
                false
              }
            ).next().is_some() {
              panic!("Unmutable variable but changed")
            } else {
              variables.insert(input.get(count + 1).unwrap().value.clone(), input.get(count).unwrap().clone());
            }
          },
          _ => {
            panic!("CompileError but like SyntaxError")
          }
        }
      },
      _ => {  }
    }
    count += 1;
  }
  input
}

fn eval(
  mut input: Vec<Token>, 
  parent: Option<&mut Vec<Token>>,
  mut variables: HashMap<String, Box<dyn Any>>, 
  mut functions: HashMap<String, (HashMap<String, Value>, Box<Token>, Value)>, 
  mut count: usize
) -> Vec<Token> {
  while input.get(count).is_some() {
    let r1 = &mut variables as *mut HashMap<String, Box<dyn Any>>;
    let r2 = &mut functions as *mut HashMap<String, (HashMap<String, Value>, Box<Token>, Value)>;
    match &input.get(count).unwrap().kind.clone() {
      TokenKind::Variable{mutable: _} => {
        count += 2;
        unsafe { 
          eval(vec![input.get(count).unwrap().clone()], Some(&mut input), read(r1), read(r2), 0)
        };
        variables.insert(input.get(count - 1).unwrap().value.clone(), Box::new(input.get(count).unwrap().kind.clone()));
      },
      TokenKind::Condition(cond) => {
        unsafe {
          eval(vec![input.get(count + 1).unwrap().clone()], Some(&mut input), read(r1), read(r2), 0)
        };
        let TokenKind::Boolean(cond1) = &input[count - 1].kind else { panic!("No boolean") };
        let TokenKind::Boolean(cond2) = &input[count + 1].kind else { panic!("No boolean") };
        let boolean = match cond {
          &Cond::And => { *cond1 && *cond2 },
          &Cond::Equal => { *cond1 == *cond2 },
          &Cond::NotEqual => { *cond1 != *cond2 },
          &Cond::Or => { *cond1 || *cond2 }
        };
        input.remove(count - 1);
        input.remove(count);
        input.remove(count + 1);
        input.insert(count, Token { kind: TokenKind::Boolean(boolean), value: input.get(count).unwrap().value.clone() } )
      },
      TokenKind::Function{ name: _, args: _, scope: _, rettype: _ } => {
        let TokenKind::Function{ name, args,scope, rettype } = input.get(count).unwrap().kind.clone() else { unreachable!() };
        functions.insert(name.clone(), (args, scope, rettype));
      },
      TokenKind::FunctionCall(a) => {
        match input.get(count).unwrap().value.clone().as_str() {
          "eval_rust" => {
            let (mut context, _) = EvalContext::new().unwrap();
            context.eval("use std::any::Any;").unwrap();
            context.eval(format!("let mut variables: HashMap<String, Box<dyn Any>> = {:?};", variables).as_str()).unwrap();
            for i in a.iter() {
              if let TokenKind::String(v) = &i.kind {
                let c = context.eval(v);
                println!("{:#?}", c);
              } else {
                panic!("No string")
              }
            }
          },
          "drop" => {
            variables.remove(&a[0].value);
          },
          "add_variable" => {
            variables.insert(a[0].value.clone(), Box::new(a[1].clone()));
          },
          _ => {
            if functions.contains_key(&input[0].value) {
              let function = functions.get(&input[0].value).unwrap();
              let mut variables = HashMap::new();
              for (i, (k, _ )) in function.0.iter().enumerate() {
                variables.insert(k, a[i].clone());
              }
              count += 1;
              let TokenKind::Scope(scope) = input[count].kind.clone() else { panic!("No scope") };
              unsafe {
                if let TokenKind::End(end) = eval(
                  scope, 
                  None, 
                  read(r1), 
                  read(r2), 
                  0
                ).iter().last().unwrap().clone().kind {
                  input.remove(count);
                  count -= 1;
                  input.remove(count);
                  if (function.2 == Value::Any || (function.2 == Value::Boolean && discriminant(&end.kind) == discriminant(&TokenKind::Boolean(true)))) || 
                    (function.2 == Value::Float && (discriminant(&end.kind) == discriminant(&TokenKind::Float(0.)))) ||
                    (function.2 == Value::Integer && (discriminant(&end.kind) == discriminant(&TokenKind::Integer(0)))) ||
                    (function.2 == Value::String && (discriminant(&end.kind) == discriminant(&TokenKind::String("".to_string())))) || function.2 != Value::Void {
                    input.insert(count, *end)
                  } else {
                    panic!("No equal return type")
                  }
                } else { 
                  if function.2 != Value::Void || function.2 != Value::Any {
                    panic!("No end but function return type not void")
                  }
                };
              }
            } else {
              panic!("No function found")
            }
          }
        }
      }
      _ => todo!()
    }
  }
  input
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
  evcxr::runtime_hook();
  let file = env::args().nth(1).unwrap();
  let release = true;
  let path = Path::new(&file).parent().unwrap().to_str().unwrap().to_string();
  let contents = fs::read_to_string(&file).expect(&format!("Couldn't read this file, result is {}", file));

  let mut lexer = Lexer::new(contents, path);
  let contextlol = lexer.lex(0, lexer.contents.len(), None);
  let mut modules: Vec<Token> = Vec::new();
  println!("{:#?}", &contextlol);
  get_modules(&contextlol, &mut modules);

  let main_func = contextlol.iter().find(|x| { 
    if let TokenKind::Function {name, args: _, scope: _, rettype: _} = &x.kind {
      name == "main"
    } else {
      false
    }
  }).unwrap();

  println!("{:#?}", main_func);
}
