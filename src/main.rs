use std::{
  fs, 
  env, 
  path::Path,
  collections::HashMap,
  mem::discriminant
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
                  "any" => Value::Any,
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

#[derive(Clone)]
struct Compiler {
  functions: HashMap<String, Token>
}

fn compile(mut compiler: Compiler, mut input: Vec<Token>, mut count: usize, variables: Option<HashMap<String, Token>>) {
  let mut variables = variables.unwrap_or_else(HashMap::new);
  while input.get(count).is_some() {
    let token = input.get(count).unwrap();
    match token.clone().kind {
      TokenKind::Module { tokens, path: _ } => {
        input.extend(tokens);
      },
      TokenKind::Function { name, args: _, scope: _, rettype: _ } => {
        compiler.functions.insert(name.clone(), input.get(count).unwrap().clone());
      },
      TokenKind::Variable {mutable} => {
        match input.get(count + 1).unwrap().kind {
          TokenKind::Identifier => {
            if variables.contains_key(&input.get(count + 1).unwrap().value) && !mutable{
              panic!("Unmutable variable but changed")
            } else {
              variables.insert(input.get(count + 1).unwrap().value.clone(), token.clone());
            }
          },
          _ => {
            panic!("CompileError but like SyntaxError")
          }
        }
      },
      TokenKind::FunctionCall(args) => {
        let token = input.get(count).unwrap();
        let function = compiler.functions.get(&token.value).expect("no function found");

        if let TokenKind::Function{ name: _, args: arguments, scope, rettype } = &function.kind {
          let scoper = if let TokenKind::Scope(scope) = scope.kind.clone() {
            scope
          } else {
            panic!("No scope")
          };
          let mut realargs = HashMap::new();
          if arguments.len() != args.len() { panic!("Arguments not equal") }
          for (i, (i2, i3)) in args.iter().zip(arguments.iter()) {
            match i3 {
              &Value::Any => { },
              &Value::Float => {
                if discriminant(&i.kind) == discriminant(&TokenKind::Float(0.0)) {
                  panic!("No return float")
                }
              },
              &Value::Void => {
                panic!("No void argument")
              },
              &Value::String => {
                if discriminant(&i.kind) == discriminant(&TokenKind::String("".to_string())) {
                  panic!("No return string")
                }
              },
              &Value::Integer => {
                if discriminant(&i.kind) == discriminant(&TokenKind::Integer(0)) {
                  panic!("No return integer")
                }
              }
            }
            realargs.insert(i2.clone(), i.clone());
          }
          compile(compiler.clone(), scoper, 0, Some(realargs));
          match rettype {
            &Value::Any => { },
            &Value::Float => {
              if let TokenKind::Scope(a) = &scope.kind {
                if discriminant(&a.iter().filter(
                  |x| discriminant(&x.kind) == discriminant(&TokenKind::End(Box::new(Token::default()))) )
                .next().unwrap().kind) == discriminant(&TokenKind::Float(0.0)) {
                  panic!("No return float")
                }
              }
            },
            &Value::Void => {
              panic!("No void argument")
            },
            &Value::String => {
              if let TokenKind::Scope(a) = &scope.kind {
                if discriminant(&a.iter().filter(
                  |x| discriminant(&x.kind) == discriminant(&TokenKind::End(Box::new(Token::default()))) )
                .next().unwrap().kind) == discriminant(&TokenKind::String("".to_string())) {
                  panic!("No return string")
                }
              }
            },
            &Value::Integer => {
              if let TokenKind::Scope(a) = &scope.kind {
                if discriminant(&a.iter().filter(
                  |x| discriminant(&x.kind) == discriminant(&TokenKind::End(Box::new(Token::default()))) )
                .next().unwrap().kind) == discriminant(&TokenKind::Integer(0)) {
                  panic!("No return int")
                }
              }
            }
          }
        }
      },
      TokenKind::VariableCall => {
        input.insert(count, variables.iter().filter(|(x, _)| x == &&token.value).next().unwrap().1.clone());
        input.remove(count + 1);
        count -= 1;
      }
      _ => { todo!(); }
    }
    count += 1;
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

  let main_func = contextlol.iter().find(|x| { 
    if let TokenKind::Function {name, args: _, scope: _, rettype: _} = &x.kind {
      name == "main"
    } else {
      false
    }
  }).unwrap();

  println!("{:#?}", main_func);
}
