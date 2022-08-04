use inkwell::{
  context::Context, 
  OptimizationLevel
};

use std::{
  fs, 
  env, 
  path::Path,
  collections::HashMap,
  mem::discriminant
};

#[derive(Debug)]
struct Lexer { contents: Vec<char>, path: String }

#[derive(Debug, PartialEq, Clone)]
enum TokenKind {
  None,
  VariableCall,
  Module{ tokens: Vec<Token>, path: String },
  Function { name: String, args: HashMap<String, Value>, scope: Box<Token> },
  FunctionCall,
  Scope(Vec<Token>),
  String,
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
  String
}

impl Default for TokenKind {
    fn default() -> Self {
      TokenKind::None
    }
}

#[derive(Debug, PartialEq, Default, Clone)]
struct Token { kind: TokenKind, value: String }

impl Lexer {
  pub fn new(contents: String, path: String) -> Self { Self { contents: contents.chars().collect(), path } }
  #[allow(clippy::expect_fun_call)]
  pub fn lex(&mut self, mut count: usize, limit: usize) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::new();

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

          tokens.push( Token { kind: TokenKind::String, value: buffer});

          count += 1;
        },
        '{' => {
          count += 1;
          
          let mut buffer = String::new();

          while self.current_char(count) != '}' {
            buffer.push(self.current_char(count));
            count += 1;
          }

          tokens.push( Token {kind: TokenKind::Scope(self.lex(count - buffer.len(), count)), value: buffer} )
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
                      buffer.push(self.current_char(count));
                      count += 1;
                    }
  
                    if !name.is_empty() { 
                      let value = match buffer.as_str().chars().filter(|x| x != &' ').collect::<String>().as_str() {
                        "Integer" => Value::Integer,
                        "Float" => Value::Float,
                        "String" => Value::String,
                        _ => panic!("SyntaxError {}", buffer.clone())
                      };

                      args.insert(name, value);
                    }
                    buffer.clear();
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
    
              while self.current_char(count) != '}' {
                buffer2.push(self.current_char(count));
                count += 1;
              }
    
              Some(TokenKind::Function{ name: fname, args, scope: Box::new(Token {kind: TokenKind::Scope(self.lex(count - buffer.len(), count)), value: buffer2})})
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
              Some(TokenKind::Module{tokens: Lexer::new(contents.clone(), self.path.clone()).lex(0, contents.len()), path})
            }
            a if tokens.clone().into_iter().any(|x| (x.kind == TokenKind::Identifier && x.value == a)) => {
              Some(TokenKind::VariableCall)
            },
            _ => {
              if discriminant(&get_token_kind(&tokens)) == discriminant(&TokenKind::Variable { mutable: false }) {
                Some(TokenKind::Identifier)
              } else if self.current_char_no_space(count) == '(' {
                Some(TokenKind::FunctionCall)
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

struct Compiler {
  context: Context,
  functions: Vec<Token>
}

impl Compiler { 
  fn compile(&self, input: Box<Vec<Token>>, count: usize, variables: Option<HashMap<String, Token>>) {
    let mut variables = variables.unwrap_or_else(|| HashMap::new());
    let mut functions: HashMap<String, Token> = HashMap::new();
    let mut content = String::new();
    while input.get(count).is_some() {
      let i = input.get(count).unwrap();
      match i.clone().kind {
        TokenKind::Module { tokens, path } => {
          self.compile(Box::new(tokens), count, None);
        } ,
        TokenKind::Function { name, args, scope } => {
          let temp = name;
          if self.functions.iter().filter(
            |x| if let TokenKind::Function { name, args, scope } = &x.kind {
              name == &temp
            } else {
              false
            }
          ).next().is_none() {
            functions.insert(temp, i.clone());
          } else {
            panic!("Duplicate function")
          }
        },
        TokenKind::FunctionCall => {
          if self.functions.iter().filter(
            |x| if let TokenKind::Function { name, args, scope } = &x.kind {
              name == &i.value
            } else {
              false
            }
          ).next().is_some() {
            todo!();
          } else {
            panic!("No function");
          }
        },
        _ => { todo!(); }
      }
    }
  }
}

fn get_modules(input: &Vec<Token>, modules: &mut Vec<Token>) {
  for i in input.iter() {
    if let TokenKind::Module { tokens, path } = &i.kind {
      if modules.iter().filter(|x| x == &i).next().is_none() {
        modules.push(i.clone());
        get_modules(&tokens, modules);
      }
    }
  }
}

#[allow(clippy::expect_fun_call)]
fn main() {
  let file = env::args().nth(1).unwrap();
  let release = env::args().nth(2).unwrap_or("true".to_string()).parse::<bool>().unwrap();
  let path = Path::new(&file).parent().unwrap().to_str().unwrap().to_string();
  let contents = fs::read_to_string(&file).expect(&format!("Couldn't read this file, result is {}", file));

  let mut lexer = Lexer::new(contents, path);
  let contextlol = lexer.lex(0, lexer.contents.len());
  let mut modules: Vec<Token> = Vec::new();
  println!("{:#?}", &contextlol);
  get_modules(&contextlol, &mut modules);
  println!("a: {:#?}", &modules);

  let level = if release {
    OptimizationLevel::Aggressive
  } else {
    OptimizationLevel::None
  };

  let context = Context::create();
  let mut module = Vec::new(); 
  module.push(context.create_module(&file));
  for i in modules {
    if let TokenKind::Module { tokens, path } = i.kind {
      module.push(context.create_module(&path));
    }
  }

  let main_func = contextlol.iter().filter(|x| { 
    if let TokenKind::Function{name, args, scope} = &x.kind {
      if name == "main" { return true; } else { return false; }
    } else {
      return false;
    };
  }).next().unwrap();

  println!("{:#?}", main_func);
}
