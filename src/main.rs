#![feature(let_else)]

use std::{
  fs, 
  env, 
  path::Path,
  collections::HashMap,
  mem::discriminant
};

use tokio::{fs::File, io::AsyncWriteExt};

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
  FunctionCall(Vec<String>),
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

enum Languages {
  Kotlin
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
                  "list" => Value::List,
                  "dict" => Value::Dict,
                  _ => panic!("SyntaxError {}", buffer.clone())
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
                let kind = tokens.iter().last().unwrap().kind.clone();
                if kind != TokenKind::Newline && (
                  kind != TokenKind::Assign && discriminant(&kind) != discriminant(&TokenKind::Condition(Cond::Equal)) && 
                  discriminant(&kind) != discriminant(&TokenKind::End("".to_string())) && kind != TokenKind::Identifier &&
                  kind != TokenKind::If && discriminant(&kind) != discriminant(&TokenKind::Module{tokens: Vec::new(), path: "".to_string()}) &&
                  kind != TokenKind::None
                ) {
                  buffer2.push_str((tokens.iter().last().unwrap().value.clone() + ".").as_str());
                }
                let mut args = Vec::new();
                while self.current_char(count) != ')' {
                  if self.current_char(count + 1) == ')' { buffer2.push(self.current_char(count)); }
                  if self.current_char(count) == ',' || self.current_char(count + 1) == ')' {
                    if buffer2.chars().last().unwrap() == '\"' && buffer2.chars().next().unwrap() == '\"' {
                      args.push(buffer2.clone());
                      buffer2.clear();
                    } else if buffer2.chars().find(|x| !x.is_numeric()).is_none() && buffer2.chars().find(|x| x == &'.').is_some() {
                      args.push(buffer2.clone());
                      buffer2.clear();
                    } else if buffer2.chars().find(|x| !x.is_numeric()).is_none() {
                      args.push(buffer2.clone());
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
            tokens.push(Token { kind: TokenKind::Newline, value: "\n".to_string() });
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

fn value_to_string(value: Value, languages: Languages) -> String {
  match languages {
    Languages::Kotlin => {
      match value {
        Value::Any => { "Any?" },
        Value::Integer => { "Int" },
        Value::Boolean => { "Boolean" },
        Value::Float => { "Float" },
        Value::Void => { "()" },
        Value::Dict => { "Map<Any?, Any?>" },
        Value::List => { "List<Any?>" },
        Value::String => { "String" }
      }.to_string()
    }
  }
}

fn transcompile(
  lang: Languages,
  input: Vec<Token>
) -> String {
  match lang {
    Languages::Kotlin => transcompile_kotlin(input, 0, 0)
  }
}

fn transcompile_kotlin(input: Vec<Token>, mut count: usize, ident: usize) -> String {
  let mut buffer = String::new();
  while input.get(count).is_some() {
    if count != 0 {
      if ident != 0 && input.get(count - 1).unwrap().kind == TokenKind::Newline {
        buffer.push_str("  ".repeat(ident).as_str());
      }
    }
    match &mut input.get(count).unwrap().kind.clone() {
      TokenKind::Assign => { buffer.push('=') },
      TokenKind::Boolean(b) => { 
        if *b {
          buffer.push_str("true")
        } else {
          buffer.push_str("false")
        }
      },
      TokenKind::Condition(a) => {
        match a {
          &mut Cond::And => { buffer.push_str("&&") },
          &mut Cond::Equal => { buffer.push_str("==") },
          &mut Cond::NotEqual => { buffer.push_str("!=") },
          &mut Cond::Or => { buffer.push_str("||") }
        }
      },
      TokenKind::End(v) => {
        buffer.push_str(format!("return {}", v).as_str());
      },
      TokenKind::Float(f) => {
        buffer.push_str(f);
      },
      TokenKind::Integer(f) => {
        buffer.push_str(f);
      }
      TokenKind::Function{name, args, scope, rettype} => {
        if let TokenKind::Scope(a) = &mut scope.kind {
          a.insert(0, Token { kind: TokenKind::Newline, value: "\n".to_string() });
        }
        let mut arguments = String::new();
        let TokenKind::Scope(a) = &scope.kind else { unreachable!() };
        let mut b: Option<Vec<Token>> = None; 
        for c in a.iter() {
          if let TokenKind::Scope(e) = &c.kind {
            b = Some(e.clone())
          }
        }
        drop(a);
        let len = args.len() - 1;
        for (i, (k, v)) in args.iter().enumerate() {
          arguments.push_str(format!("{}: {}", k, value_to_string(v.clone(), Languages::Kotlin)).as_str());
          if i != len {
            arguments.push_str(", ")
          }
        }
        drop(len);
        buffer.push_str(format!("fun {}({}): {}", name, arguments, value_to_string(rettype.clone(), Languages::Kotlin)).as_str());
        buffer.push_str("{\n  ");
        buffer.push_str(&transcompile_kotlin(b.clone().unwrap(), 0, ident + 1));
        buffer.push_str("}")
      },
      TokenKind::Newline => {
        buffer.push('\n');
      },
      _ => todo!()
    }
    count += 1;
  }
  buffer
}

#[allow(clippy::expect_fun_call)]
#[tokio::main]
async fn main() {
  let file = env::args().nth(1).unwrap();
  let path = Path::new(&file).parent().unwrap().to_str().unwrap().to_string();
  let contents = fs::read_to_string(&file).expect(&format!("Couldn't read this file, result is {}", file));

  let mut lexer = Lexer::new(contents, path);
  let contextlol = transcompile(Languages::Kotlin, lexer.lex(0, lexer.contents.len(), None));

  let b = file.replace(".space", ".kt");
  let ex = Path::new(&b);
  let mut c = File::create(ex).await.unwrap();

  c.write_all(contextlol.as_bytes()).await.unwrap();

  println!("Ready!");
}
