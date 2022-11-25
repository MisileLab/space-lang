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
  FunctionCall(String),
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
                let mut args = String::new();
                while self.current_char(count) != ')' {
                  args.push(self.current_char(count));
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

fn value_to_string(value: Value, languages: Languages) -> String {
  match languages {
    Languages::Kotlin => {
      match value {
        Value::Any => { "Any?" },
        Value::Integer => { "Int" },
        Value::Boolean => { "Boolean" },
        Value::Float => { "Float" },
        Value::Void => { "Unit" },
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
  println!("asd: {:#?}", input);
  while input.get(count).is_some() {
    if count != 0 && ident != 0 && input.get(count - 1).unwrap().kind == TokenKind::Newline {
      buffer.push_str("  ".repeat(ident).as_str());
    }
    if cfg!(debug_assertions) {
      println!("{}: {:#?}", count, input.get(count).unwrap().clone());
    }
    match &mut input.get(count).unwrap().kind.clone() {
      TokenKind::Assign => { buffer.push_str(" = ") },
      TokenKind::Boolean(b) => { 
        if *b {
          buffer.push_str("true")
        } else {
          buffer.push_str("false")
        }
      },
      TokenKind::Condition(a) => {
        match *a {
          Cond::And => { buffer.push_str("&&") },
          Cond::Equal => { buffer.push_str("==") },
          Cond::NotEqual => { buffer.push_str("!=") },
          Cond::Or => { buffer.push_str("||") }
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
      },
      TokenKind::Function{name, args, scope, rettype} => {
        let mut arguments = String::new();
        let TokenKind::Scope(a) = &scope.kind else { unreachable!() };
        let len = if args.is_empty() { 0 } else { args.len() - 1 };
        for (i, (k, v)) in args.iter().enumerate() {
          arguments.push_str(format!("{}: {}", k, value_to_string(v.clone(), Languages::Kotlin)).as_str());
          if i != len {
            arguments.push_str(", ")
          }
        }
        println!("{}", transcompile_kotlin(a.clone(), 0, ident + 1));
        buffer.push_str(format!("fun {}({}): {}", name, arguments, value_to_string(rettype.clone(), Languages::Kotlin)).as_str());
        buffer.push_str("{\n  ");
        buffer.push_str(&transcompile_kotlin(a.clone(), 0, ident + 1));
        buffer.push('}')
      },
      TokenKind::Newline => {
        buffer.push('\n');
      },
      TokenKind::FunctionCall(a) => {
        buffer.push_str(format!("{}({})", &input.get(count).unwrap().value, a).as_str())
      },
      TokenKind::Variable{mutable} => {
        if *mutable {
          buffer.push_str("var")
        } else {
          buffer.push_str("val")
        }
      },
      TokenKind::Identifier => {
        buffer.push(' ');
        buffer.push_str(&input.get(count).unwrap().value);
      },
      TokenKind::VariableCall => {
        buffer.push_str(&input.get(count).unwrap().value);
      },
      TokenKind::If => {
        buffer.push_str("if");
        count += 1;
        if let TokenKind::Condition(_) = &input.get(count + 1).unwrap().kind {
          count += 1;
          buffer.push_str(format!(
            "({})", 
            transcompile_kotlin(vec![
              input.get(count - 1).unwrap().clone(),
              input.get(count).unwrap().clone(),
              input.get(count + 1).unwrap().clone()
            ], 0, ident),
          ).as_str());
          count += 2;
          while let TokenKind::Condition(_) = &input.get(count).unwrap().kind {
            buffer.push_str(format!(
              "({})", 
              transcompile_kotlin(vec![
                input.get(count - 1).unwrap().clone(),
                input.get(count).unwrap().clone(),
                input.get(count + 1).unwrap().clone()
              ], 0, ident),
            ).as_str());
            count += 2;
          }
        } else if let TokenKind::Boolean(a) = &input.get(count).unwrap().kind {
          if *a {
            buffer.push_str("true")
          } else {
            buffer.push_str("false")
          }
          count += 1;
        } else {
          panic!("No condition after if")
        };
      },
      TokenKind::Scope(a) => {
        buffer.push('{');
        buffer.push_str(&transcompile_kotlin(a.clone(), 0, ident + 1));
        buffer.push('}');
      },
      TokenKind::String(a) => {
        buffer.push_str(a);
      }
      _ => panic!("Not yet implemented, {:#?}", input.get(count).unwrap().clone().kind)
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
  println!("{:#?}", lexer.lex(0, lexer.contents.len(), None));
  let contextlol = transcompile(Languages::Kotlin, lexer.lex(0, lexer.contents.len(), None));

  let b = file.replace(".space", ".kt");
  let ex = Path::new(&b);
  let mut c = File::create(ex).await.unwrap();

  c.write_all(contextlol.as_bytes()).await.unwrap();

  println!("Ready!");
}
