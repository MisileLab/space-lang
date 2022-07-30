use std::{fs, env};

#[derive(Debug)]
struct Lexer { contents: Vec<char>, counter: usize }

#[derive(Debug, PartialEq)]
enum TokenKind {
  Function,
  FunctionName,
  ArgumentName,
  ArgumentType,
  Scope,
  ArgumentValue,
  String,
  Assign,
  UnMutVariable,
  MutVariable,
  Identifier,
  Number
}

#[derive(Debug)]
struct Token { kind: TokenKind, value: String }

impl Lexer {
  pub fn new(contents: String) -> Self { Self { contents: contents.chars().collect(), counter: 0 } }
  pub fn lex(&mut self) {
    let mut tokens: Vec<Token> = Vec::new();

    while self.contents.len() > self.counter {
      let c = self.current_char();

      match c {
        '=' => {
          tokens.push( Token { kind: TokenKind::Assign, value: "=".to_string() } );
          self.counter += 1;
        },
        '\'' | '\"' => {
          self.counter += 1;

          let mut buffer = String::new();

          while self.current_char() != c {
            buffer.push(self.current_char());
            self.counter += 1;
          }

          tokens.push( Token { kind: TokenKind::String, value: buffer});

          self.counter += 1;
        },
        _ if c.is_numeric() => {
          let mut buffer = String::new();
          buffer.push(c);
          
          self.counter += 1;

          loop {
            if self.counter >= self.contents.len() || self.current_char() == '\n' { break; }

            if self.current_char().is_numeric() { buffer.push(self.current_char()); }
            self.counter += 1;
          }

          tokens.push(Token { kind: TokenKind::Number, value: buffer });
        }
        _ if c.is_alphabetic() => {
          let mut buffer = String::new();

          buffer.push(c);
          
          self.counter += 1;

          while self.current_char().is_alphabetic() {
            buffer.push(self.current_char());
            self.counter += 1;
          }

          let kind: Option<TokenKind> = match buffer.as_str() {
            "unmut" => Some(TokenKind::UnMutVariable),
            "mut" => Some(TokenKind::MutVariable),
            "fun" => Some(TokenKind::Function),
            _ => {
              if tokens.last().unwrap().kind == TokenKind::UnMutVariable || tokens.last().unwrap().kind == TokenKind::MutVariable {
                Some(TokenKind::Identifier)
              } else if tokens.last().unwrap().kind == TokenKind::Function {
                Some(TokenKind::FunctionName)
              } else { None }
            }
          };
          
          if let Some(k) = kind { 
            tokens.push(Token { kind: k, value: buffer }); 
          } else if tokens.last().unwrap().kind == TokenKind::FunctionName {
            self.counter += 1;

            while self.current_char() != ')' {
              if self.current_char() != '\n' { 
                if self.current_char_no_space(None) == ':' {
                  tokens.push( Token { kind: TokenKind::ArgumentName, value: buffer.clone() } );
                  buffer.clear();
                  self.counter += 1;

                  while self.current_char() != ')' && self.current_char() != ',' {
                    buffer.push(self.current_char());
                    self.counter += 1;
                  }

                  tokens.push( Token { kind: TokenKind::ArgumentType, value: buffer.clone() } );
                  buffer.clear();
                } else { 
                  buffer.push(self.current_char());
                  self.counter += 1;
                }
              } else {
                self.counter += 1;
              }
            }
          }
        },
        _ => {
          self.counter += 1;
        }
      }
    }
    println!("{:#?}", tokens);
  }

  fn current_char_with_counter(&self, counter: Option<usize>) -> char { *self.contents.get(counter.unwrap_or(self.counter)).unwrap() }
  fn current_char_no_space(&self, counter: Option<usize>) -> char { 
    let mut counter = counter.unwrap_or(self.counter);
    while self.current_char_with_counter(Some(counter)) == ' ' {
      counter -= 1;
    }
    self.current_char_with_counter(Some(counter))
  }
  fn current_char(&self) -> char { self.current_char_with_counter(None) }
}

fn main() {
  let file = env::args().nth(1).unwrap();
  let contents = fs::read_to_string(&file).expect(&format!("Couldn't read this file, filename is {}", file));

  println!("{:?}", contents.chars().collect::<Vec<char>>());
  let mut lexer = Lexer::new(contents);
  lexer.lex();
}
