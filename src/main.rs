use std::{fs, env};

#[derive(Debug)]
struct Lexer { contents: Vec<char>, counter: usize }

#[derive(Debug, PartialEq)]
enum TokenKind {
  None,
  Function,
  FunctionName,
  ArgumentName,
  ArgumentType,
  Scope(Vec<Token>),
  ArgumentValue,
  String,
  Assign,
  UnMutVariable,
  MutVariable,
  Identifier,
  Number,
  If,
  Condition
}

impl Default for TokenKind {
    fn default() -> Self {
      TokenKind::None
    }
}

#[derive(Debug, PartialEq, Default)]
struct Token { kind: TokenKind, value: String }

impl Lexer {
  pub fn new(contents: String) -> Self { Self { contents: contents.chars().collect(), counter: 0 } }
  pub fn lex(&mut self, counter: Option<usize>, limit: Option<usize>) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut count = counter.unwrap_or(self.counter);
    let limit = limit.unwrap_or(self.contents.len());

    while limit > count {
      let c = self.current_char(Some(count));

      match c {
        '=' => {
          tokens.push( Token { kind: TokenKind::Assign, value: "=".to_string() } );
          count += 1;
        },
        '\'' | '\"' => {
          count += 1;

          let mut buffer = String::new();

          while self.current_char(Some(count)) != c {
            buffer.push(self.current_char(Some(count)));
            count += 1;
          }

          tokens.push( Token { kind: TokenKind::String, value: buffer});

          count += 1;
        },
        '{' => {
          count += 1;
          
          let mut buffer = String::new();

          while self.current_char(Some(count)) != '}' {
            buffer.push(self.current_char(Some(count)));
            count += 1;
          }

          tokens.push( Token {kind: TokenKind::Scope(self.lex(Some(count - buffer.len()), Some(count))), value: buffer} )
        },
        _ if c.is_numeric() => {
          let mut buffer = String::new();
          buffer.push(c);
          
          count += 1;

          loop {
            if count >= limit || self.current_char(Some(count)) == '\n' { break; }

            if self.current_char(Some(count)).is_numeric() { buffer.push(self.current_char(Some(count))); }
            count += 1;
          }

          tokens.push(Token { kind: TokenKind::Number, value: buffer });
        }
        _ if c.is_alphabetic() => {
          let mut buffer = String::new();

          buffer.push(c);
          
          count += 1;

          while self.current_char(Some(count)).is_alphabetic() {
            buffer.push(self.current_char(Some(count)));
            count += 1;
          }

          let kind: Option<TokenKind> = match buffer.as_str() {
            "unmut" => Some(TokenKind::UnMutVariable),
            "mut" => Some(TokenKind::MutVariable),
            "fun" => Some(TokenKind::Function),
            _ => {
              if tokens.last().unwrap_or(&Token::default()).kind == TokenKind::UnMutVariable || tokens.last().unwrap_or(&Token::default()).kind == TokenKind::MutVariable {
                Some(TokenKind::Identifier)
              } else if self.current_char_no_space(Some(count)) == '(' {
                Some(TokenKind::FunctionName)
              } else { None }
            }
          };
          
          if let Some(k) = kind { 
            tokens.push(Token { kind: k, value: buffer }); 
          } else if tokens.last().unwrap_or(&Token::default()).kind == TokenKind::FunctionName {
            count += 1;

            while self.current_char(Some(count)) != ')' {
              if self.current_char(Some(count)) != '\n' { 
                if self.current_char_no_space(Some(count)) == ':' {
                  tokens.push( Token { kind: TokenKind::ArgumentName, value: buffer.clone() } );
                  buffer.clear();
                  count += 1;

                  while self.current_char(Some(count)) != ')' && self.current_char(Some(count)) != ',' {
                    buffer.push(self.current_char(Some(count)));
                    count += 1;
                  }

                  tokens.push( Token { kind: TokenKind::ArgumentType, value: buffer.clone() } );
                  buffer.clear();
                } else { 
                  buffer.push(self.current_char(Some(count)));
                  count += 1;
                }
              } else {
                count += 1;
              }
            }
          }
        },
        _ => {
          count += 1;
        }
      }
    }
    if counter.is_none() { self.counter = count; }
    return tokens;
  }

  fn current_char(&self, counter: Option<usize>) -> char { *self.contents.get(counter.unwrap_or(self.counter)).unwrap() }
  fn current_char_no_space(&self, counter: Option<usize>) -> char { 
    let mut counter = counter.unwrap_or(self.counter);
    while self.current_char(Some(counter)) == ' ' || self.current_char(Some(counter)) == '\n' {
      counter -= 1;
    }
    self.current_char(Some(counter))
  }
}

fn main() {
  let file = env::args().nth(1).unwrap();
  let contents = fs::read_to_string(&file).expect(&format!("Couldn't read this file, filename is {}", file));

  println!("{:?}", contents.chars().collect::<Vec<char>>());
  let mut lexer = Lexer::new(contents);
  println!("{:#?}", lexer.lex(None, None));
}
