use std::{fs, env};

#[derive(Debug)]
struct Lexer { contents: Vec<char>, counter: usize }

#[derive(Debug, PartialEq, Clone)]
enum TokenKind {
  None,
  Function,
  FunctionName,
  ArgumentName,
  ArgumentType,
  ArgumentValue,
  Scope(Vec<Token>),
  String,
  Assign,
  UnMutVariable,
  MutVariable,
  Identifier,
  Number,
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

impl Default for TokenKind {
    fn default() -> Self {
      TokenKind::None
    }
}

#[derive(Debug, PartialEq, Default, Clone)]
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
        '/' => {
          count += 1;
          if self.current_char(Some(count)) == '/' {
            while self.current_char(Some(count)) != '\n' {
              count += 1;
            }
          } else if self.current_char(Some(count)) == '*' {
            while self.current_char(Some(count)) != '*' || self.current_char(Some(count+1)) != '/' {
              count += 1;
            }
          } else if self.current_char(Some(count)) != '\n' {
            panic!("SyntaxError");
          }
        },
        '=' => {
          if is_variable(get_token_kind(&tokens)) {
            tokens.push( Token { kind: TokenKind::Assign, value: "=".to_string() } );
            count += 1;
          } else {
            count += 1;
            if self.current_char(Some(count)) == '=' {
              tokens.push( Token { kind: TokenKind::Condition(Cond::Equal), value: "==".to_string() } );
            }
          }
        },
        '!' => {
          count += 1;
          if self.current_char(Some(count)) == '=' {
            tokens.push( Token { kind: TokenKind::Condition(Cond::NotEqual), value: "!=".to_string() } )
          }
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
            "if" => Some(TokenKind::If),
            "and" => Some(TokenKind::Condition(Cond::And)),
            "or" => Some(TokenKind::Condition(Cond::Or)),
            a if !tokens.clone().into_iter().filter(|x| (is_identifier(x) && x.value == a)).collect::<Vec<Token>>().is_empty() => {
              Some(TokenKind::ArgumentValue)
            },
            _ => {
              if is_variable(get_token_kind(&tokens)) {
                Some(TokenKind::Identifier)
              } else if self.current_char_no_space(Some(count)) == '(' {
                Some(TokenKind::FunctionName)
              } else { None }
            }
          };
          
          if let Some(k) = kind { 
            tokens.push(Token { kind: k, value: buffer }); 
          } else if get_token_kind(&tokens) == TokenKind::FunctionName {

            while self.current_char(Some(count)) != ')' {
              println!("{:?}", tokens);
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
                  if self.current_char(Some(count)) != ',' || self.current_char(Some(count)) == '('{ buffer.push(self.current_char(Some(count))); }
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

fn get_token_kind(tokens: &Vec<Token>) -> TokenKind {
  return tokens.last().unwrap_or(&Token::default()).kind.clone();
}

fn is_variable(kind: TokenKind) -> bool {
  return kind == TokenKind::UnMutVariable || kind == TokenKind::MutVariable;
}

fn is_identifier(x: &Token) -> bool {
  return x.kind == TokenKind::Identifier || x.kind == TokenKind::ArgumentName;
}

fn main() {
  let file = env::args().nth(1).unwrap();
  let contents = fs::read_to_string(&file).expect(&format!("Couldn't read this file, filename is {}", file));

  println!("{:?}", contents.chars().collect::<Vec<char>>());
  let mut lexer = Lexer::new(contents);
  println!("{:#?}", lexer.lex(None, None));
}
