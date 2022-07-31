use std::{fs, env, path::Path};

#[derive(Debug)]
struct Lexer { contents: Vec<char>, path: String }

#[derive(Debug, PartialEq, Clone)]
enum TokenKind {
  None,
  Function,
  FunctionAssignName,
  FunctionName,
  ArgumentName,
  ArgumentType,
  Variable,
  Scope(Vec<Token>),
  String,
  Assign,
  UnMutVariable,
  MutVariable,
  Identifier,
  Number(Number),
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
enum Number {
  Float,
  Int
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
          if is_variable(get_token_kind(&tokens)) {
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
            tokens.push(Token { kind: TokenKind::Number(Number::Float), value: buffer });
          } else {
            tokens.push(Token { kind: TokenKind::Number(Number::Int), value: buffer });
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
            "unmut" => Some(TokenKind::UnMutVariable),
            "mut" => Some(TokenKind::MutVariable),
            "fun" => Some(TokenKind::Function),
            "if" => Some(TokenKind::If),
            "and" => Some(TokenKind::Condition(Cond::And)),
            "or" => Some(TokenKind::Condition(Cond::Or)),
            "import" => {
              let mut file = String::new();
              while self.current_char(count).is_alphabetic() || ((self.current_char(count) == '/' || self.current_char(count) == '.') && self.current_char(count + 1).is_alphabetic()) || self.current_char(count) == ' ' {
                if self.current_char(count) != ' ' { file.push(self.current_char(count)); }
                count += 1;
              }
              let path = Path::new(&format!("{}/{}", self.path, &file).to_string()).to_str().unwrap().to_string();
              let contents = fs::read_to_string(path).expect(&format!("Couldn't read this file, result is {}", file));
              Some(TokenKind::Scope(parser(Lexer::new(contents.clone(), self.path.clone()).lex(0, contents.len()))))
            }
            a if !tokens.clone().into_iter().filter(|x| (is_identifier(x) && x.value == a)).collect::<Vec<Token>>().is_empty() => {
              Some(TokenKind::Variable)
            },
            _ => {
              if is_variable(get_token_kind(&tokens)) {
                Some(TokenKind::Identifier)
              } else if self.current_char_no_space(count) == '(' {
                Some(TokenKind::FunctionName)
              } else { None }
            }
          };
          
          if let Some(k) = kind { 
            tokens.push(Token { kind: k, value: buffer }); 
          } else if get_token_kind(&tokens) == TokenKind::FunctionName {

            while self.current_char(count) != ')' {
              if self.current_char(count) != '\n' { 
                if self.current_char_no_space(count) == ':' {
                  tokens.push( Token { kind: TokenKind::ArgumentName, value: buffer.clone() } );
                  buffer.clear();
                  count += 1;

                  while self.current_char(count) != ')' && self.current_char(count) != ',' {
                    buffer.push(self.current_char(count));
                    count += 1;
                  }

                  tokens.push( Token { kind: TokenKind::ArgumentType, value: buffer.clone() } );
                  buffer.clear();
                } else { 
                  if self.current_char(count) != ',' || self.current_char(count) == '('{ buffer.push(self.current_char(count)); }
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
    return tokens;
  }

  fn current_char(&self, counter: usize) -> char { *self.contents.get(counter).unwrap() }
  fn current_char_no_space(&self, mut counter: usize) -> char { 
    while self.current_char(counter) == ' ' || self.current_char(counter) == '\n' {
      counter -= 1;
    }
    self.current_char(counter)
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

fn parser(mut tokens: Vec<Token>) -> Vec<Token> {
  let a: Vec<Token> = tokens.clone().into_iter().filter(|x| x.kind == TokenKind::Variable ).collect();
  let b: (Vec<Token>, Vec<usize>);
  {
    let mut temp: Vec<usize> = Vec::new();
    for (i, i2) in tokens.clone().into_iter().enumerate() {
      if i2.kind == TokenKind::Identifier {
        temp.push(i+1);
      }
    }
    b = (
      tokens.clone().into_iter().filter(|x| x.kind == TokenKind::Identifier ).collect(),
      temp
    )
  }
  for (f, d) in a.iter().enumerate() {
    let c = b.clone().0.into_iter().filter(|x| x.value == d.value ).collect::<Vec<Token>>();
    if c.is_empty() { panic!("SyntaxError"); } else {
      let e = tokens.iter().position(|x| x == d).unwrap();
      tokens[e] = tokens[b.1[f]].clone();
    }
  }
  tokens
}

fn main() {
  let file = env::args().nth(1).unwrap();
  let path = Path::new(&file).parent().unwrap().to_str().unwrap().to_string();
  let contents = fs::read_to_string(&file).expect(&format!("Couldn't read this file, result is {}", file));

  let mut lexer = Lexer::new(contents, path);
  println!("{:#?}", parser(lexer.lex(0, lexer.contents.len())));
}
