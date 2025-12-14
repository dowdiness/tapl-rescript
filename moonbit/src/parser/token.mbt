///|
pub enum Token {
  Lambda // λ or \
  Dot // .
  LeftParen // (
  RightParen // )
  Plus // +
  Minus // -
  If // if
  Then // then
  Else // else
  Identifier(String) // variable names
  Integer(Int) // integer literals
  EOF // end of input
} derive(Show, Eq)

///|
pub fn print_token(token : Token) -> String {
  match token {
    Lambda => "λ"
    Dot => "."
    LeftParen => "("
    RightParen => ")"
    Plus => "+"
    Minus => "-"
    If => "if"
    Then => "then"
    Else => "else"
    Identifier(name) => name
    Integer(n) => n.to_string()
    EOF => "EOF"
  }
}

///|
pub fn print_tokens(tokens : Array[Token]) -> String {
  let token_strs = tokens.map(print_token)
  "[" + token_strs.join(", ") + "]"
}
