// Parser for Lambda Calculus expressions

pub enum Token {
  Lambda        // λ or \
  Dot           // .
  LeftParen     // (
  RightParen    // )
  Plus          // +
  Minus         // -
  If            // if
  Then          // then
  Else          // else
  Identifier(String)  // variable names
  Integer(Int)        // integer literals
  EOF           // end of input
} derive(Show, Eq)

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

pub fn print_tokens(tokens : Array[Token]) -> String {
  let token_strs = tokens.map(print_token)
  "[" + token_strs.join(", ") + "]"
}

pub suberror TokenizationError String
pub suberror ParseError(String, Token)

fn is_big_alphabet(code : Int) -> Bool {
  code >= 65 && code <= 90
}

fn is_small_alphabet(code : Int) -> Bool {
  code >= 97 && code <= 122
}

fn is_alphabet(code : Int) -> Bool {
  is_big_alphabet(code) || is_small_alphabet(code)
}

fn is_numeric(code : Int) -> Bool {
  code >= 48 && code <= 57
}

fn read_identifier(input : String, pos : Int, acc : String) -> (Int, String) {
  if pos >= input.length() {
    (pos, acc)
  } else {
    let code = input.code_unit_at(pos).to_int()
    if is_alphabet(code) || is_numeric(code) {
      match code.to_char() {
        Some(ch) => read_identifier(input, pos + 1, acc + ch.to_string())
        None => (pos, acc)
      }
    } else {
      (pos, acc)
    }
  }
}

fn read_number(input : String, pos : Int, acc : Int) -> (Int, Int) {
  if pos >= input.length() {
    (pos, acc)
  } else {
    let code = input.code_unit_at(pos).to_int()
    if is_numeric(code) {
      let digit = code - 48
      read_number(input, pos + 1, acc * 10 + digit)
    } else {
      (pos, acc)
    }
  }
}

fn tokenize_helper(input : String, pos : Int, acc : Array[Token]) -> Array[Token] raise TokenizationError {
  if pos >= input.length() {
    let result = acc
    result.push(EOF)
    result
  } else {
    let c = input.code_unit_at(pos).to_char()
    match c {
      Some(' ') | Some('\t') | Some('\n') | Some('\r') => tokenize_helper(input, pos + 1, acc)
      Some('λ') | Some('\\') => {
        acc.push(Lambda)
        tokenize_helper(input, pos + 1, acc)
      }
      Some('.') => {
        acc.push(Dot)
        tokenize_helper(input, pos + 1, acc)
      }
      Some('(') => {
        acc.push(LeftParen)
        tokenize_helper(input, pos + 1, acc)
      }
      Some(')') => {
        acc.push(RightParen)
        tokenize_helper(input, pos + 1, acc)
      }
      Some('+') => {
        acc.push(Plus)
        tokenize_helper(input, pos + 1, acc)
      }
      Some('-') => {
        acc.push(Minus)
        tokenize_helper(input, pos + 1, acc)
      }
      Some(c) => {
        if is_alphabet(c.to_int()) {
          let (new_pos, identifier) = read_identifier(input, pos, "")
          let token = match identifier {
            "if" => Token::If
            "then" => Token::Then
            "else" => Token::Else
            _ => Token::Identifier(identifier)
          }
          acc.push(token)
          tokenize_helper(input, new_pos, acc)
        } else if is_numeric(c.to_int()) {
          let (new_pos, number) = read_number(input, pos, 0)
          acc.push(Integer(number))
          tokenize_helper(input, new_pos, acc)
        } else {
          raise TokenizationError(c.to_string())
        }
      }
      None => raise TokenizationError("Error to read character at position ".to_string() + pos.to_string())
    }
  }
}

pub fn tokenize(input : String) -> Array[Token] raise TokenizationError {
  tokenize_helper(input, 0, [])
}

priv struct Parser {
  tokens : Array[Token]
  mut position : Int
}

fn make_parser(tokens : Array[Token]) -> Parser {
  { tokens, position: 0 }
}

fn peek(parser : Parser) -> Token {
  if parser.position < parser.tokens.length() {
    parser.tokens[parser.position]
  } else {
    EOF
  }
}

fn advance(parser : Parser) -> Parser {
  parser.position = parser.position + 1
  parser
}

fn expect(parser : Parser, expected : Token) -> Parser raise ParseError {
  let current = peek(parser)
  match (current, expected) {
    (a, b) if a == b => advance(parser)
    _ => raise ParseError(("Expected token", expected))
  }
}

pub fn parse(input : String) -> Term raise {
  let tokens = tokenize(input)
  let parser = make_parser(tokens)

  letrec parse_expression = fn (parser : Parser) -> (Parser, Term) raise {
    parse_binary_op(parser)
  }

  and parse_binary_op = fn (parser : Parser) -> (Parser, Term) raise {
    let (parser, left) = parse_application(parser)

    loop (parser, left) {
      (parser, term) => {
        match peek(parser) {
          Plus => {
            let parser = advance(parser)
            let (parser, right) = parse_application(parser)
            continue(parser, Term::Bop(Bop::Plus, term, right))
          }
          Minus => {
            let parser = advance(parser)
            let (parser, right) = parse_application(parser)
            continue(parser, Term::Bop(Bop::Minus, term, right))
          }
          _ => break(parser, term)
        }
      }
    }
  }

  and parse_application = fn (parser : Parser) -> (Parser, Term) raise {
    let (parser, first) = parse_atom(parser)

    loop (parser, first) {
      (parser, acc) => {
        match peek(parser) {
          LeftParen | Identifier(_) | Integer(_) | Lambda => {
            let (parser, next) = parse_atom(parser)
            continue(parser, Term::App(acc, next))
          }
          _ => break(parser, acc)
        }
      }
    }
  }

  and parse_atom = fn (parser : Parser) -> (Parser, Term) raise {
    match peek(parser) {
      Integer(n) => (advance(parser), Term::Int(n))
      Identifier(name) => (advance(parser), Term::Var(name))
      Lambda => {
        let parser = advance(parser)
        match peek(parser) {
          Identifier(param) => {
            let parser = advance(parser)
            let parser = expect(parser, Dot)
            let (parser, body) = parse_expression(parser)
            (parser, Term::Lam(param, body))
          }
          token => raise ParseError(("Expected parameter after λ", token))
        }
      }
      If => {
        let parser = advance(parser)
        let (parser, condition) = parse_expression(parser)
        let parser = expect(parser, Then)
        let (parser, then_expr) = parse_expression(parser)
        let parser = expect(parser, Else)
        let (parser, else_expr) = parse_expression(parser)
        (parser, Term::If(condition, then_expr, else_expr))
      }
      LeftParen => {
        let parser = advance(parser)
        let (parser, expr) = parse_expression(parser)
        let parser = expect(parser, RightParen)
        (parser, expr)
      }
      token => raise ParseError(("Unexpected token", token))
    }
  }

  let (final_parser, expr) = parse_expression(parser)

  match peek(final_parser) {
    EOF => expr
    token => raise ParseError(("Unexpected tokens after expression", token))
  }
}
