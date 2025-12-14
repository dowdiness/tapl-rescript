// Parser for Lambda Calculus expressions

///|
pub suberror ParseError (String, Token)

///|
priv struct Parser {
  tokens : Array[Token]
  mut position : Int
}

///|
fn make_parser(tokens : Array[Token]) -> Parser {
  { tokens, position: 0 }
}

///|
fn peek(parser : Parser) -> Token {
  if parser.position < parser.tokens.length() {
    parser.tokens[parser.position]
  } else {
    EOF
  }
}

///|
fn advance(parser : Parser) -> Parser {
  parser.position = parser.position + 1
  parser
}

///|
fn expect(parser : Parser, expected : Token) -> Parser raise ParseError {
  let current = peek(parser)
  match (current, expected) {
    (a, b) if a == b => advance(parser)
    _ => raise ParseError(("Expected token", expected))
  }
}

///| Parse the input string into a Term
pub fn parse(input : String) -> Term raise {
  let tokens = tokenize(input)
  let parser = make_parser(tokens)
  letrec parse_expression = fn(parser : Parser) -> (Parser, Term) raise {
    parse_binary_op(parser)
  }
  and parse_binary_op = fn(parser : Parser) -> (Parser, Term) raise {
    let (parser, left) = parse_application(parser)
    loop (parser, left) {
      (parser, term) =>
        match peek(parser) {
          Plus => {
            let parser = advance(parser)
            let (parser, right) = parse_application(parser)
            continue (parser, Term::Bop(Bop::Plus, term, right))
          }
          Minus => {
            let parser = advance(parser)
            let (parser, right) = parse_application(parser)
            continue (parser, Term::Bop(Bop::Minus, term, right))
          }
          _ => break (parser, term)
        }
    }
  }
  and parse_application = fn(parser : Parser) -> (Parser, Term) raise {
    let (parser, first) = parse_atom(parser)
    loop (parser, first) {
      (parser, acc) =>
        match peek(parser) {
          LeftParen | Identifier(_) | Integer(_) | Lambda => {
            let (parser, next) = parse_atom(parser)
            continue (parser, Term::App(acc, next))
          }
          _ => break (parser, acc)
        }
    }
  }
  and parse_atom = fn(parser : Parser) -> (Parser, Term) raise {
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
