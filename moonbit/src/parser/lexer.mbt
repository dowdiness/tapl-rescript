// Lexer for Lambda Calculus

///|
pub suberror TokenizationError String

///|
fn is_big_alphabet(code : Int) -> Bool {
  code >= 65 && code <= 90
}

///|
fn is_small_alphabet(code : Int) -> Bool {
  code >= 97 && code <= 122
}

///|
fn is_alphabet(code : Int) -> Bool {
  is_big_alphabet(code) || is_small_alphabet(code)
}

///|
fn is_numeric(code : Int) -> Bool {
  code >= 48 && code <= 57
}

///|
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

///|
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

///|
fn tokenize_helper(
  input : String,
  pos : Int,
  acc : Array[Token],
) -> Array[Token] raise TokenizationError {
  if pos >= input.length() {
    let result = acc
    result.push(EOF)
    result
  } else {
    let c = input.code_unit_at(pos).to_char()
    match c {
      Some(' ') | Some('\t') | Some('\n') | Some('\r') =>
        tokenize_helper(input, pos + 1, acc)
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
      Some(c) =>
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
      None =>
        raise TokenizationError(
          "Error to read character at position ".to_string() + pos.to_string(),
        )
    }
  }
}

///| Tokenize the input string into an array of tokens
pub fn tokenize(input : String) -> Array[Token] raise TokenizationError {
  tokenize_helper(input, 0, [])
}
