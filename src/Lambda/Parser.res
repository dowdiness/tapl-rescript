// Simple Parser for Lambda Calculus expressions
// Supports: λx.e, (e1 e2), e1 + e2, e1 - e2, if e1 then e2 else e3, integers, variables

exception ParseError(string)

// Token types
type token =
  | Lambda                    // λ or \
  | Dot                      // .
  | LeftParen               // (
  | RightParen              // )
  | Plus                    // +
  | Minus                   // -
  | If                      // if
  | Then                    // then
  | Else                    // else
  | Identifier(string)      // variable names
  | Integer(int)            // integer literals
  | EOF                     // end of input

// Pretty print tokens (for debugging)
let printToken = (token: token): string => {
  switch token {
  | Lambda => "λ"
  | Dot => "."
  | LeftParen => "("
  | RightParen => ")"
  | Plus => "+"
  | Minus => "-"
  | If => "if"
  | Then => "then"
  | Else => "else"
  | Identifier(name) => name
  | Integer(n) => Int.toString(n)
  | EOF => "EOF"
  }
}

let printTokens = (tokens: list<token>): string => {
  let tokens = tokens->List.map(printToken)->List.toArray->Array.joinWith(", ")
  `[${tokens}]`
}

// Simple recursive tokenizer
let rec tokenizeHelper = (input: string, pos: int, acc: list<token>): list<token> => {
  if pos >= String.length(input) {
    List.reverse(list{EOF, ...acc})
  } else {
    let c = String.charAt(input, pos)
    switch c {
    | " " | "\t" | "\n" | "\r" => tokenizeHelper(input, pos + 1, acc)
    | "λ" | "\\" => tokenizeHelper(input, pos + 1, list{Lambda, ...acc})
    | "." => tokenizeHelper(input, pos + 1, list{Dot, ...acc})
    | "(" => tokenizeHelper(input, pos + 1, list{LeftParen, ...acc})
    | ")" => tokenizeHelper(input, pos + 1, list{RightParen, ...acc})
    | "+" => tokenizeHelper(input, pos + 1, list{Plus, ...acc})
    | "-" => tokenizeHelper(input, pos + 1, list{Minus, ...acc})
    | c => {
        let code = String.charCodeAt(c, 0)->Float.toInt
        if (code >= 65 && code <= 90) || (code >= 97 && code <= 122) {
          // Alphabetic - read identifier
          let (newPos, identifier) = readIdentifier(input, pos, "")
          let token = switch identifier {
          | "if" => If
          | "then" => Then
          | "else" => Else
          | _ => Identifier(identifier)
          }
          tokenizeHelper(input, newPos, list{token, ...acc})
        } else if code >= 48 && code <= 57 {
          // Numeric - read number
          let (newPos, number) = readNumber(input, pos, 0)
          tokenizeHelper(input, newPos, list{Integer(number), ...acc})
        } else {
          raise(ParseError(`Unexpected character: ${c}`))
        }
      }
    }
  }
}

and readIdentifier = (input: string, pos: int, acc: string): (int, string) => {
  if pos >= String.length(input) {
    (pos, acc)
  } else {
    let c = String.charAt(input, pos)
    let code = String.charCodeAt(c, 0)->Float.toInt
    if (code >= 65 && code <= 90) || (code >= 97 && code <= 122) || (code >= 48 && code <= 57) {
      readIdentifier(input, pos + 1, acc ++ c)
    } else {
      (pos, acc)
    }
  }
}

and readNumber = (input: string, pos: int, acc: int): (int, int) => {
  if pos >= String.length(input) {
    (pos, acc)
  } else {
    let c = String.charAt(input, pos)
    let code = String.charCodeAt(c, 0)->Float.toInt
    if code >= 48 && code <= 57 {
      let digit = code - 48
      readNumber(input, pos + 1, acc * 10 + digit)
    } else {
      (pos, acc)
    }
  }
}

let tokenize = (input: string): list<token> => {
  tokenizeHelper(input, 0, list{})
}

// Parser state
type parser = {
  tokens: list<token>,
  position: int,
}

let makeParser = (tokens: list<token>): parser => {
  tokens: tokens,
  position: 0,
}

let peek = (parser: parser): token => {
  switch List.get(parser.tokens, parser.position) {
  | Some(token) => token
  | None => EOF
  }
}

let advance = (parser: parser): parser => {
  ...parser,
  position: parser.position + 1,
}

let expect = (parser: parser, expected: token): parser => {
  let current = peek(parser)
  if current != expected {
    raise(ParseError(`Expected ${printToken(expected)}, but get ${printToken(current)}`))
  }
  advance(parser)
}

// Enhanced parser with binary operators and proper precedence
let parse = (input: string): Compile.Lam.t => {
  let tokens = tokenize(input)
  let parser = makeParser(tokens)

  // Recursive descent parser with proper precedence
  let rec parseExpression = (parser: parser): (parser, Compile.Lam.t) => {
    parseBinaryOp(parser)
  }

  // Parse binary operations with left associativity (lowest precedence)
  and parseBinaryOp = (parser: parser): (parser, Compile.Lam.t) => {
    let (parser, left) = parseApplication(parser)

    let rec loop = (parser: parser, acc: Compile.Lam.t): (parser, Compile.Lam.t) => {
      switch peek(parser) {
      | Plus => {
          let parser = advance(parser)
          let (parser, right) = parseApplication(parser)
          loop(parser, Compile.Lam.Bop(Compile.Plus, acc, right))
        }
      | Minus => {
          let parser = advance(parser)
          let (parser, right) = parseApplication(parser)
          loop(parser, Compile.Lam.Bop(Compile.Minus, acc, right))
        }
      | _ => (parser, acc)
      }
    }

    loop(parser, left)
  }

  // Parse function application (higher precedence than binary ops)
  and parseApplication = (parser: parser): (parser, Compile.Lam.t) => {
    let (parser, first) = parseAtom(parser)

    let rec loop = (parser: parser, acc: Compile.Lam.t): (parser, Compile.Lam.t) => {
      switch peek(parser) {
      | LeftParen | Identifier(_) | Integer(_) | Lambda => {
          let (parser, next) = parseAtom(parser)
          loop(parser, Compile.Lam.App(acc, next))
        }
      | _ => (parser, acc)
      }
    }

    loop(parser, first)
  }

  // Parse atomic expressions (highest precedence)
  and parseAtom = (parser: parser): (parser, Compile.Lam.t) => {
    switch peek(parser) {
    | Integer(n) => (advance(parser), Compile.Lam.Int(n))
    | Identifier(name) => (advance(parser), Compile.Lam.Var(name))
    | Lambda => {
        let parser = advance(parser)
        switch peek(parser) {
        | Identifier(param) => {
            let parser = advance(parser)
            let parser = expect(parser, Dot)
            let (parser, body) = parseExpression(parser)
            (parser, Compile.Lam.Lam(param, body))
          }
        | _ => raise(ParseError("Expected parameter after λ"))
        }
      }
    | If => {
        let parser = advance(parser)
        let (parser, condition) = parseExpression(parser)
        let parser = expect(parser, Then)
        let (parser, thenExpr) = parseExpression(parser)
        let parser = expect(parser, Else)
        let (parser, elseExpr) = parseExpression(parser)
        (parser, Compile.Lam.If(condition, thenExpr, elseExpr))
      }
    | LeftParen => {
        let parser = advance(parser)
        let (parser, expr) = parseExpression(parser)
        let parser = expect(parser, RightParen)
        (parser, expr)
      }
    | token => raise(ParseError(`Unexpected token: ${printToken(token)}`))
    }
  }

  let (finalParser, expr) = parseExpression(parser)

  // Check that we consumed all tokens (except EOF)
  switch peek(finalParser) {
  | EOF => expr
  | _ => raise(ParseError("Unexpected tokens after expression"))
  }
}

// Convenience functions
let parseAndCompile = (input: string): Compile.ANF.t => {
  input->parse->Compile.Compiler.compile
}

let parseAndCompileToLLVM = (input: string, phase: int): string => {
  input->parse->Compile.Compiler.compileToLLVM(phase)
}

// Test the parser with binary operators
let testParser = () => {
  Console.log("Testing enhanced parser with binary operators...")

  // Test tokenization
  let tokens1 = tokenize("1 + 2")
  Console.log("Tokens for '1 + 2': " ++ printTokens(tokens1))

  let tokens2 = tokenize("λx.x + 1")
  Console.log("Tokens for 'λx.x + 1': " ++ printTokens(tokens2))

  let tokens3 = tokenize("3 - 1 + 2")
  Console.log("Tokens for '3 - 1 + 2': " ++ printTokens(tokens3))

  // Test parsing basic expressions
  let expr1 = parse("42")
  Console.log("Parsed '42': " ++ Compile.Print.printLam(expr1))

  let expr2 = parse("x")
  Console.log("Parsed 'x': " ++ Compile.Print.printLam(expr2))

  let expr3 = parse("λx.x")
  Console.log("Parsed 'λx.x': " ++ Compile.Print.printLam(expr3))

  // Test parsing binary operations
  let expr4 = parse("1 + 2")
  Console.log("Parsed '1 + 2': " ++ Compile.Print.printLam(expr4))

  let expr5 = parse("5 - 3")
  Console.log("Parsed '5 - 3': " ++ Compile.Print.printLam(expr5))

  let expr6 = parse("1 + 2 + 3")
  Console.log("Parsed '1 + 2 + 3': " ++ Compile.Print.printLam(expr6))

  let expr7 = parse("10 - 5 + 2")
  Console.log("Parsed '10 - 5 + 2': " ++ Compile.Print.printLam(expr7))

  // Test parsing with parentheses
  let expr8 = parse("(1 + 2) + 3")
  Console.log("Parsed '(1 + 2) + 3': " ++ Compile.Print.printLam(expr8))

  // Test lambda with binary operations
  let expr9 = parse("λx.x + 1")
  Console.log("Parsed 'λx.x + 1': " ++ Compile.Print.printLam(expr9))

  // Test if-then-else
  let expr10 = parse("if 1 then 2 else 3")
  Console.log("Parsed 'if 1 then 2 else 3': " ++ Compile.Print.printLam(expr10))

  // Test compilation of binary operations
  let compiled1 = parseAndCompile("1 + 2")
  Console.log("Compiled '1 + 2': " ++ Compile.Print.printANF(compiled1))

  let compiled2 = parseAndCompile("λx.x + 1")
  Console.log("Compiled 'λx.x + 1': " ++ Compile.Print.printANF(compiled2))

  Console.log("Enhanced parser tests completed!")
}
