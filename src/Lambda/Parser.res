// Simple Parser for Lambda Calculus expressions
// Supports: λx.e, (e1 e2), e1 + e2, e1 - e2, if e1 then e2 else e3, integers, variables

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
  let tokens = tokens->List.map(printToken)->List.toArray->Array.join(", ")
  `[${tokens}]`
}

exception TokenizationError(string)
exception ParseError(string, token)

// 65-90(A-Z)
let isBigAlphabet = (code: int) => {
  code >= 65 && code <= 90
}

// 97-122(a-z)
let isSmallAlphabet = (code: int) => {
  code >= 97 && code <= 122
}

// 65-90(A-Z), 97-122(a-z)
let isAlphabet = (code: int) => {
  isBigAlphabet(code) || isSmallAlphabet(code)
}

// 48-57(0-9)
let isNumeric = (code: int) => {
  code >= 48 && code <= 57
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
        if isAlphabet(code) {
          // Alphabetic - read identifier
          let (newPos, identifier) = readIdentifier(input, pos, "")
          let token = switch identifier {
          | "if" => If
          | "then" => Then
          | "else" => Else
          | _ => Identifier(identifier)
          }
          tokenizeHelper(input, newPos, list{token, ...acc})
        } else if isNumeric(code) {
          // Numeric - read number
          let (newPos, number) = readNumber(input, pos, 0)
          tokenizeHelper(input, newPos, list{Integer(number), ...acc})
        } else {
          raise(TokenizationError(c))
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
    if isAlphabet(code) || isNumeric(code) {
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
    if isNumeric(code) {
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
    raise(ParseError("Expected ${printToken(expected)},", current))
  }
  advance(parser)
}

// Enhanced parser with binary operators and proper precedence
let parse = (input: string): Ast.t => {
  let tokens = tokenize(input)
  let parser = makeParser(tokens)

  // Recursive descent parser with proper precedence
  let rec parseExpression = (parser: parser): (parser, Ast.t) => {
    parseBinaryOp(parser)
  }

  // Parse binary operations with left associativity (lowest precedence)
  and parseBinaryOp = (parser: parser): (parser, Ast.t) => {
    let (parser, left) = parseApplication(parser)

    let rec loop = (parser: parser, acc: Ast.t): (parser, Ast.t) => {
      switch peek(parser) {
      | Plus => {
          let parser = advance(parser)
          let (parser, right) = parseApplication(parser)
          loop(parser, Ast.Bop(Ast.Plus, acc, right))
        }
      | Minus => {
          let parser = advance(parser)
          let (parser, right) = parseApplication(parser)
          loop(parser, Ast.Bop(Ast.Minus, acc, right))
        }
      | _ => (parser, acc)
      }
    }

    loop(parser, left)
  }

  // Parse function application (higher precedence than binary ops)
  and parseApplication = (parser: parser): (parser, Ast.t) => {
    let (parser, first) = parseAtom(parser)

    let rec loop = (parser: parser, acc: Ast.t): (parser, Ast.t) => {
      switch peek(parser) {
      | LeftParen | Identifier(_) | Integer(_) | Lambda => {
          let (parser, next) = parseAtom(parser)
          loop(parser, Ast.App(acc, next))
        }
      | _ => (parser, acc)
      }
    }

    loop(parser, first)
  }

  // Parse atomic expressions (highest precedence)
  and parseAtom = (parser: parser): (parser, Ast.t) => {
    switch peek(parser) {
    | Integer(n) => (advance(parser), Ast.Int(n))
    | Identifier(name) => (advance(parser), Ast.Var(name))
    | Lambda => {
        let parser = advance(parser)
        switch peek(parser) {
        | Identifier(param) => {
            let parser = advance(parser)
            let parser = expect(parser, Dot)
            let (parser, body) = parseExpression(parser)
            (parser, Ast.Lam(param, body))
          }
        | token => raise(ParseError("Expected parameter after λ", token))
        }
      }
    | If => {
        let parser = advance(parser)
        let (parser, condition) = parseExpression(parser)
        let parser = expect(parser, Then)
        let (parser, thenExpr) = parseExpression(parser)
        let parser = expect(parser, Else)
        let (parser, elseExpr) = parseExpression(parser)
        (parser, Ast.If(condition, thenExpr, elseExpr))
      }
    | LeftParen => {
        let parser = advance(parser)
        let (parser, expr) = parseExpression(parser)
        let parser = expect(parser, RightParen)
        (parser, expr)
      }
    | token => raise(ParseError("Unexpected token:", token))
    }
  }

  let (finalParser, expr) = parseExpression(parser)

  // Check that we consumed all tokens (except EOF)
  switch peek(finalParser) {
  | EOF => expr
  | token => raise(ParseError("Unexpected tokens after expression", token))
  }
}

// Convenience functions
let parseAndCompile = (input: string): ANF.t => {
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
  Console.log("Parsed '42': " ++ Ast.printLam(expr1))

  let expr2 = parse("x")
  Console.log("Parsed 'x': " ++ Ast.printLam(expr2))

  let expr3 = parse("λx.x")
  Console.log("Parsed 'λx.x': " ++ Ast.printLam(expr3))

  // Test parsing binary operations
  let expr4 = parse("1 + 2")
  Console.log("Parsed '1 + 2': " ++ Ast.printLam(expr4))

  let expr5 = parse("5 - 3")
  Console.log("Parsed '5 - 3': " ++ Ast.printLam(expr5))

  let expr6 = parse("1 + 2 + 3")
  Console.log("Parsed '1 + 2 + 3': " ++ Ast.printLam(expr6))

  let expr7 = parse("10 - 5 + 2")
  Console.log("Parsed '10 - 5 + 2': " ++ Ast.printLam(expr7))

  // Test parsing with parentheses
  let expr8 = parse("(1 + 2) + 3")
  Console.log("Parsed '(1 + 2) + 3': " ++ Ast.printLam(expr8))

  // Test lambda with binary operations
  let expr9 = parse("λx.x + 1")
  Console.log("Parsed 'λx.x + 1': " ++ Ast.printLam(expr9))

  // Test if-then-else
  let expr10 = parse("if 1 then 2 else 3")
  Console.log("Parsed 'if 1 then 2 else 3': " ++ Ast.printLam(expr10))

  // Test compilation of binary operations
  let compiled1 = parseAndCompile("1 + 2")
  Console.log("Compiled '1 + 2': " ++ ANF.printANF(compiled1))

  let compiled2 = parseAndCompile("λx.x + 1")
  Console.log("Compiled 'λx.x + 1': " ++ ANF.printANF(compiled2))

  Console.log("Enhanced parser tests completed!")
}
