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

// Main parse function - simple expression parser
let parse = (input: string): LambdaCompile.Lam.t => {
  // For now, let's implement a simple parser for basic cases
  let tokens = tokenize(input)

  let rec parseExpr = (parser: parser): (parser, LambdaCompile.Lam.t) => {
    switch peek(parser) {
    | Integer(n) => (advance(parser), LambdaCompile.Lam.Int(n))
    | Identifier(name) => (advance(parser), LambdaCompile.Lam.Var(name))
    | Lambda => {
        let parser = advance(parser)
        switch peek(parser) {
        | Identifier(param) => {
            let parser = advance(parser)
            let parser = expect(parser, Dot)
            let (parser, body) = parseExpr(parser)
            (parser, LambdaCompile.Lam.Lam(param, body))
          }
        | _ => raise(ParseError("Expected parameter after λ"))
        }
      }
    | LeftParen => {
        let parser = advance(parser)
        let (parser, expr) = parseExpr(parser)
        let parser = expect(parser, RightParen)
        (parser, expr)
      }
    | If => {
      let parser = advance(parser)
      let (parser, condExpr) = parseExpr(parser)
      let parser = expect(parser, Then)
      let (parser, thenExpr) = parseExpr(parser)
      let parser = expect(parser, Else)
      let (parser, elseExpr) = parseExpr(parser)
      (parser, LambdaCompile.Lam.If(condExpr, thenExpr, elseExpr))
    }
    | token => raise(ParseError(`Unexpected token: ${printToken(token)}`))
    }
  }

  let parser = makeParser(tokens)
  let (_, expr) = parseExpr(parser)
  expr
}

// Convenience functions
let parseAndCompile = (input: string): LambdaCompile.ANF.t => {
  input->parse->LambdaCompile.Compiler.compile
}

let parseAndCompileToLLVM = (input: string, phase: int): string => {
  input->parse->LambdaCompile.Compiler.compileToLLVM(phase)
}

// Test the parser
let testParser = () => {
  Console.log("Testing parser...")

  // Test tokenization
  let tokens1 = tokenize("λx.2")
  Console.log("Tokens for '42': " ++ printTokens(tokens1))

  let tokens2 = tokenize("λx.x")
  Console.log("Tokens for 'λx.x': " ++ printTokens(tokens2))

  let tokens3 = tokenize("if true then 2 else 3")
  Console.log("Tokens for 'if true then 2 else 3': " ++ printTokens(tokens3))

  // Test parsing
  let expr1 = parse("42")
  Console.log("Parsed '42': " ++ LambdaCompile.Print.printLam(expr1))

  let expr2 = parse("x")
  Console.log("Parsed 'x': " ++ LambdaCompile.Print.printLam(expr2))

  let expr3 = parse("λx.x")
  Console.log("Parsed 'λx.x': " ++ LambdaCompile.Print.printLam(expr3))

  let expr4 = parse("if true then 2 else 3")
  Console.log("Parsed 'if true then 2 else 3': " ++ LambdaCompile.Print.printLam(expr4))

  // Test compilation
  let compiled = parseAndCompile("λx.x")
  Console.log("Compiled 'λx.x': " ++ LambdaCompile.Print.printANF(compiled))

  Console.log("Parser tests completed!")
}

testParser()
