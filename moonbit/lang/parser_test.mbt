// Tests for Lambda Calculus Parser
// Based on ReScript Parser_test.res implementation

// ===== Tokenizer Tests =====

test "tokenize simple integer" {
  let tokens = tokenize("42")
  let token_str = print_tokens(tokens)
  inspect(token_str.contains("42"), content="true")
  inspect(token_str.contains("EOF"), content="true")
}

test "tokenize simple variable" {
  let tokens = tokenize("x")
  let token_str = print_tokens(tokens)
  inspect(token_str.contains("x"), content="true")
  inspect(token_str.contains("EOF"), content="true")
}

test "tokenize lambda expression" {
  let tokens = tokenize("λx.x")
  let token_str = print_tokens(tokens)
  inspect(token_str.contains("λ"), content="true")
  inspect(token_str.contains("x"), content="true")
  inspect(token_str.contains("."), content="true")
}

test "tokenize with backslash lambda" {
  let tokens = tokenize("\\x.x")
  let token_str = print_tokens(tokens)
  inspect(token_str.contains("λ"), content="true")
}

test "tokenize plus binary operator" {
  let tokens = tokenize("1 + 2")
  let token_str = print_tokens(tokens)
  inspect(token_str.contains("1"), content="true")
  inspect(token_str.contains("+"), content="true")
  inspect(token_str.contains("2"), content="true")
}

test "tokenize minus binary operator" {
  let tokens = tokenize("5 - 2")
  let token_str = print_tokens(tokens)
  inspect(token_str.contains("5"), content="true")
  inspect(token_str.contains("-"), content="true")
}

test "tokenize if-then-else" {
  let tokens = tokenize("if x then y else z")
  let token_str = print_tokens(tokens)
  inspect(token_str.contains("if"), content="true")
  inspect(token_str.contains("then"), content="true")
  inspect(token_str.contains("else"), content="true")
}

test "tokenize parentheses" {
  let tokens = tokenize("(x)")
  let token_str = print_tokens(tokens)
  inspect(token_str.contains("("), content="true")
  inspect(token_str.contains("x"), content="true")
}

test "tokenize multi-digit integer" {
  let tokens = tokenize("12345")
  let token_str = print_tokens(tokens)
  inspect(token_str.contains("12345"), content="true")
}

test "tokenize identifier with numbers" {
  let tokens = tokenize("x1y2")
  let token_str = print_tokens(tokens)
  inspect(token_str.contains("x1y2"), content="true")
}

test "tokenize whitespace handling" {
  let tokens = tokenize("  x   y  ")
  let token_str = print_tokens(tokens)
  inspect(token_str.contains("x"), content="true")
  inspect(token_str.contains("y"), content="true")
}

test "tokenize complex expression" {
  let tokens = tokenize("λf.λx.(f (f x))")
  inspect(tokens.length() > 0, content="true")
}

test "tokenize error - invalid character" {
  let should_error = try {
    let _tokens = tokenize("@")
    false
  } catch {
    TokenizationError(_) => true
  }
  inspect(should_error, content="true")
}

// ===== Parser Tests =====

test "parse simple integer" {
  let expr = parse("42")
  let printed = print_term(expr)
  inspect(printed, content="42")
}

test "parse simple variable" {
  let expr = parse("x")
  let printed = print_term(expr)
  inspect(printed, content="x")
}

test "parse identity function" {
  let expr = parse("λx.x")
  let printed = print_term(expr)
  inspect(printed, content="(λx. x)")
}

test "parse plus binary operator" {
  let expr = parse("1 + 2")
  let printed = print_term(expr)
  inspect(printed, content="(1 + 2)")
}

test "parse minus binary operator" {
  let expr = parse("5 - 2")
  let printed = print_term(expr)
  inspect(printed, content="(5 - 2)")
}

test "parse complex binary operators" {
  let expr = parse("10 - 5 + 2")
  let printed = print_term(expr)
  inspect(printed, content="((10 - 5) + 2)")
}

test "parse parenthesized expression" {
  let expr = parse("(x)")
  let printed = print_term(expr)
  inspect(printed, content="x")
}

test "parse nested lambda" {
  let expr = parse("λf.λx.x")
  let printed = print_term(expr)
  inspect(printed.contains("λf"), content="true")
  inspect(printed.contains("λx"), content="true")
}

test "parse simple application" {
  let expr = parse("f x")
  let printed = print_term(expr)
  inspect(printed, content="(f x)")
}

test "parse multiple applications" {
  let expr = parse("f x y")
  let printed = print_term(expr)
  inspect(printed, content="((f x) y)")
}

test "parse if-then-else" {
  let expr = parse("if x then y else z")
  let printed = print_term(expr)
  inspect(printed, content="if x then y else z")
}

test "parse complex if expression" {
  let expr = parse("if 1 + 2 then 3 else 4")
  let printed = print_term(expr)
  inspect(printed.contains("if"), content="true")
  inspect(printed.contains("then"), content="true")
  inspect(printed.contains("else"), content="true")
}

test "parse lambda with binary operation in body" {
  let expr = parse("λx.x + 1")
  let printed = print_term(expr)
  inspect(printed.contains("λx"), content="true")
  inspect(printed.contains("+"), content="true")
}

test "parse application with lambda" {
  let expr = parse("(λx.x) y")
  let printed = print_term(expr)
  inspect(printed.contains("λx"), content="true")
  inspect(printed.contains("y"), content="true")
}

test "parse nested parentheses" {
  let expr = parse("((x))")
  let printed = print_term(expr)
  inspect(printed, content="x")
}

test "parse complex nested expression" {
  let expr = parse("(λf.(λx.(f (f x))))")
  let printed = print_term(expr)
  inspect(printed.contains("λf"), content="true")
  inspect(printed.contains("λx"), content="true")
}

// ===== Error Handling Tests =====

test "parse error - missing parameter after lambda" {
  let should_error = try {
    let _expr = parse("λ.x")
    false
  } catch {
    _ => true
  }
  inspect(should_error, content="true")
}

test "parse error - missing dot after lambda parameter" {
  let should_error = try {
    let _expr = parse("λx")
    false
  } catch {
    _ => true
  }
  inspect(should_error, content="true")
}

test "parse error - unmatched left paren" {
  let should_error = try {
    let _expr = parse("(x")
    false
  } catch {
    _ => true
  }
  inspect(should_error, content="true")
}

test "parse error - unmatched right paren" {
  let should_error = try {
    let _expr = parse("x)")
    false
  } catch {
    _ => true
  }
  inspect(should_error, content="true")
}

test "parse error - missing then in if" {
  let should_error = try {
    let _expr = parse("if x else y")
    false
  } catch {
    _ => true
  }
  inspect(should_error, content="true")
}

test "parse error - missing else in if" {
  let should_error = try {
    let _expr = parse("if x then y")
    false
  } catch {
    _ => true
  }
  inspect(should_error, content="true")
}

// ===== Edge Case Tests =====

test "parse single character identifier" {
  let expr = parse("a")
  let printed = print_term(expr)
  inspect(printed, content="a")
}

test "parse long identifier" {
  let expr = parse("variableName123")
  let printed = print_term(expr)
  inspect(printed, content="variableName123")
}

test "parse zero" {
  let expr = parse("0")
  let printed = print_term(expr)
  inspect(printed, content="0")
}

test "parse large integer" {
  let expr = parse("999999")
  let printed = print_term(expr)
  inspect(printed, content="999999")
}

test "parse lambda with backslash" {
  let expr = parse("\\x.x")
  let printed = print_term(expr)
  inspect(printed, content="(λx. x)")
}

test "parse application chain" {
  let expr = parse("a b c d")
  let printed = print_term(expr)
  inspect(printed, content="(((a b) c) d)")
}

test "parse mixed operators and applications" {
  let expr = parse("f x + g y")
  let printed = print_term(expr)
  inspect(printed.contains("f"), content="true")
  inspect(printed.contains("x"), content="true")
  inspect(printed.contains("+"), content="true")
  inspect(printed.contains("g"), content="true")
  inspect(printed.contains("y"), content="true")
}

// ===== Integration Tests =====

test "parse and rename identity function" {
  let expr = parse("λx.x")
  match rename(expr) {
    Ok(term) => {
      let printed = print_term(term)
      inspect(printed.contains("λ"), content="true")
    }
    Err(_) => inspect(false, content="true")
  }
}

test "parse and rename nested lambda" {
  let expr = parse("λf.λx.f x")
  match rename(expr) {
    Ok(term) => {
      let printed = print_term(term)
      inspect(printed.contains("λ"), content="true")
    }
    Err(_) => inspect(false, content="true")
  }
}

test "parse complex expression with all features" {
  let expr = parse("(λf.λx.if f x then x + 1 else x - 1)")
  let printed = print_term(expr)
  inspect(printed.contains("λf"), content="true")
  inspect(printed.contains("λx"), content="true")
  inspect(printed.contains("if"), content="true")
  inspect(printed.contains("then"), content="true")
  inspect(printed.contains("else"), content="true")
  inspect(printed.contains("+"), content="true")
  inspect(printed.contains("-"), content="true")
}
