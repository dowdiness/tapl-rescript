// ===== Integration Tests =====

test "parse and rename identity function" {
  let expr = @parser.parse("λx.x")
  match rename(expr) {
    Ok(term) => {
      let printed = @parser.print_term(term)
      inspect(printed.contains("x0"), content="true")
    }
    Err(_) => inspect(false, content="true")
  }
}

test "parse and rename nested lambda" {
  let expr = @parser.parse("λf.λx.f x")
  match rename(expr) {
    Ok(term) => {
      let printed = @parser.print_term(term)
      inspect(printed.contains("f0"), content="true")
      inspect(printed.contains("x1"), content="true")
    }
    Err(_) => inspect(false, content="true")
  }
}
