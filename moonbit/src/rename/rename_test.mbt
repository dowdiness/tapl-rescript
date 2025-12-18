// ===== Integration Tests =====

test "parse and rename identity function" {
  let expr = @parser.parse("λx.x")
  reset()
  let renamed = rename(expr)
  let printed = @parser.print_term(renamed)
  inspect(printed.contains("x0"), content="true")
}

test "parse and rename nested lambda" {
  let expr = @parser.parse("λf.λx.f x")
  reset()
  let renamed = rename(expr)
  let printed = @parser.print_term(renamed)
  inspect(printed.contains("f0"), content="true")
  inspect(printed.contains("x1"), content="true")
}
