// Test for convert function
///|
test "convert/basic_terms" {
  // Test integer conversion
  let int_term = @parser.Int(42)
  let int_result = convert(int_term)
  inspect(int_result, content="Halt(AtomInt(42))")

  // Test variable conversion
  let var_term = @parser.Var("x")
  let var_result = convert(var_term)
  inspect(var_result, content="Halt(AtomVar(\"x\"))")
}

///|
test "convert/lambda_and_application" {
  // Test lambda conversion: λx.x (identity function)
  let lambda_term = @parser.Lam("x", @parser.Var("x"))
  let lambda_result = convert(lambda_term)
  // The result should be a function definition with a halt
  match lambda_result {
    Fun(_, params, body, Halt(AtomVar(_))) => {
      inspect(params.to_array(), content="[\"x\"]")
      inspect(body, content="Halt(AtomVar(\"x\"))")
    }
    _ => fail("Expected Fun with Halt continuation")
  }

  // Test function application: (λx.x) 5
  let app_term = @parser.App(@parser.Lam("x", @parser.Var("x")), @parser.Int(5))
  let app_result = convert(app_term)
  // Should create a function and then apply it
  match app_result {
    Fun(_, _, _, App(_, _, args, Halt(AtomVar(_)))) =>
      inspect(args.to_array(), content="[AtomInt(5)]")
    _ => fail("Expected Fun with App continuation")
  }
}

///|
test "convert/binary_ops_and_conditionals" {
  // Test binary operation: 1 + 2
  let bop_term = @parser.Bop(@parser.Plus, @parser.Int(1), @parser.Int(2))
  let bop_result = convert(bop_term)
  match bop_result {
    Bop(_, @parser.Plus, AtomInt(1), AtomInt(2), Halt(AtomVar(_))) => ()
    _ => fail("Expected Bop with Plus operation")
  }

  // Test if-then-else: if true then 1 else 2
  let if_term = @parser.Term::If(
    @parser.Int(1), // condition
    @parser.Int(2), // then branch
    @parser.Int(3), // else branch
  )
  let if_result = convert(if_term)
  // Should create a join point structure
  match if_result {
    Join(
      _,
      Some(_),
      Halt(AtomVar(_)),
      ANF::If(AtomInt(1), Jump(_, Some(AtomVar(_))), Jump(_, Some(AtomVar(_))))
    ) => ()
    _ => fail("Expected Join point with If structure")
  }
}
