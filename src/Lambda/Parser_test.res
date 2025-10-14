// Tests for Parser
// Suppres warning that suggests shadowing the value identifier not.
@@warning("-44")

open Vitest

describe("Parser Tests", () => {
  test("tokenize simple integer", () => {
    let tokens = Parser.tokenize("42")
    let tokenStrings = Parser.printTokens(tokens)
    expect(tokenStrings)->toContain("42")
    expect(tokenStrings)->toContain("EOF")
  })

  test("tokenize simple variable", () => {
    let tokens = Parser.tokenize("x")
    let tokenStrings = Parser.printTokens(tokens)
    expect(tokenStrings)->toContain("x")
    expect(tokenStrings)->toContain("EOF")
  })

  test("tokenize lambda expression", () => {
    let tokens = Parser.tokenize("λx.x")
    let tokenStrings = Parser.printTokens(tokens)
    expect(tokenStrings)->toContain("λ")
    expect(tokenStrings)->toContain("x")
    expect(tokenStrings)->toContain(".")
    expect(tokenStrings)->toContain("EOF")
  })

  test("tokenize with backslash lambda", () => {
    let tokens = Parser.tokenize("\\x.x")
    let tokenStrings = Parser.printTokens(tokens)
    expect(tokenStrings)->toContain("λ")
    expect(tokenStrings)->toContain("x")
    expect(tokenStrings)->toContain(".")
  })

  test("tokenize plus binary operator", () => {
    let tokens = Parser.tokenize("1 + 2")
    let tokenStrings = Parser.printTokens(tokens)
    expect(tokenStrings)->toContain("1")
    expect(tokenStrings)->toContain("+")
    expect(tokenStrings)->toContain("2")
    expect(tokenStrings)->toContain("EOF")
  })

  test("tokenize plus minus operator", () => {
    let tokens = Parser.tokenize("5 - 2")
    let tokenStrings = Parser.printTokens(tokens)
    expect(tokenStrings)->toContain("5")
    expect(tokenStrings)->toContain("-")
    expect(tokenStrings)->toContain("2")
    expect(tokenStrings)->toContain("EOF")
  })

  test("parse simple integer", () => {
    let expr = Parser.parse("42")
    let printed = Compile.Print.printLam(expr)
    expect(printed)->toBe("42")
  })

  test("parse plus binary operator", () => {
    let expr = Parser.parse("1 + 2")
    let printed = Compile.Print.printLam(expr)
    expect(printed)->toBe("(1 + 2)")
  })

  test("parse minus binary operator", () => {
    let expr = Parser.parse("5 - 2")
    let printed = Compile.Print.printLam(expr)
    expect(printed)->toBe("(5 - 2)")
  })

  test("parse complex binary operators", () => {
    let expr = Parser.parse("10 - 5 + 2")
    let printed = Compile.Print.printLam(expr)
    expect(printed)->toBe("((10 - 5) + 2)")
  })

  test("parse simple variable", () => {
    let expr = Parser.parse("x")
    let printed = Compile.Print.printLam(expr)
    expect(printed)->toBe("x")
  })

  test("parse identity function", () => {
    let expr = Parser.parse("λx.x")
    let printed = Compile.Print.printLam(expr)
    expect(printed)->toBe("(λx. x)")
  })

  test("parse parenthesized expression", () => {
    let expr = Parser.parse("(x)")
    let printed = Compile.Print.printLam(expr)
    expect(printed)->toBe("x")
  })

  test("parse and compile identity function", () => {
    let compiled = Parser.parseAndCompile("λx.x")
    let printed = Compile.Print.printANF(compiled)
    expect(printed)->toContain("fun")
    expect(printed)->toContain("halt")
  })

  test("parse and compile to LLVM", () => {
    let llvm = Parser.parseAndCompileToLLVM("42", 1)
    expect(llvm)->toContain("ret i64 42")
    expect(llvm)->toContain("define i64 @main()")
  })

  test("parse and compile lambda to LLVM", () => {
    let llvm = Parser.parseAndCompileToLLVM("λx.x", 2)
    expect(llvm)->toContain("define i64")
    expect(llvm)->toContain("ret i64")
  })

  test("integration with corrected hoisting - no join points", () => {
    // This test verifies that the corrected hoisting eliminates join points
    let compiled = Parser.parseAndCompile("λx.x")
    let printed = Compile.Print.printANF(compiled)

    // Should contain function definition
    expect(printed)->toContain("fun")

    // Should NOT contain join points (corrected hoisting eliminates them)
    expect(printed)->not->toContain("join")
    expect(printed)->not->toContain("jump")
  })

  test("parser error handling", () => {
    expect(() => Parser.parse("λ.x"))->toThrow // Missing parameter
    expect(() => Parser.parse("λx"))->toThrow  // Missing dot and body
    expect(() => Parser.parse("(x"))->toThrow  // Unmatched paren
  })

  test("complex expression parsing", () => {
    // Test that we can parse and the result makes sense
    let expr = Parser.parse("λf.λx.x")
    let printed = Compile.Print.printLam(expr)
    expect(printed)->toContain("λf")
    expect(printed)->toContain("λx")
  })
})

// Integration test with the full pipeline
describe("Parser Integration with Corrected Hoisting", () => {
  test("end-to-end: parse -> compile -> LLVM with corrected hoisting", () => {
    // This demonstrates that the parser works with the corrected hoisting
    let input = "λx.x"

    // Parse
    let expr = Parser.parse(input)
    expect(Compile.Print.printLam(expr))->toBe("(λx. x)")

    // Compile with corrected hoisting
    let compiled = Compile.Compiler.compile(expr)
    let anfPrinted = Compile.Print.printANF(compiled)

    // Verify hoisting worked correctly (no join points)
    expect(anfPrinted)->not->toContain("join")
    expect(anfPrinted)->not->toContain("jump")

    // Generate LLVM IR
    let llvm = Compile.Compiler.compileToLLVM(expr, 2)
    expect(llvm)->toContain("define i64")
    expect(llvm)->toContain("ret i64")

    Console.log("✅ End-to-end test passed: Parser + Corrected Hoisting + LLVM")
  })

  test("parser enables text-based lambda calculus programming", () => {
    // Show that we can now write lambda calculus as text and compile it
    let programs = [
      ("42", "Simple integer"),
      ("x", "Variable"),
      ("λx.x", "Identity function"),
      ("λf.λx.x", "Nested lambda"),
    ]

    programs->Array.forEach(((program, description)) => {
      Console.log(`Testing: ${description} - "${program}"`)

      // Parse and compile
      let expr = Parser.parse(program)
      let compiled = Compile.Compiler.compile(expr)

      // Verify no join points (corrected hoisting)
      let printed = Compile.Print.printANF(compiled)
      expect(printed)->not->toContain("join")

      Console.log(`✅ ${description} compiled successfully`)
    })
  })
})
