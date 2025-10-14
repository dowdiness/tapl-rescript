// Suppres warning that suggests shadowing the value identifier not.
@@warning("-44")

open Vitest
open Compile

// Test cases
let testLambda = Ast.Lam("x", Ast.Var("x"))
let testApp = Ast.App(Ast.Lam("x", Ast.Var("x")), Ast.Int(42))
let testBop = Ast.Bop(Plus, Ast.Int(3), Ast.Int(4))
let testIf = Ast.If(Ast.Int(1), Ast.Int(2), Ast.Int(3))

// Complex test cases
let testNested = Ast.Lam("x",
  Ast.App(
    Ast.Lam("y",
      Ast.App(
        Ast.Lam("z", Ast.Bop(Plus, Ast.Var("x"), Ast.Bop(Plus, Ast.Var("y"), Ast.Var("z")))),
        Ast.Int(3)
      )
    ),
    Ast.Int(2)
  )
)

let testCurried = Ast.Lam("x",
  Ast.Lam("y",
    Ast.Bop(Plus, Ast.Var("x"), Ast.Var("y"))
  )
)

let testComplexFreeVars = Ast.App(
  Ast.Lam("a",
    Ast.App(
      Ast.Lam("b",
        Ast.App(
          Ast.Lam("c",
            Ast.Bop(Plus,
              Ast.Var("a"),
              Ast.Bop(Plus, Ast.Var("b"), Ast.Var("c"))
            )
          ),
          Ast.Bop(Plus, Ast.Var("a"), Ast.Var("b"))
        )
      ),
      Ast.Bop(Plus, Ast.Var("a"), Ast.Int(1))
    )
  ),
  Ast.Int(10)
)

let testConditionalNested = Ast.If(
  Ast.Int(1),
  Ast.Lam("x", Ast.Bop(Plus, Ast.Var("x"), Ast.Int(1))),
  Ast.Lam("y", Ast.Bop(Minus, Ast.Var("y"), Ast.Int(1)))
)

describe("Alpha Renaming", () => {
  test("identity function", () => {
    let renamed = Ast.rename(testLambda)
    let printed = Ast.printLam(renamed)
    // Should contain lambda and renamed variable
    expect(printed)->toContain("λ")
    expect(printed)->toContain("x0") // Should have renamed variable like x0
  })

  test("application", () => {
    let renamed = Ast.rename(testApp)
    let printed = Ast.printLam(renamed)
    expect(printed)->toContain("λ")
    expect(printed)->toContain("42")
    expect(printed)->toContain("x1") // Should have renamed variables
  })

  test("binary operation", () => {
    let renamed = Ast.rename(testBop)
    let printed = Ast.printLam(renamed)
    expect(printed)->toContain("3")
    expect(printed)->toContain("4")
    expect(printed)->toContain("+")
  })

  test("if expression", () => {
    let renamed = Ast.rename(testIf)
    let printed = Ast.printLam(renamed)
    expect(printed)->toContain("if")
    expect(printed)->toContain("then")
    expect(printed)->toContain("else")
    expect(printed)->toContain("1")
    expect(printed)->toContain("2")
    expect(printed)->toContain("3")
  })

  test("nested functions preserve structure", () => {
    let renamed = Ast.rename(testNested)
    let printed = Ast.printLam(renamed)
    expect(printed)->toContain("λ")
    expect(printed)->toContain("+")
    expect(printed)->toContain("2")
    expect(printed)->toContain("3")
  })

  test("curried function maintains currying", () => {
    let renamed = Ast.rename(testCurried)
    let printed = Ast.printLam(renamed)
    // Should have two lambda abstractions
    let lambdaCount = Js.String2.split(printed, "λ")->Array.length - 1
    expect(lambdaCount)->toBe(2)
    expect(printed)->toContain("+")
  })
})

describe("ANF Conversion", () => {
  test("identity function produces function definition", () => {
    let renamed = Ast.rename(testLambda)
    let anf = ANF.convert(renamed)
    let printed = ANF.printANF(anf)
    expect(printed)->toContain("fun")
    expect(printed)->toContain("halt")
  })

  test("application produces function call", () => {
    let renamed = Ast.rename(testApp)
    let anf = ANF.convert(renamed)
    let printed = ANF.printANF(anf)
    expect(printed)->toContain("fun")
    expect(printed)->toContain("let")
    expect(printed)->toContain("42")
  })

  test("binary operation produces arithmetic", () => {
    let renamed = Ast.rename(testBop)
    let anf = ANF.convert(renamed)
    let printed = ANF.printANF(anf)
    expect(printed)->toContain("3 + 4")
    expect(printed)->toContain("halt")
  })

  test("if expression produces conditional", () => {
    let renamed = Ast.rename(testIf)
    let anf = ANF.convert(renamed)
    let printed = ANF.printANF(anf)
    expect(printed)->toContain("if")
    expect(printed)->toContain("then")
    expect(printed)->toContain("else")
    expect(printed)->toContain("join")
    expect(printed)->toContain("jump")
  })

  test("nested functions produce multiple function definitions", () => {
    let renamed = Ast.rename(testNested)
    let anf = ANF.convert(renamed)
    let printed = ANF.printANF(anf)
    // Should have multiple function definitions
    let funCount = Js.String2.split(printed, "fun ")->Array.length - 1
    expect(Belt.Int.toFloat(funCount))->toBeGreaterThan(1.0)
    expect(printed)->toContain("halt")
  })

  test("complex free variables handled correctly", () => {
    let renamed = Ast.rename(testComplexFreeVars)
    let anf = ANF.convert(renamed)
    let printed = ANF.printANF(anf)
    expect(printed)->toContain("fun")
    expect(printed)->toContain("let")
    expect(printed)->toContain("10")
  })
})

describe("Free Variables Computation", () => {
  test("simple halt", () => {
    let fvs = ClosureConversion.compute(ANF.Halt(ANF.AtomVar("x")))
    expect(Belt.Set.String.has(fvs, "x"))->toBeTruthy
  })

  test("halt with integer", () => {
    let fvs = ClosureConversion.compute(ANF.Halt(ANF.AtomInt(42)))
    expect(Belt.Set.String.size(fvs))->toBe(0)
  })

  test("binary operation", () => {
    let anf = ANF.Bop("r", Plus, ANF.AtomVar("x"), ANF.AtomVar("y"), ANF.Halt(ANF.AtomVar("r")))
    let fvs = ClosureConversion.compute(anf)
    expect(Belt.Set.String.has(fvs, "x"))->toBeTruthy
    expect(Belt.Set.String.has(fvs, "y"))->toBeTruthy
  })
})

describe("Closure Conversion", () => {
  test("identity function creates closure tuple", () => {
    let renamed = Ast.rename(testLambda)
    let anf = ANF.convert(renamed)
    let closure = ClosureConversion.convert(anf)
    let printed = ANF.printANF(closure)
    expect(printed)->toContain("fun")
    expect(printed)->toContain("env")
    expect(printed)->toContain("@") // Global function reference
  })

  test("application uses closure projection", () => {
    let renamed = Ast.rename(testApp)
    let anf = ANF.convert(renamed)
    let closure = ClosureConversion.convert(anf)
    let printed = ANF.printANF(closure)
    expect(printed)->toContain("let")
    expect(printed)->toContain(".0") // Projection
    expect(printed)->toContain("42")
  })

  test("binary operation unchanged", () => {
    let renamed = Ast.rename(testBop)
    let anf = ANF.convert(renamed)
    let closure = ClosureConversion.convert(anf)
    let printed = ANF.printANF(closure)
    expect(printed)->toContain("3 + 4")
    expect(printed)->toContain("halt")
  })

  test("nested functions create environment tuples", () => {
    let renamed = Ast.rename(testNested)
    let anf = ANF.convert(renamed)
    let closure = ClosureConversion.convert(anf)
    let printed = ANF.printANF(closure)
    expect(printed)->toContain("env")
    expect(printed)->toContain("@") // Global references
    expect(printed)->toContain("(") // Tuple creation
  })
})

describe("Hoisting", () => {
  test("identity function hoists to top level", () => {
    let renamed = Ast.rename(testLambda)
    let anf = ANF.convert(renamed)
    let closure = ClosureConversion.convert(anf)
    let hoisted = Hoisting.hoist(closure)
    let printed = ANF.printANF(hoisted)
    // Function definitions should appear before main computation
    let funIndex = Js.String2.indexOf(printed, "fun")
    let tupleIndex = Js.String2.indexOf(printed, "(")
    expect(Belt.Int.toFloat(funIndex))->toBeLessThan(Belt.Int.toFloat(tupleIndex))
  })

  test("application maintains function order", () => {
    let renamed = Ast.rename(testApp)
    let anf = ANF.convert(renamed)
    let closure = ClosureConversion.convert(anf)
    let hoisted = Hoisting.hoist(closure)
    let printed = ANF.printANF(hoisted)
    expect(printed)->toContain("fun")
    expect(printed)->toContain("let")
    expect(printed)->toContain("42")
  })

  test("binary operation structure preserved", () => {
    let renamed = Ast.rename(testBop)
    let anf = ANF.convert(renamed)
    let closure = ClosureConversion.convert(anf)
    let hoisted = Hoisting.hoist(closure)
    let printed = ANF.printANF(hoisted)
    expect(printed)->toContain("3 + 4")
    expect(printed)->toContain("halt")
  })

  test("complex expressions have proper function ordering", () => {
    let renamed = Ast.rename(testComplexFreeVars)
    let anf = ANF.convert(renamed)
    let closure = ClosureConversion.convert(anf)
    let hoisted = Hoisting.hoist(closure)
    let printed = ANF.printANF(hoisted)
    // All function definitions should come before main computation
    let lines = Js.String2.split(printed, "\n")
    let funLines = lines->Array.filter(line => Js.String2.includes(line, "fun "))
    expect(Belt.Int.toFloat(Array.length(funLines)))->toBeGreaterThan(0.0)
  })
})

describe("LLVM Lowering Phase 1", () => {
  test("simple integer halt", () => {
    let testSimpleInt = ANF.Halt(ANF.AtomInt(42))
    let llvm = LLVMLowering.lowerPhase1(testSimpleInt)
    expect(llvm)->toContain("ret i64 42")
    expect(llvm)->toContain("define i64 @main()")
  })

  test("basic addition with integers", () => {
    let testAddInts = ANF.Bop("r", Plus, ANF.AtomInt(3), ANF.AtomInt(4), ANF.Halt(ANF.AtomVar("r")))
    let llvm = LLVMLowering.lowerPhase1(testAddInts)
    expect(llvm)->toContain("add i64 3, 4")
    expect(llvm)->toContain("ret i64 %r")
  })

  test("basic subtraction with integers", () => {
    let testSubInts = ANF.Bop("s", Minus, ANF.AtomInt(10), ANF.AtomInt(3), ANF.Halt(ANF.AtomVar("s")))
    let llvm = LLVMLowering.lowerPhase1(testSubInts)
    expect(llvm)->toContain("sub i64 10, 3")
    expect(llvm)->toContain("ret i64 %s")
  })

  test("mixed variable and integer operations", () => {
    let testMixed = ANF.Bop("r", Plus, ANF.AtomVar("x"), ANF.AtomInt(5), ANF.Halt(ANF.AtomVar("r")))
    let llvm = LLVMLowering.lowerPhase1(testMixed)
    expect(llvm)->toContain("add i64 %x, 5")
  })
})

describe("LLVM Lowering Phase 2", () => {
  test("function call", () => {
    let testFuncCall = ANF.Fun("double", list{"x"},
      ANF.Bop("r", Plus, ANF.AtomVar("x"), ANF.AtomVar("x"), ANF.Halt(ANF.AtomVar("r"))),
      ANF.App("result", "double", list{ANF.AtomInt(21)}, ANF.Halt(ANF.AtomVar("result")))
    )
    let llvm = LLVMLowering.lowerPhase2(testFuncCall)
    expect(llvm)->toContain("define i64 @double(i64 %x)")
    expect(llvm)->toContain("call i64 @double(i64 21)")
  })

  test("function with arithmetic in body", () => {
    let testFuncWithArith = ANF.Fun("addOne", list{"x"},
      ANF.Bop("r", Plus, ANF.AtomVar("x"), ANF.AtomInt(1), ANF.Halt(ANF.AtomVar("r"))),
      ANF.Halt(ANF.AtomVar("addOne"))
    )
    expect(() => LLVMLowering.lowerPhase2(testFuncWithArith))->toThrow
  })
})

describe("LLVM Lowering Phase 3", () => {
  test("simple tuple creation and projection", () => {
    let testSimpleTuple = ANF.Tuple("t", list{ANF.AtomInt(10), ANF.AtomInt(20), ANF.AtomInt(30)},
      ANF.Proj("x", "t", 1, ANF.Halt(ANF.AtomVar("x")))
    )
    let llvm = LLVMLowering.lowerPhase3(testSimpleTuple)
    expect(llvm)->toContain("alloca { i64, i64, i64 }")
    expect(llvm)->toContain("getelementptr")
    expect(llvm)->toContain("load i64")
  })

  test("tuple with variables", () => {
    let testTupleWithVars = ANF.Bop("a", Plus, ANF.AtomInt(5), ANF.AtomInt(3),
      ANF.Bop("b", Plus, ANF.AtomInt(10), ANF.AtomInt(2),
        ANF.Tuple("t", list{ANF.AtomVar("a"), ANF.AtomVar("b"), ANF.AtomInt(100)},
          ANF.Proj("result", "t", 0, ANF.Halt(ANF.AtomVar("result")))
        )
      )
    )
    let llvm = LLVMLowering.lowerPhase3(testTupleWithVars)
    expect(llvm)->toContain("add i64 5, 3")
    expect(llvm)->toContain("add i64 10, 2")
    expect(llvm)->toContain("alloca { i64, i64, i64 }")
  })

  test("multiple projections from same tuple", () => {
    let testMultipleProj = ANF.Tuple("t", list{ANF.AtomInt(1), ANF.AtomInt(2), ANF.AtomInt(3)},
      ANF.Proj("x", "t", 0,
        ANF.Proj("y", "t", 2,
          ANF.Bop("sum", Plus, ANF.AtomVar("x"), ANF.AtomVar("y"), ANF.Halt(ANF.AtomVar("sum")))
        )
      )
    )
    let llvm = LLVMLowering.lowerPhase3(testMultipleProj)
    expect(llvm)->toContain("add i64 %x, %y")
  })
})

describe("LLVM Lowering Phase 4", () => {
  test("simple if statement", () => {
    let testSimpleIf = ANF.If(ANF.AtomInt(1), ANF.Halt(ANF.AtomInt(10)), ANF.Halt(ANF.AtomInt(20)))
    let llvm = LLVMLowering.lowerPhase4(testSimpleIf)
    expect(llvm)->toContain("icmp ne i64 1, 0")
    expect(llvm)->toContain("br i1 %cond")
    expect(llvm)->toContain("then1:")
    expect(llvm)->toContain("else2:")
  })

  test("if with computation", () => {
    let testIfWithComputation = ANF.Bop("x", Plus, ANF.AtomInt(5), ANF.AtomInt(3),
      ANF.If(ANF.AtomVar("x"),
        ANF.Bop("result1", Plus, ANF.AtomVar("x"), ANF.AtomInt(10), ANF.Halt(ANF.AtomVar("result1"))),
        ANF.Bop("result2", Minus, ANF.AtomVar("x"), ANF.AtomInt(5), ANF.Halt(ANF.AtomVar("result2")))
      )
    )
    let llvm = LLVMLowering.lowerPhase4(testIfWithComputation)
    expect(llvm)->toContain("add i64 5, 3")
    expect(llvm)->toContain("icmp ne i64 %x, 0")
  })

  test("nested if statements", () => {
    let testNestedIf = ANF.If(ANF.AtomInt(1),
      ANF.If(ANF.AtomInt(1), ANF.Halt(ANF.AtomInt(100)), ANF.Halt(ANF.AtomInt(200))),
      ANF.Halt(ANF.AtomInt(300))
    )
    let llvm = LLVMLowering.lowerPhase4(testNestedIf)
    expect(llvm)->toContain("then1:")
    expect(llvm)->toContain("else2:")
    expect(llvm)->toContain("then3:")
    expect(llvm)->toContain("else4:")
  })
})

describe("Complete Compilation Pipeline", () => {
  test("identity function compiles to proper ANF", () => {
    let compiled = Compiler.compile(testLambda)
    let printed = ANF.printANF(compiled)
    expect(printed)->toContain("fun")
    expect(printed)->toContain("env")
    expect(printed)->toContain("halt")
  })

  test("application produces function call structure", () => {
    let compiled = Compiler.compile(testApp)
    let printed = ANF.printANF(compiled)
    expect(printed)->toContain("fun")
    expect(printed)->toContain("let")
    expect(printed)->toContain("42")
    expect(printed)->toContain("@") // Global function reference
  })

  test("binary operation compiles to arithmetic", () => {
    let compiled = Compiler.compile(testBop)
    let printed = ANF.printANF(compiled)
    expect(printed)->toContain("3 + 4")
    expect(printed)->toContain("halt")
  })

  test("if expression compiles to straightline code (hoisting eliminates join points)", () => {
    let compiled = Compiler.compile(testIf)
    let printed = ANF.printANF(compiled)
    expect(printed)->toContain("if")
    // After proper hoisting, join points should be eliminated
    expect(printed)->not->toContain("join")
    expect(printed)->not->toContain("jump")
  })

  test("nested functions maintain proper structure", () => {
    let compiled = Compiler.compile(testNested)
    let printed = ANF.printANF(compiled)
    let funCount = Js.String2.split(printed, "fun ")->Array.length - 1
    expect(Belt.Int.toFloat(funCount))->toBeGreaterThan(1.0)
    expect(printed)->toContain("env")
  })

  test("curried function preserves currying", () => {
    let compiled = Compiler.compile(testCurried)
    let printed = ANF.printANF(compiled)
    expect(printed)->toContain("fun")
    expect(printed)->toContain("+")
    expect(printed)->toContain("env")
  })

  test("complex free variables handled correctly", () => {
    let compiled = Compiler.compile(testComplexFreeVars)
    let printed = ANF.printANF(compiled)
    expect(printed)->toContain("fun")
    expect(printed)->toContain("10")
    expect(printed)->toContain("env")
  })

  test("conditional nested functions compile properly (hoisting extracts functions)", () => {
    let compiled = Compiler.compile(testConditionalNested)
    let printed = ANF.printANF(compiled)
    expect(printed)->toContain("if")
    expect(printed)->toContain("fun")
    // After proper hoisting, join points should be eliminated
    expect(printed)->not->toContain("join")
  })
})

describe("Integrated Compiler Pipeline Tests", () => {
  test("complete pipeline - simple arithmetic", () => {
    let testPipelineArith = Ast.Bop(Plus, Ast.Int(10), Ast.Int(5))
    let llvm = Compiler.compileToLLVM(testPipelineArith, 1)
    expect(llvm)->toContain("add i64 10, 5")
    expect(llvm)->toContain("define i64 @main()")
  })

  test("complete pipeline - function call", () => {
    let testPipelineFunc = Ast.App(Ast.Lam("x", Ast.Bop(Plus, Ast.Var("x"), Ast.Int(1))), Ast.Int(42))
    expect(() => Compiler.compileToLLVM(testPipelineFunc, 2))->toThrow
  })

  test("complete pipeline - conditional (now works with corrected hoisting)", () => {
    let testPipelineIf = Ast.If(Ast.Int(1), Ast.Bop(Plus, Ast.Int(10), Ast.Int(5)), Ast.Bop(Minus, Ast.Int(20), Ast.Int(3)))
    // Should no longer throw because hoisting creates straightline code
    let llvm = Compiler.compileToLLVM(testPipelineIf, 4)
    expect(llvm)->toContain("icmp ne i64 1, 0")
    expect(llvm)->toContain("define i64 @main()")
  })
})

describe("Print Functions", () => {
  test("print lambda expressions with proper syntax", () => {
    let printed = Ast.printLam(testLambda)
    expect(printed)->toContain("λ")
    expect(printed)->toContain("x")
    expect(printed)->toContain(".")
  })

  test("print complex lambda expressions", () => {
    let printed = Ast.printLam(testBop)
    expect(printed)->toContain("(")
    expect(printed)->toContain(")")
    expect(printed)->toContain("3")
    expect(printed)->toContain("4")
    expect(printed)->toContain("+")
  })

  test("print ANF halt expressions", () => {
    let anf = ANF.Halt(ANF.AtomInt(42))
    let printed = ANF.printANF(anf)
    expect(printed)->toBe("halt 42")
  })

  test("print ANF binary operations", () => {
    let anf = ANF.Bop("r", Plus, ANF.AtomInt(3), ANF.AtomInt(4), ANF.Halt(ANF.AtomVar("r")))
    let printed = ANF.printANF(anf)
    expect(printed)->toContain("let r = 3 + 4 in")
    expect(printed)->toContain("halt r")
  })

  test("print atoms correctly", () => {
    let atomInt = ANF.printAtom(ANF.AtomInt(42))
    expect(atomInt)->toBe("42")

    let atomVar = ANF.printAtom(ANF.AtomVar("x"))
    expect(atomVar)->toBe("x")

    let atomGlob = ANF.printAtom(ANF.AtomGlob("f"))
    expect(atomGlob)->toBe("@f")
  })

  test("print if expressions", () => {
    let printed = Ast.printLam(testIf)
    expect(printed)->toContain("if 1 then 2 else 3")
  })
})

// test("complete pipeline - compileToLLVM", () => {
//   let anf = Compiler.compile(testIf)
//   let compiled = Compiler.compileToLLVM(testIf, 4)
//   Console.log(Ast.printLam(testIf))
//   Console.log(ANF.printANF(anf))
//   Console.log(compiled)
// })
