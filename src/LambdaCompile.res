// https://compiler.club/compiling-lambda-calculus/
type deBruijnIndex = int
type depth = int

type varName = string
type bop =
  | Plus
  | Minus

module Lam = {
  // Term
  type rec t =
    // Integer
    | Int(int)
    // variable
    | Var(varName)
    // lambda abstraction
    | Lam(varName, t)
    // application
    | App(t, t)
    // Binary operation
    | Bop(bop, t, t)
    | If(t, t, t)

  exception NoRuleApplies(t)

  let c = ref(-1)
  let fresh = str => {
    c := c.contents + 1
    str ++ Int.toString(c.contents)
  }

  let rename = {
    let rec go = env => t => {
      switch t {
      | Int(i) => Int(i)
      | Var(name) => {
          switch Belt.Map.String.get(env, name) {
          | Some(name') => Var(name')
          | None => raise(NoRuleApplies(t))
          }
        }
      | Lam(name, t1) => {
          let name' = fresh(name)
          let env' = Belt.Map.String.set(env, name, name')
          Lam(name', go(env')(t1))
        }
      | App(t1, t2) => App(go(env)(t1), go(env)(t2))
      | Bop(op, t1, t2) => Bop(op, go(env)(t1), go(env)(t2))
      | If(t1, t2, t3) => If(go(env)(t1), go(env)(t2), go(env)(t3))
      }
    }
    let env = Belt.Map.String.empty
    go(env)
  }
}

module ANF = {
  // atoms
  type atom =
    | AtomInt(int)
    | AtomVar(varName)
    | AtomGlob(varName)

  // ANF representation
  type rec t =
    | Halt(atom)
    | Fun(varName, list<varName>, t, t)
    | Join(varName, option<varName>, t, t)
    | Jump(varName, option<atom>)
    | App(varName, varName, list<atom>, t)
    | Bop(varName, bop, atom, atom, t)
    | If(atom, t, t)
    | Tuple(varName, list<atom>, t)
    | Proj(varName, varName, int, t)

  // Helper function to create Halt
  let mkHalt = (v: atom) => Halt(v)

  // let* operator for continuation-passing style
  let letStar = (f, k) => f(k)

  // ANF conversion algorithm
  let convert = {
    let rec go = (e: Lam.t, k: atom => t): t => {
      switch e {
      | Int(i) => k(AtomInt(i))
      | Var(x) => k(AtomVar(x))
      | Lam(x, t) => {
          let f = Lam.fresh("f")
          let t' = go(t, v => mkHalt(v))
          Fun(f, list{x}, t', k(AtomVar(f)))
        }
      | App(f, x) => {
          letStar(go(f, _), fAtom => {
            letStar(go(x, _), xAtom => {
              switch fAtom {
              | AtomVar(fVar) => {
                  let r = Lam.fresh("r")
                  App(r, fVar, list{xAtom}, k(AtomVar(r)))
                }
              | _ => failwith("Must apply named value!")
              }
            })
          })
        }
      | Bop(op, x, y) => {
          letStar(go(x, _), xAtom => {
            letStar(go(y, _), yAtom => {
              let r = Lam.fresh("r")
              Bop(r, op, xAtom, yAtom, k(AtomVar(r)))
            })
          })
        }
      | If(e, t, f) => {
          letStar(go(e, _), eAtom => {
            let j = Lam.fresh("j")
            let p = Lam.fresh("p")
            let joinVar = Jump(j, Some(AtomVar(p)))
            Join(j, Some(p), k(AtomVar(p)), If(eAtom, go(t, _ => joinVar), go(f, _ => joinVar)))
          })
        }
      }
    }

    // Entry point for conversion
    (e: Lam.t) => go(e, mkHalt)
  }
}

// Free variables computation
module FreeVars = {
  let rec compute = (t: ANF.t): Belt.Set.String.t => {
    switch t {
    | Halt(AtomVar(x)) => Belt.Set.String.fromArray([x])
    | Halt(_) => Belt.Set.String.empty
    | Fun(f, xs, e, e') => {
        let fvE = compute(e)
        let fvE' = compute(e')
        let bound = Belt.Set.String.fromArray(Array.concat([f], List.toArray(xs)))
        Belt.Set.String.union(
          Belt.Set.String.diff(fvE, bound),
          Belt.Set.String.diff(fvE', Belt.Set.String.fromArray([f])),
        )
      }
    | Join(j, Some(p), e, e') => {
        let fvE = compute(e)
        let fvE' = compute(e')
        let bound = Belt.Set.String.fromArray([j, p])
        Belt.Set.String.union(
          Belt.Set.String.diff(fvE, bound),
          Belt.Set.String.diff(fvE', bound),
        )
      }
    | Join(j, None, e, e') => {
        let fvE = compute(e)
        let fvE' = compute(e')
        let bound = Belt.Set.String.fromArray([j])
        Belt.Set.String.union(
          Belt.Set.String.diff(fvE, bound),
          Belt.Set.String.diff(fvE', bound),
        )
      }
    | Jump(j, Some(AtomVar(x))) => Belt.Set.String.fromArray([j, x])
    | Jump(j, Some(_)) => Belt.Set.String.fromArray([j])
    | Jump(j, None) => Belt.Set.String.fromArray([j])
    | App(r, f, vs, e) => {
        let atomVars =
          vs
          ->List.map(atom =>
            switch atom {
            | AtomVar(x) => [x]
            | _ => []
            }
          )
          ->List.toArray
          ->Array.flat
        let fvE = compute(e)
        Belt.Set.String.union(
          Belt.Set.String.fromArray(Array.concat([f], atomVars)),
          Belt.Set.String.diff(fvE, Belt.Set.String.fromArray([r])),
        )
      }
    | Bop(r, _, x, y, e) => {
        let atomVars =
          [x, y]
          ->Array.map(atom =>
            switch atom {
            | AtomVar(x) => [x]
            | _ => []
            }
          )
          ->Array.flat
        let fvE = compute(e)
        Belt.Set.String.union(
          Belt.Set.String.fromArray(atomVars),
          Belt.Set.String.diff(fvE, Belt.Set.String.fromArray([r])),
        )
      }
    | If(AtomVar(x), t, f) => {
        Belt.Set.String.union(
          Belt.Set.String.fromArray([x]),
          Belt.Set.String.union(compute(t), compute(f)),
        )
      }
    | If(_, t, f) => Belt.Set.String.union(compute(t), compute(f))
    | Tuple(r, vs, e) => {
        let atomVars =
          vs
          ->List.map(atom =>
            switch atom {
            | AtomVar(x) => [x]
            | _ => []
            }
          )
          ->List.toArray
          ->Array.flat
        let fvE = compute(e)
        Belt.Set.String.union(
          Belt.Set.String.fromArray(atomVars),
          Belt.Set.String.diff(fvE, Belt.Set.String.fromArray([r])),
        )
      }
    | Proj(r, x, _, e) => {
        let fvE = compute(e)
        Belt.Set.String.union(
          Belt.Set.String.fromArray([x]),
          Belt.Set.String.diff(fvE, Belt.Set.String.fromArray([r])),
        )
      }
    }
  }
}

// Closure conversion
module ClosureConversion = {
  let convert = {
    let rec go = (t: ANF.t): ANF.t => {
      switch t {
      | Fun(f, xs, e, e') => {
          let env = Lam.fresh("env")
          let fvs = FreeVars.compute(e)->Belt.Set.String.toArray->List.fromArray

          // Create projections for free variables in the function body
          let rec addProjections = (body, fvList, index) => {
            switch fvList {
            | list{} => body
            | list{x, ...rest} => ANF.Proj(x, env, index, addProjections(body, rest, index + 1))
            }
          }

          let transformedBody = addProjections(go(e), fvs, 1)
          let vs = List.map(fvs, x => ANF.AtomVar(x))
          let closureTuple = ANF.Tuple(f, list{ANF.AtomGlob(f), ...vs}, go(e'))
          ANF.Fun(f, list{env, ...xs}, transformedBody, closureTuple)
        }
      | App(r, f, vs, e) => {
          let ptr = Lam.fresh("f")
          ANF.Proj(ptr, f, 0, ANF.App(r, ptr, list{ANF.AtomVar(f), ...vs}, go(e)))
        }
      | Join(j, p, e, e') => ANF.Join(j, p, go(e), go(e'))
      | Bop(r, op, x, y, e) => ANF.Bop(r, op, x, y, go(e))
      | If(x, t, f) => ANF.If(x, go(t), go(f))
      | Tuple(r, vs, e) => ANF.Tuple(r, vs, go(e))
      | Proj(r, x, i, e) => ANF.Proj(r, x, i, go(e))
      | Halt(_) | Jump(_, _) => t
      }
    }
    go
  }
}

// Hoisting transformation
module Hoisting = {
  // Collect all function declarations and variable bindings
  type binding =
    | FunBinding(varName, list<varName>, ANF.t)
    | VarBinding(varName, ANF.atom)
    | TupleBinding(varName, list<ANF.atom>)

  let rec collectBindings = (t: ANF.t): (list<binding>, ANF.t) => {
    switch t {
    | Fun(f, xs, e, e') => {
        let (bindings1, hoistedE) = collectBindings(e)
        let (bindings2, hoistedE') = collectBindings(e')
        let funBinding = FunBinding(f, xs, hoistedE)
        (list{funBinding, ...List.concat(bindings1, bindings2)}, hoistedE')
      }
    | App(r, f, vs, e) => {
        let (bindings, hoistedE) = collectBindings(e)
        (bindings, ANF.App(r, f, vs, hoistedE))
      }
    | Bop(r, op, x, y, e) => {
        let (bindings, hoistedE) = collectBindings(e)
        let varBinding = VarBinding(r, ANF.AtomVar(r))
        (list{varBinding, ...bindings}, ANF.Bop(r, op, x, y, hoistedE))
      }
    | Tuple(r, vs, e) => {
        let (bindings, hoistedE) = collectBindings(e)
        let tupleBinding = TupleBinding(r, vs)
        (list{tupleBinding, ...bindings}, hoistedE)
      }
    | Proj(r, x, _i, e) => {
        let (bindings, hoistedE) = collectBindings(e)
        let varBinding = VarBinding(r, ANF.AtomVar(x))
        (list{varBinding, ...bindings}, hoistedE)
      }
    | Join(j, p, e, e') => {
        let (bindings1, hoistedE) = collectBindings(e)
        let (bindings2, hoistedE') = collectBindings(e')
        (List.concat(bindings1, bindings2), ANF.Join(j, p, hoistedE, hoistedE'))
      }
    | If(atom, t, f) => {
        let (bindings1, hoistedT) = collectBindings(t)
        let (bindings2, hoistedF) = collectBindings(f)
        (List.concat(bindings1, bindings2), ANF.If(atom, hoistedT, hoistedF))
      }
    | Halt(_) | Jump(_, _) => (list{}, t)
    }
  }

  let rec reconstructWithBindings = (bindings: list<binding>, body: ANF.t): ANF.t => {
    switch bindings {
    | list{} => body
    | list{FunBinding(f, xs, e), ...rest} => {
        let restBody = reconstructWithBindings(rest, body)
        ANF.Fun(f, xs, e, restBody)
      }
    | list{VarBinding(_r, atom), ...rest} => {
        let restBody = reconstructWithBindings(rest, body)
        // For variable bindings, we need to create appropriate ANF constructs
        switch atom {
        | ANF.AtomVar(_) => restBody // Skip simple variable bindings
        | _ => restBody
        }
      }
    | list{TupleBinding(r, vs), ...rest} => {
        let restBody = reconstructWithBindings(rest, body)
        ANF.Tuple(r, vs, restBody)
      }
    }
  }

  let hoist = (t: ANF.t): ANF.t => {
    let (bindings, body) = collectBindings(t)
    // Group bindings by type - functions first, then variables
    let (funBindings, otherBindings) = List.partition(bindings, binding =>
      switch binding {
      | FunBinding(_, _, _) => true
      | _ => false
      }
    )
    let orderedBindings = List.concat(funBindings, otherBindings)
    reconstructWithBindings(orderedBindings, body)
  }
}

// Pretty printing functions
module Print = {
  let printAtom = (atom: ANF.atom): string => {
    switch atom {
    | AtomInt(i) => Int.toString(i)
    | AtomVar(x) => x
    | AtomGlob(x) => `@${x}`
    }
  }

  let rec printANF = (t: ANF.t): string => {
    switch t {
    | Halt(atom) => `halt ${printAtom(atom)}`
    | Fun(f, xs, e, e') => {
        let params = xs->List.toArray->Array.joinWith(", ")
        `fun ${f}(${params}) =\n  ${printANF(e)}\nin\n${printANF(e')}`
      }
    | Join(j, Some(p), e, e') => `join ${j}(${p}) =\n  ${printANF(e)}\nin\n${printANF(e')}`
    | Join(j, None, e, e') => `join ${j} =\n  ${printANF(e)}\nin\n${printANF(e')}`
    | Jump(j, Some(atom)) => `jump ${j}(${printAtom(atom)})`
    | Jump(j, None) => `jump ${j}`
    | App(r, f, vs, e) => {
        let args = vs->List.map(printAtom)->List.toArray->Array.joinWith(", ")
        `let ${r} = ${f}(${args}) in\n${printANF(e)}`
      }
    | Bop(r, Plus, x, y, e) => `let ${r} = ${printAtom(x)} + ${printAtom(y)} in\n${printANF(e)}`
    | Bop(r, Minus, x, y, e) => `let ${r} = ${printAtom(x)} - ${printAtom(y)} in\n${printANF(e)}`
    | If(atom, t, f) => `if ${printAtom(atom)} then\n  ${printANF(t)}\nelse\n  ${printANF(f)}`
    | Tuple(r, vs, e) => {
        let values = vs->List.map(printAtom)->List.toArray->Array.joinWith(", ")
        `let ${r} = (${values}) in\n${printANF(e)}`
      }
    | Proj(r, x, i, e) => `let ${r} = ${x}.${Int.toString(i)} in\n${printANF(e)}`
    }
  }

  let rec printLam = (t: Lam.t): string => {
    switch t {
    | Int(i) => Int.toString(i)
    | Var(x) => x
    | Lam(x, t) => `(λ${x}. ${printLam(t)})`
    | App(t1, t2) => `(${printLam(t1)} ${printLam(t2)})`
    | Bop(Plus, t1, t2) => `(${printLam(t1)} + ${printLam(t2)})`
    | Bop(Minus, t1, t2) => `(${printLam(t1)} - ${printLam(t2)})`
    | If(t1, t2, t3) => `if ${printLam(t1)} then ${printLam(t2)} else ${printLam(t3)}`
    }
  }
}

// LLVMlite Lowering
module LLVMLowering = {
  // Phase 1: Basic arithmetic operations and primitives
  let lowerPhase1 = (anf: ANF.t): string => {
    let instructions = ref(list{})

    let rec go = (t: ANF.t) => {
      switch t {
      | Halt(AtomInt(n)) =>
          instructions := list{`ret i64 ${Int.toString(n)}`, ...instructions.contents}
      | Halt(AtomVar(x)) =>
          instructions := list{`ret i64 %${x}`, ...instructions.contents}
      | Bop(r, Plus, AtomInt(x), AtomInt(y), e) => {
          instructions := list{`%${r} = add i64 ${Int.toString(x)}, ${Int.toString(y)}`, ...instructions.contents}
          go(e)
        }
      | Bop(r, Minus, AtomInt(x), AtomInt(y), e) => {
          instructions := list{`%${r} = sub i64 ${Int.toString(x)}, ${Int.toString(y)}`, ...instructions.contents}
          go(e)
        }
      | Bop(r, Plus, AtomVar(x), AtomInt(y), e) => {
          instructions := list{`%${r} = add i64 %${x}, ${Int.toString(y)}`, ...instructions.contents}
          go(e)
        }
      | Bop(r, Plus, AtomInt(x), AtomVar(y), e) => {
          instructions := list{`%${r} = add i64 ${Int.toString(x)}, %${y}`, ...instructions.contents}
          go(e)
        }
      | Bop(r, Plus, AtomVar(x), AtomVar(y), e) => {
          instructions := list{`%${r} = add i64 %${x}, %${y}`, ...instructions.contents}
          go(e)
        }
      | Bop(r, Minus, AtomVar(x), AtomInt(y), e) => {
          instructions := list{`%${r} = sub i64 %${x}, ${Int.toString(y)}`, ...instructions.contents}
          go(e)
        }
      | Bop(r, Minus, AtomInt(x), AtomVar(y), e) => {
          instructions := list{`%${r} = sub i64 ${Int.toString(x)}, %${y}`, ...instructions.contents}
          go(e)
        }
      | Bop(r, Minus, AtomVar(x), AtomVar(y), e) => {
          instructions := list{`%${r} = sub i64 %${x}, %${y}`, ...instructions.contents}
          go(e)
        }
      | _ => failwith("Phase 1: Unsupported ANF construct")
      }
    }

    go(anf)

    let body = instructions.contents->List.reverse->List.toArray->Array.joinWith("\n  ")
    `define i64 @main() {\nentry:\n  ${body}\n}`
  }

  // Helper function to convert atom to string representation
  let atomToString = (atom: ANF.atom): string => {
    switch atom {
    | AtomInt(i) => Int.toString(i)
    | AtomVar(x) => `%${x}`
    | AtomGlob(x) => `@${x}`
    }
  }

  // Phase 2: Function definitions and calls
  let lowerPhase2 = (anf: ANF.t): string => {
    let functions = ref(list{})
    let mainInstructions = ref(list{})

    let rec extractFunctions = (t: ANF.t) => {
      switch t {
      | Fun(f, params, body, cont) => {
          // Generate function definition
          let paramList = params->List.map(p => `i64 %${p}`)->List.toArray->Array.joinWith(", ")
          let bodyInstructions = ref(list{})

          let rec generateBody = (bodyTerm: ANF.t) => {
            switch bodyTerm {
            | Halt(AtomInt(n)) =>
                bodyInstructions := list{`ret i64 ${Int.toString(n)}`, ...bodyInstructions.contents}
            | Halt(AtomVar(x)) =>
                bodyInstructions := list{`ret i64 %${x}`, ...bodyInstructions.contents}
            | Bop(r, Plus, x, y, e) => {
                bodyInstructions := list{`%${r} = add i64 ${atomToString(x)}, ${atomToString(y)}`, ...bodyInstructions.contents}
                generateBody(e)
              }
            | Bop(r, Minus, x, y, e) => {
                bodyInstructions := list{`%${r} = sub i64 ${atomToString(x)}, ${atomToString(y)}`, ...bodyInstructions.contents}
                generateBody(e)
              }
            | _ => failwith("Phase 2: Unsupported construct in function body")
            }
          }

          generateBody(body)

          let bodyStr = bodyInstructions.contents->List.reverse->List.toArray->Array.joinWith("\n  ")
          let funcDef = `define i64 @${f}(${paramList}) {\nentry:\n  ${bodyStr}\n}`

          functions := list{funcDef, ...functions.contents}
          extractFunctions(cont)
        }
      | Halt(AtomVar(x)) => {
          // Variable reference - could be function call result or function reference
          mainInstructions := list{`ret i64 %${x}`, ...mainInstructions.contents}
        }
      | Halt(AtomInt(n)) =>
          mainInstructions := list{`ret i64 ${Int.toString(n)}`, ...mainInstructions.contents}
      | App(r, f, args, e) => {
          // Simple direct function call (Phase 2 - no closures yet)
          let argList = args->List.map(atom => {
            switch atom {
            | AtomInt(i) => `i64 ${Int.toString(i)}`
            | AtomVar(x) => `i64 %${x}`
            | AtomGlob(x) => `i64 @${x}`
            }
          })->List.toArray->Array.joinWith(", ")
          mainInstructions := list{`%${r} = call i64 @${f}(${argList})`, ...mainInstructions.contents}
          extractFunctions(e)
        }
      | Bop(r, Plus, x, y, e) => {
          mainInstructions := list{`%${r} = add i64 ${atomToString(x)}, ${atomToString(y)}`, ...mainInstructions.contents}
          extractFunctions(e)
        }
      | Bop(r, Minus, x, y, e) => {
          mainInstructions := list{`%${r} = sub i64 ${atomToString(x)}, ${atomToString(y)}`, ...mainInstructions.contents}
          extractFunctions(e)
        }
      | _ => failwith("Phase 2: Unsupported ANF construct")
      }
    }

    extractFunctions(anf)

    let functionsStr = functions.contents->List.reverse->List.toArray->Array.joinWith("\n\n")
    let mainBody = mainInstructions.contents->List.reverse->List.toArray->Array.joinWith("\n  ")
    let mainFunc = `define i64 @main() {\nentry:\n  ${mainBody}\n}`

    if List.length(functions.contents) > 0 {
      `${functionsStr}\n\n${mainFunc}`
    } else {
      mainFunc
    }
  }
}

module Compiler = {
  let compile = (term: Lam.t) => {
    term->Lam.rename->ANF.convert->ClosureConversion.convert->Hoisting.hoist
  }
}

// Test cases
let testLambda = Lam.Lam("x", Lam.Var("x"))
let testApp = Lam.App(Lam.Lam("x", Lam.Var("x")), Lam.Int(42))
let testBop = Lam.Bop(Plus, Lam.Int(3), Lam.Int(4))
let testIf = Lam.If(Lam.Int(1), Lam.Int(2), Lam.Int(3))

// Complex test cases to reveal potential ordering issues
// Test case 1: Nested functions with free variables
let testNested = Lam.Lam("x",
  Lam.App(
    Lam.Lam("y",
      Lam.App(
        Lam.Lam("z", Lam.Bop(Plus, Lam.Var("x"), Lam.Bop(Plus, Lam.Var("y"), Lam.Var("z")))),
        Lam.Int(3)
      )
    ),
    Lam.Int(2)
  )
)

// Test case 2: Function returning another function (currying)
let testCurried = Lam.Lam("x",
  Lam.Lam("y",
    Lam.Bop(Plus, Lam.Var("x"), Lam.Var("y"))
  )
)

// Test case 3: Complex free variable dependencies
let testComplexFreeVars = Lam.App(
  Lam.Lam("a",
    Lam.App(
      Lam.Lam("b",
        Lam.App(
          Lam.Lam("c",
            Lam.Bop(Plus,
              Lam.Var("a"),
              Lam.Bop(Plus, Lam.Var("b"), Lam.Var("c"))
            )
          ),
          Lam.Bop(Plus, Lam.Var("a"), Lam.Var("b"))
        )
      ),
      Lam.Bop(Plus, Lam.Var("a"), Lam.Int(1))
    )
  ),
  Lam.Int(10)
)

// Test case 4: Conditional with nested functions
let testConditionalNested = Lam.If(
  Lam.Int(1),
  Lam.Lam("x", Lam.Bop(Plus, Lam.Var("x"), Lam.Int(1))),
  Lam.Lam("y", Lam.Bop(Minus, Lam.Var("y"), Lam.Int(1)))
)

Console.log("=== Original Lambda Terms ===")
Console.log2("Identity function:", Print.printLam(testLambda))
Console.log2("Application:", Print.printLam(testApp))
Console.log2("Binary operation:", Print.printLam(testBop))
Console.log2("If expression:", Print.printLam(testIf))

Console.log("\n=== After Alpha Renaming ===")
let renamedLambda = Lam.rename(testLambda)
let renamedApp = Lam.rename(testApp)
let renamedBop = Lam.rename(testBop)
let renamedIf = Lam.rename(testIf)

Console.log2("Identity function:", Print.printLam(renamedLambda))
Console.log2("Application:", Print.printLam(renamedApp))
Console.log2("Binary operation:", Print.printLam(renamedBop))
Console.log2("If expression:", Print.printLam(renamedIf))

Console.log("\n=== After ANF Conversion ===")
let anfLambda = ANF.convert(renamedLambda)
let anfApp = ANF.convert(renamedApp)
let anfBop = ANF.convert(renamedBop)
let anfIf = ANF.convert(renamedIf)

Console.log2("Identity function:", Print.printANF(anfLambda))
Console.log2("Application:", Print.printANF(anfApp))
Console.log2("Binary operation:", Print.printANF(anfBop))
Console.log2("If expression:", Print.printANF(anfIf))

Console.log("\n=== After Closure Conversion ===")
let closureLambda = ClosureConversion.convert(anfLambda)
let closureApp = ClosureConversion.convert(anfApp)
let closureBop = ClosureConversion.convert(anfBop)
let closureIf = ClosureConversion.convert(anfIf)

Console.log2("Identity function:", Print.printANF(closureLambda))
Console.log2("Application:", Print.printANF(closureApp))
Console.log2("Binary operation:", Print.printANF(closureBop))
Console.log2("If expression:", Print.printANF(closureIf))

Console.log("\n=== After Hoisting (Correct Order) ===")
let hoistedLambda = Hoisting.hoist(closureLambda)
let hoistedApp = Hoisting.hoist(closureApp)
let hoistedBop = Hoisting.hoist(closureBop)
let hoistedIf = Hoisting.hoist(closureIf)

Console.log2("Identity function:", Print.printANF(hoistedLambda))
Console.log2("Application:", Print.printANF(hoistedApp))
Console.log2("Binary operation:", Print.printANF(hoistedBop))
Console.log2("If expression:", Print.printANF(hoistedIf))

Console.log("\n=== Complete Compilation Pipeline ===")
let compiledLambda = Compiler.compile(testLambda)
let compiledApp = Compiler.compile(testApp)
let compiledBop = Compiler.compile(testBop)
let compiledIf = Compiler.compile(testIf)

Console.log2("Identity function:", Print.printANF(compiledLambda))
Console.log2("Application:", Print.printANF(compiledApp))
Console.log2("Binary operation:", Print.printANF(compiledBop))
Console.log2("If expression:", Print.printANF(compiledIf))

Console.log("\n=== Complex Test Cases ===")
Console.log("--- Original Complex Lambda Terms ---")
Console.log2("Nested functions:", Print.printLam(testNested))
Console.log2("Curried function:", Print.printLam(testCurried))
Console.log2("Complex free vars:", Print.printLam(testComplexFreeVars))
Console.log2("Conditional nested:", Print.printLam(testConditionalNested))

Console.log("\n--- After Alpha Renaming ---")
let renamedNested = Lam.rename(testNested)
let renamedCurried = Lam.rename(testCurried)
let renamedComplexFreeVars = Lam.rename(testComplexFreeVars)
let renamedConditionalNested = Lam.rename(testConditionalNested)

Console.log2("Nested functions:", Print.printLam(renamedNested))
Console.log2("Curried function:", Print.printLam(renamedCurried))
Console.log2("Complex free vars:", Print.printLam(renamedComplexFreeVars))
Console.log2("Conditional nested:", Print.printLam(renamedConditionalNested))

Console.log("\n--- After ANF Conversion ---")
let anfNested = ANF.convert(renamedNested)
let anfCurried = ANF.convert(renamedCurried)
let anfComplexFreeVars = ANF.convert(renamedComplexFreeVars)
let anfConditionalNested = ANF.convert(renamedConditionalNested)

Console.log2("Nested functions:", Print.printANF(anfNested))
Console.log2("Curried function:", Print.printANF(anfCurried))
Console.log2("Complex free vars:", Print.printANF(anfComplexFreeVars))
Console.log2("Conditional nested:", Print.printANF(anfConditionalNested))

Console.log("\n--- After Closure Conversion (Correct Order) ---")
let closureNested = ClosureConversion.convert(anfNested)
let closureCurried = ClosureConversion.convert(anfCurried)
let closureComplexFreeVars = ClosureConversion.convert(anfComplexFreeVars)
let closureConditionalNested = ClosureConversion.convert(anfConditionalNested)

Console.log2("Nested functions:", Print.printANF(closureNested))
Console.log2("Curried function:", Print.printANF(closureCurried))
Console.log2("Complex free vars:", Print.printANF(closureComplexFreeVars))
Console.log2("Conditional nested:", Print.printANF(closureConditionalNested))

Console.log("\n--- After Hoisting (Correct Order) ---")
let hoistedNested = Hoisting.hoist(closureNested)
let hoistedCurried = Hoisting.hoist(closureCurried)
let hoistedComplexFreeVars = Hoisting.hoist(closureComplexFreeVars)
let hoistedConditionalNested = Hoisting.hoist(closureConditionalNested)

Console.log2("Nested functions:", Print.printANF(hoistedNested))
Console.log2("Curried function:", Print.printANF(hoistedCurried))
Console.log2("Complex free vars:", Print.printANF(hoistedComplexFreeVars))
Console.log2("Conditional nested:", Print.printANF(hoistedConditionalNested))

Console.log("\n--- Complete Compilation Pipeline (Correct Order) ---")
let finalNested = Compiler.compile(testNested)
let finalCurried = Compiler.compile(testCurried)
let finalComplexFreeVars = Compiler.compile(testComplexFreeVars)
let finalConditionalNested = Compiler.compile(testConditionalNested)

Console.log2("Nested functions:", Print.printANF(finalNested))
Console.log2("Curried function:", Print.printANF(finalCurried))
Console.log2("Complex free vars:", Print.printANF(finalComplexFreeVars))
Console.log2("Conditional nested:", Print.printANF(finalConditionalNested))

Console.log("\n=== LLVMlite Lowering Phase 1 Tests ===")

// Test 1: Simple integer halt
let testSimpleInt = ANF.Halt(ANF.AtomInt(42))
Console.log("--- Test 1: Simple integer ---")
Console.log2("ANF:", Print.printANF(testSimpleInt))
Console.log2("LLVM IR:", LLVMLowering.lowerPhase1(testSimpleInt))

// Test 2: Basic addition with integers
let testAddInts = ANF.Bop("r", Plus, ANF.AtomInt(3), ANF.AtomInt(4), ANF.Halt(ANF.AtomVar("r")))
Console.log("\n--- Test 2: Addition with integers ---")
Console.log2("ANF:", Print.printANF(testAddInts))
Console.log2("LLVM IR:", LLVMLowering.lowerPhase1(testAddInts))

// Test 3: Basic subtraction with integers
let testSubInts = ANF.Bop("s", Minus, ANF.AtomInt(10), ANF.AtomInt(3), ANF.Halt(ANF.AtomVar("s")))
Console.log("\n--- Test 3: Subtraction with integers ---")
Console.log2("ANF:", Print.printANF(testSubInts))
Console.log2("LLVM IR:", LLVMLowering.lowerPhase1(testSubInts))

// Test 4: Test the basic binary operation from existing test cases
Console.log("\n--- Test 4: Existing binary operation test ---")
Console.log2("ANF:", Print.printANF(hoistedBop))
try {
  Console.log2("LLVM IR:", LLVMLowering.lowerPhase1(hoistedBop))
} catch {
| Failure(msg) => Console.log2("Expected failure:", msg)
| _ => Console.log("Unexpected error")
}

Console.log("\n=== LLVMlite Lowering Phase 2 Tests ===")

// Test 5: Simple function definition and reference
let testSimpleFunc = ANF.Fun("identity", list{"x"}, ANF.Halt(ANF.AtomVar("x")), ANF.Halt(ANF.AtomVar("identity")))
Console.log("--- Test 5: Simple function definition ---")
Console.log2("ANF:", Print.printANF(testSimpleFunc))
Console.log2("LLVM IR:", LLVMLowering.lowerPhase2(testSimpleFunc))

// Test 6: Function with arithmetic in body
let testFuncWithArith = ANF.Fun("addOne", list{"x"},
  ANF.Bop("r", Plus, ANF.AtomVar("x"), ANF.AtomInt(1), ANF.Halt(ANF.AtomVar("r"))),
  ANF.Halt(ANF.AtomVar("addOne"))
)
Console.log("\n--- Test 6: Function with arithmetic ---")
Console.log2("ANF:", Print.printANF(testFuncWithArith))
Console.log2("LLVM IR:", LLVMLowering.lowerPhase2(testFuncWithArith))

// Test 7: Function call (simplified - without closures)
let testFuncCall = ANF.Fun("double", list{"x"},
  ANF.Bop("r", Plus, ANF.AtomVar("x"), ANF.AtomVar("x"), ANF.Halt(ANF.AtomVar("r"))),
  ANF.App("result", "double", list{ANF.AtomInt(21)}, ANF.Halt(ANF.AtomVar("result")))
)
Console.log("\n--- Test 7: Function call ---")
Console.log2("ANF:", Print.printANF(testFuncCall))
Console.log2("LLVM IR:", LLVMLowering.lowerPhase2(testFuncCall))

// Test 8: Test with existing identity function (ANF only)
Console.log("\n--- Test 8: ANF identity function test ---")
Console.log2("ANF:", Print.printANF(anfLambda))
try {
  Console.log2("LLVM IR:", LLVMLowering.lowerPhase2(anfLambda))
} catch {
| Failure(msg) => Console.log2("Expected failure (complex constructs):", msg)
| _ => Console.log("Unexpected error")
}
