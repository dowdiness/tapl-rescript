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
  // atoms, these are either variables or constants
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
        // We introduce Join Point here
        // https://compiler.club/compiling-lambda-calculus/#:~:text=we%20introduce%20a-,join%20point,-%3A
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

// Hoisting transformation - CORRECTED IMPLEMENTATION
module Hoisting = {
  // Join point information
  type join_point = {
    name: varName,
    param: option<varName>,
    body: ANF.t,
  }

  // Substitute a variable with an atom in ANF term
  let rec substituteVar = (term: ANF.t, var: varName, replacement: ANF.atom): ANF.t => {
    let substAtom = (atom: ANF.atom): ANF.atom => {
      switch atom {
      | AtomVar(x) when x == var => replacement
      | _ => atom
      }
    }

    switch term {
    | Halt(atom) => Halt(substAtom(atom))
    | Fun(f, params, body, cont) => {
        // Don't substitute if var is bound by function parameters
        if List.some(params, p => p == var) || f == var {
          Fun(f, params, body, substituteVar(cont, var, replacement))
        } else {
          Fun(f, params, substituteVar(body, var, replacement), substituteVar(cont, var, replacement))
        }
      }
    | App(r, f, args, cont) => {
        let newArgs = List.map(args, substAtom)
        let newF = if f == var {
          switch replacement {
          | AtomVar(newF) => newF
          | _ => f // Can't substitute non-variable
          }
        } else {
          f
        }
        App(r, newF, newArgs, substituteVar(cont, var, replacement))
      }
    | Bop(r, op, x, y, cont) =>
        Bop(r, op, substAtom(x), substAtom(y), substituteVar(cont, var, replacement))
    | If(cond, thenBranch, elseBranch) =>
        If(substAtom(cond), substituteVar(thenBranch, var, replacement), substituteVar(elseBranch, var, replacement))
    | Tuple(r, atoms, cont) =>
        Tuple(r, List.map(atoms, substAtom), substituteVar(cont, var, replacement))
    | Proj(r, x, i, cont) => {
        let newX = if x == var {
          switch replacement {
          | AtomVar(newX) => newX
          | _ => x // Can't substitute non-variable
          }
        } else {
          x
        }
        Proj(r, newX, i, substituteVar(cont, var, replacement))
      }
    | Join(j, param, body, cont) =>
        Join(j, param, substituteVar(body, var, replacement), substituteVar(cont, var, replacement))
    | Jump(j, arg) => Jump(j, Option.map(arg, substAtom))
    }
  }

  // Collect all join points from ANF term
  let rec collectJoinPoints = (term: ANF.t): Belt.Map.String.t<join_point> => {
    let joinPoints = ref(Belt.Map.String.empty)

    let rec collect = (t: ANF.t) => {
      switch t {
      | Join(j, param, body, cont) => {
          let jp = {name: j, param: param, body: body}
          joinPoints := Belt.Map.String.set(joinPoints.contents, j, jp)
          collect(body)
          collect(cont)
        }
      | Fun(_, _, body, cont) => {
          collect(body)
          collect(cont)
        }
      | App(_, _, _, cont) => collect(cont)
      | Bop(_, _, _, _, cont) => collect(cont)
      | If(_, thenBranch, elseBranch) => {
          collect(thenBranch)
          collect(elseBranch)
        }
      | Tuple(_, _, cont) => collect(cont)
      | Proj(_, _, _, cont) => collect(cont)
      | Halt(_) | Jump(_, _) => ()
      }
    }

    collect(term)
    joinPoints.contents
  }

  // Eliminate join points by substituting jumps with inlined bodies
  let eliminateJoinPoints = (term: ANF.t): ANF.t => {
    let joinPoints = collectJoinPoints(term)

    let rec eliminate = (t: ANF.t): ANF.t => {
      switch t {
      | Jump(j, arg) => {
          switch Belt.Map.String.get(joinPoints, j) {
          | Some({body, param: Some(p), _}) => {
              switch arg {
              | Some(atom) => eliminate(substituteVar(body, p, atom))
              | None => eliminate(body) // Should not happen with Some(p)
              }
            }
          | Some({body, param: None, _}) => eliminate(body)
          | None => t // Should not happen in well-formed code
          }
        }
      | Join(_, _, _, cont) => eliminate(cont) // Remove join point, keep continuation
      | Fun(f, params, body, cont) => Fun(f, params, eliminate(body), eliminate(cont))
      | App(r, f, args, cont) => App(r, f, args, eliminate(cont))
      | Bop(r, op, x, y, cont) => Bop(r, op, x, y, eliminate(cont))
      | If(cond, thenBranch, elseBranch) => If(cond, eliminate(thenBranch), eliminate(elseBranch))
      | Tuple(r, atoms, cont) => Tuple(r, atoms, eliminate(cont))
      | Proj(r, x, i, cont) => Proj(r, x, i, eliminate(cont))
      | Halt(_) => t
      }
    }

    eliminate(term)
  }

  // Extract all functions to top level
  let extractFunctions = (term: ANF.t): (list<(varName, list<varName>, ANF.t)>, ANF.t) => {
    let functions = ref(list{})

    let rec extract = (t: ANF.t): ANF.t => {
      switch t {
      | Fun(f, params, body, cont) => {
          let hoistedBody = extract(body)
          functions := list{(f, params, hoistedBody), ...functions.contents}
          extract(cont) // Continue with rest, function is removed from main flow
        }
      | App(r, f, args, cont) => App(r, f, args, extract(cont))
      | Bop(r, op, x, y, cont) => Bop(r, op, x, y, extract(cont))
      | If(cond, thenBranch, elseBranch) => If(cond, extract(thenBranch), extract(elseBranch))
      | Tuple(r, atoms, cont) => Tuple(r, atoms, extract(cont))
      | Proj(r, x, i, cont) => Proj(r, x, i, extract(cont))
      | Join(j, param, body, cont) => Join(j, param, extract(body), extract(cont))
      | Jump(_, _) | Halt(_) => t
      }
    }

    let mainFlow = extract(term)
    (List.reverse(functions.contents), mainFlow)
  }

  // Reconstruct ANF with functions at top level
  let reconstructWithFunctions = (functions: list<(varName, list<varName>, ANF.t)>, mainFlow: ANF.t): ANF.t => {
    let rec reconstruct = (funcs: list<(varName, list<varName>, ANF.t)>, acc: ANF.t): ANF.t => {
      switch funcs {
      | list{} => acc
      | list{(f, params, body), ...rest} => Fun(f, params, body, reconstruct(rest, acc))
      }
    }
    reconstruct(functions, mainFlow)
  }

  // Main hoisting function - creates straightline code
  let hoist = (term: ANF.t): ANF.t => {
    term
    |> eliminateJoinPoints  // Step 1: Remove all Join/Jump constructs
    |> extractFunctions     // Step 2: Extract functions to top level
    |> (((functions, mainFlow)) => reconstructWithFunctions(functions, mainFlow))
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
          // Check if this is a function reference by looking at the functions list
          let isFunctionName = functions.contents->List.some(funcDef =>
            Js.String2.includes(funcDef, `@${x}(`)
          )
          if isFunctionName {
            // Function reference - return function pointer (not supported in simple Phase 2)
            failwith(`Phase 2: Function references not yet supported: ${x}`)
          } else {
            // Variable reference - return the variable value
            mainInstructions := list{`ret i64 %${x}`, ...mainInstructions.contents}
          }
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
  // Phase 3: Closures and memory management (Tuple, Proj)
  let lowerPhase3 = (anf: ANF.t): string => {
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
            | Tuple(r, vs, e) => {
                // Create tuple structure
                let size = List.length(vs)

                // Allocate memory for tuple
                bodyInstructions := list{`%${r}_ptr = alloca { ${Array.make(~length=size, "i64")->Array.joinWith(", ")} }`, ...bodyInstructions.contents}

                // Store each element
                vs->List.mapWithIndex((i, atom) => {
                  let gepInstr = `%${r}_gep${Int.toString(i)} = getelementptr { ${Array.make(~length=size, "i64")->Array.joinWith(", ")} }, { ${Array.make(~length=size, "i64")->Array.joinWith(", ")} }* %${r}_ptr, i32 0, i32 ${Int.toString(i)}`
                  let storeInstr = `store i64 ${atomToString(atom)}, i64* %${r}_gep${Int.toString(i)}`
                  bodyInstructions := list{storeInstr, gepInstr, ...bodyInstructions.contents}
                })->ignore

                // Cast to i64 for compatibility (simplified approach)
                bodyInstructions := list{`%${r} = ptrtoint { ${Array.make(~length=size, "i64")->Array.joinWith(", ")} }* %${r}_ptr to i64`, ...bodyInstructions.contents}
                generateBody(e)
              }
            | Proj(r, x, i, e) => {
                // Project from tuple - simplified approach (assume 3-element tuples)
                let ptrVar = `${x}_ptr_${r}`
                bodyInstructions := list{`%${ptrVar} = inttoptr i64 %${x} to { i64, i64, i64 }*`, ...bodyInstructions.contents}
                bodyInstructions := list{`%${r}_gep = getelementptr { i64, i64, i64 }, { i64, i64, i64 }* %${ptrVar}, i32 0, i32 ${Int.toString(i)}`, ...bodyInstructions.contents}
                bodyInstructions := list{`%${r} = load i64, i64* %${r}_gep`, ...bodyInstructions.contents}
                generateBody(e)
              }
            | _ => failwith("Phase 3: Unsupported construct in function body")
            }
          }

          generateBody(body)

          let bodyStr = bodyInstructions.contents->List.reverse->List.toArray->Array.joinWith("\n  ")
          let funcDef = `define i64 @${f}(${paramList}) {\nentry:\n  ${bodyStr}\n}`

          functions := list{funcDef, ...functions.contents}
          extractFunctions(cont)
        }
      | Halt(AtomVar(x)) => {
          // Check if this is a function reference by looking at the functions list
          let isFunctionName = functions.contents->List.some(funcDef =>
            Js.String2.includes(funcDef, `@${x}(`)
          )
          if isFunctionName {
            // Function reference - return function pointer (not supported in simple Phase 3)
            failwith(`Phase 3: Function references not yet supported: ${x}`)
          } else {
            // Variable reference - return the variable value
            mainInstructions := list{`ret i64 %${x}`, ...mainInstructions.contents}
          }
        }
      | Halt(AtomInt(n)) =>
          mainInstructions := list{`ret i64 ${Int.toString(n)}`, ...mainInstructions.contents}
      | App(r, f, args, e) => {
          // Simple direct function call (Phase 3 - no closures yet)
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
      | Tuple(r, vs, e) => {
          // Create tuple structure in main
          let size = List.length(vs)

          // Allocate memory for tuple
          mainInstructions := list{`%${r}_ptr = alloca { ${Array.make(~length=size, "i64")->Array.joinWith(", ")} }`, ...mainInstructions.contents}

          // Store each element
          vs->List.mapWithIndex((i, atom) => {
            let gepInstr = `%${r}_gep${Int.toString(i)} = getelementptr { ${Array.make(~length=size, "i64")->Array.joinWith(", ")} }, { ${Array.make(~length=size, "i64")->Array.joinWith(", ")} }* %${r}_ptr, i32 0, i32 ${Int.toString(i)}`
            let storeInstr = `store i64 ${atomToString(atom)}, i64* %${r}_gep${Int.toString(i)}`
            mainInstructions := list{storeInstr, gepInstr, ...mainInstructions.contents}
          })->ignore

          // Cast to i64 for compatibility
          mainInstructions := list{`%${r} = ptrtoint { ${Array.make(~length=size, "i64")->Array.joinWith(", ")} }* %${r}_ptr to i64`, ...mainInstructions.contents}
          extractFunctions(e)
        }
      | Proj(r, x, i, e) => {
          // Project from tuple in main - we need to know the tuple size
          // For simplicity, assume all tuples have the same structure for now
          let ptrVar = `${x}_ptr_${r}`
          mainInstructions := list{`%${ptrVar} = inttoptr i64 %${x} to { i64, i64, i64 }*`, ...mainInstructions.contents}
          mainInstructions := list{`%${r}_gep = getelementptr { i64, i64, i64 }, { i64, i64, i64 }* %${ptrVar}, i32 0, i32 ${Int.toString(i)}`, ...mainInstructions.contents}
          mainInstructions := list{`%${r} = load i64, i64* %${r}_gep`, ...mainInstructions.contents}
          extractFunctions(e)
        }
      | _ => failwith("Phase 3: Unsupported ANF construct")
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

  // Phase 4: Control flow (If, Join, Jump)
  let lowerPhase4 = (anf: ANF.t): string => {
    let functions = ref(list{})
    let mainInstructions = ref(list{})
    let labelCounter = ref(0)

    let getNextLabel = (prefix: string) => {
      labelCounter := labelCounter.contents + 1
      `${prefix}${Int.toString(labelCounter.contents)}`
    }

    // https://llvm.org/docs/LangRef.html#br-instruction
    // let br = (j) => `br label %${j}`

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
            | If(cond, thenBranch, elseBranch) => {
                let thenLabel = getNextLabel("then")
                let elseLabel = getNextLabel("else")

                // Generate condition check
                bodyInstructions := list{`%cond = icmp ne i64 ${atomToString(cond)}, 0`, ...bodyInstructions.contents}
                bodyInstructions := list{`br i1 %cond, label %${thenLabel}, label %${elseLabel}`, ...bodyInstructions.contents}

                // Generate then branch
                bodyInstructions := list{`${thenLabel}:`, ...bodyInstructions.contents}
                generateBody(thenBranch)

                // Generate else branch
                bodyInstructions := list{`${elseLabel}:`, ...bodyInstructions.contents}
                generateBody(elseBranch)
              }
            | _ => failwith("Phase 4: Unsupported construct in function body")
            }
          }

          generateBody(body)

          let bodyStr = bodyInstructions.contents->List.reverse->List.toArray->Array.joinWith("\n  ")
          let funcDef = `define i64 @${f}(${paramList}) {\nentry:\n  ${bodyStr}\n}`

          functions := list{funcDef, ...functions.contents}
          extractFunctions(cont)
        }
      | Halt(AtomVar(x)) => {
          // Check if this is a function reference by looking at the functions list
          let isFunctionName = functions.contents->List.some(funcDef =>
            Js.String2.includes(funcDef, `@${x}(`)
          )
          if isFunctionName {
            // Function reference - return function pointer (not supported in simple Phase 4)
            failwith(`Phase 4: Function references not yet supported: ${x}`)
          } else {
            // Variable reference - return the variable value
            mainInstructions := list{`ret i64 %${x}`, ...mainInstructions.contents}
          }
        }
      | Halt(AtomInt(n)) =>
          mainInstructions := list{`ret i64 ${Int.toString(n)}`, ...mainInstructions.contents}
      | If(cond, thenBranch, elseBranch) => {
          let thenLabel = getNextLabel("then")
          let elseLabel = getNextLabel("else")

          // Generate condition check
          mainInstructions := list{`%cond = icmp ne i64 ${atomToString(cond)}, 0`, ...mainInstructions.contents}
          mainInstructions := list{`br i1 %cond, label %${thenLabel}, label %${elseLabel}`, ...mainInstructions.contents}

          // Generate then branch
          mainInstructions := list{`${thenLabel}:`, ...mainInstructions.contents}
          extractFunctions(thenBranch)

          // Generate else branch
          mainInstructions := list{`${elseLabel}:`, ...mainInstructions.contents}
          extractFunctions(elseBranch)
        }
      | Bop(r, Plus, x, y, e) => {
          mainInstructions := list{`%${r} = add i64 ${atomToString(x)}, ${atomToString(y)}`, ...mainInstructions.contents}
          extractFunctions(e)
        }
      | Bop(r, Minus, x, y, e) => {
          mainInstructions := list{`%${r} = sub i64 ${atomToString(x)}, ${atomToString(y)}`, ...mainInstructions.contents}
          extractFunctions(e)
        }
      | Jump(j, None) => {
        mainInstructions := list{`br label %${j}`, ...mainInstructions.contents}
      }
      | Join(_) => failwith("Code must be straightline!")
      | _ => failwith("Phase 4: Unsupported ANF construct")
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

  let compileToLLVM = (term: Lam.t, phase: int) => {
    let anf = compile(term)
    switch phase {
    | 1 => LLVMLowering.lowerPhase1(anf)
    | 2 => LLVMLowering.lowerPhase2(anf)
    | 3 => LLVMLowering.lowerPhase3(anf)
    | 4 => LLVMLowering.lowerPhase4(anf)
    | _ => failwith("Unsupported LLVM lowering phase")
    }
  }
}
