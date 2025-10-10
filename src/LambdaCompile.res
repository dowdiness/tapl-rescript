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

module Compiler = {
  let compile = (term: Lam.t) => {
    term->Lam.rename->ANF.convert->ClosureConversion.convert
  }
}

// Test cases
let testLambda = Lam.Lam("x", Lam.Var("x"))
let testApp = Lam.App(Lam.Lam("x", Lam.Var("x")), Lam.Int(42))
let testBop = Lam.Bop(Plus, Lam.Int(3), Lam.Int(4))
let testIf = Lam.If(Lam.Int(1), Lam.Int(2), Lam.Int(3))

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
