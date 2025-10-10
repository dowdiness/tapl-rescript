// https://compiler.club/anf-conversion/

// Source language AST
type varName = string

type bop =
  | Plus
  | Minus
  | Times
  | Div

// Source language expressions
type rec expr =
  | Int(int)
  | Var(varName)
  | Lam(varName, expr)
  | App(expr, expr)
  | Let(varName, expr, expr)
  | Bop(bop, expr, expr)
  | If(expr, expr, expr)

// ANF language
// Atoms are simple expressions that can be evaluated without further reduction
type atom =
  | AInt(int)
  | AVar(varName)

// ANF expressions
type rec anfExpr =
  | AAtom(atom)
  | ALet(varName, anfComp, anfExpr)
  | AIf(atom, anfExpr, anfExpr)

// ANF computations (non-trivial expressions)
and anfComp =
  | ACApp(atom, atom)
  | ACBop(bop, atom, atom)
  | ACLam(varName, anfExpr)

// Fresh variable generator
let freshVar = {
  let counter = ref(0)
  () => {
    counter := counter.contents + 1
    "x" ++ Int.toString(counter.contents)
  }
}

// Convert a simple expression to an atom
let rec exprToAtom = (expr) => {
  switch expr {
    | Int(n) => (AInt(n), list{})
    | Var(x) => (AVar(x), list{})
    | e => {
        let x = freshVar()
        let (acomp, bindings) = exprToComp(e)
        (AVar(x), list{...bindings, (x, acomp)})
      }
  }
}

// Convert an expression to an ANF computation
and exprToComp = (expr) => {
  switch expr {
    | Lam(x, e) => {
        let (anf, _) = exprToANF(e)
        (ACLam(x, anf), list{})
      }
    | App(e1, e2) => {
        let (a1, bindings1) = exprToAtom(e1)
        let (a2, bindings2) = exprToAtom(e2)
        (ACApp(a1, a2), list{...bindings1, ...bindings2})
      }
    | Bop(op, e1, e2) => {
        let (a1, bindings1) = exprToAtom(e1)
        let (a2, bindings2) = exprToAtom(e2)
        (ACBop(op, a1, a2), list{...bindings1, ...bindings2})
      }
    | If(e1, e2, e3) => {
        let (a1, bindings1) = exprToAtom(e1)
        let (anf2, _) = exprToANF(e2)
        let (anf3, _) = exprToANF(e3)
        let x = freshVar()
        (ACLam(x, AIf(a1, anf2, anf3)), bindings1)
      }
    | Let(x, e1, e2) => {
        let (acomp, bindings) = exprToComp(e1)
        let (anf, _) = exprToANF(e2)
        let letExpr = ALet(x, acomp, anf)
        (ACLam("_", letExpr), bindings)
      }
    | e => {
        let (a, bindings) = exprToAtom(e)
        switch a {
          | AVar(_) => {
              // If we already have a variable, no need for a lambda
              if List.length(bindings) == 0 {
                (ACApp(a, AInt(0)), list{}) // Dummy application to maintain ANF structure
              } else {
                (ACLam("_", AAtom(a)), bindings)
              }
            }
          | _ => (ACLam("_", AAtom(a)), bindings)
        }
      }
  }
}

// Convert an expression to ANF
and exprToANF = (expr) => {
  switch expr {
    | Int(n) => (AAtom(AInt(n)), list{})
    | Var(x) => (AAtom(AVar(x)), list{})
    | If(e1, e2, e3) => {
        let (a1, bindings1) = exprToAtom(e1)
        let (anf2, _) = exprToANF(e2)
        let (anf3, _) = exprToANF(e3)
        (makeBindings(bindings1, AIf(a1, anf2, anf3)), list{})
      }
    | Let(x, e1, e2) => {
        let (acomp, bindings1) = exprToComp(e1)
        let (anf2, _) = exprToANF(e2)
        (makeBindings(bindings1, ALet(x, acomp, anf2)), list{})
      }
    | e => {
        let (a, bindings) = exprToAtom(e)
        (makeBindings(bindings, AAtom(a)), list{})
      }
  }
}

// Helper to create a sequence of let bindings
and makeBindings = (bindings, body) => {
  bindings->List.reduce(body, (acc, (x, comp)) => {
    ALet(x, comp, acc)
  })
}

// Convert an expression to ANF (public API)
let toANF = (expr) => {
  let (anfExpr, _) = exprToANF(expr)
  anfExpr
}

// Pretty printing functions
let atomToString = (atom) => {
  switch atom {
    | AInt(n) => Int.toString(n)
    | AVar(x) => x
  }
}

let bopToString = (op) => {
  switch op {
    | Plus => "+"
    | Minus => "-"
    | Times => "*"
    | Div => "/"
  }
}

let rec anfCompToString = (comp) => {
  switch comp {
    | ACApp(a1, a2) => `${atomToString(a1)} ${atomToString(a2)}`
    | ACBop(op, a1, a2) => `${atomToString(a1)} ${bopToString(op)} ${atomToString(a2)}`
    | ACLam(x, e) => `λ${x}. ${anfExprToString(e)}`
  }
}

and anfExprToString = (expr) => {
  switch expr {
    | AAtom(a) => atomToString(a)
    | ALet(x, comp, e) => `let ${x} = ${anfCompToString(comp)} in ${anfExprToString(e)}`
    | AIf(a, e1, e2) => `if ${atomToString(a)} then ${anfExprToString(e1)} else ${anfExprToString(e2)}`
  }
}

// Pretty printing for source expressions
let rec exprToString = (expr) => {
  switch expr {
    | Int(n) => Int.toString(n)
    | Var(x) => x
    | Lam(x, e) => `λ${x}. ${exprToString(e)}`
    | App(e1, e2) => `(${exprToString(e1)}) (${exprToString(e2)})`
    | Let(x, e1, e2) => `let ${x} = ${exprToString(e1)} in ${exprToString(e2)}`
    | Bop(op, e1, e2) => `(${exprToString(e1)} ${bopToString(op)} ${exprToString(e2)})`
    | If(e1, e2, e3) => `if ${exprToString(e1)} then ${exprToString(e2)} else ${exprToString(e3)}`
  }
}

// Examples
let example1 = App(Lam("x", Bop(Plus, Var("x"), Int(1))), Int(42))
let example2 = Let("x", Int(10), Bop(Plus, Var("x"), Int(5)))
let example3 = If(Bop(Plus, Int(1), Int(2)), Int(3), Int(4))
let example4 = App(App(Lam("x", Lam("y", Bop(Plus, Var("x"), Var("y")))), Int(1)), Int(2))

Console.log("Example 1:")
Console.log2("Source: ", exprToString(example1))
Console.log2("ANF: ", anfExprToString(toANF(example1)))
Console.log("")

Console.log("Example 2:")
Console.log2("Source: ", exprToString(example2))
Console.log2("ANF: ", anfExprToString(toANF(example2)))
Console.log("")

Console.log("Example 3:")
Console.log2("Source: ", exprToString(example3))
Console.log2("ANF: ", anfExprToString(toANF(example3)))
Console.log("")

Console.log("Example 4:")
Console.log2("Source: ", exprToString(example4))
Console.log2("ANF: ", anfExprToString(toANF(example4)))
