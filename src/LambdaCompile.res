// Implementation of Chapter 10 on Types and Programming Languages
type deBruijnIndex = int
type depth = int

type varName = string
type bop =
  | Plus
  | Minus

let bopToString = (op: bop) => {
  switch op {
    | Plus => "+"
    | Minus => "-"
  }
}
// Term
type rec term =
  // Integer
  | Int(int)
  // Binary operation
  | Bop(bop, term, term)
  // variable
  | Var(deBruijnIndex, depth)
  // lambda abstraction
  | Abs(varName, term)
  // application
  | App(term, term)
  | True
  | False
  | If(term, term, term)

type binding =
  | NameBind
  | VarBind

// Typing context
type context = list<(varName, binding)>

let addBinding = (ctx: context, name, bind): context => ctx->List.add((name, bind))

let getBinding = (ctx: context, n: deBruijnIndex) => {
  switch ctx->List.get(n) {
    | Some(_, binding) => Some(binding)
    | None => None
  }
}

// generate new name
let rec pickFreshName = (ctx: context, name): (context, varName) => {
  switch ctx->List.getBy(((varName, _binding)) => name == varName) {
    | Some(name, _binding) => pickFreshName(ctx, name ++ "'")
    | None => (ctx->List.add((name, NameBind)), name)
  }
}

// Find variable name by De Bruijn index
let indexToName = (ctx: context, x: deBruijnIndex) => {
  switch ctx->List.get(x) {
    | Some(name, _binding) => name
    | None => `[${String.make(x)} bad index]`
  }
}

// PrettyPrinter for term
let rec printTerm = (ctx: context, t: term) => {
  switch t {
    | Abs(name, t1) => {
      let (ctx', name') = pickFreshName(ctx, name)
      `(λ${name'}. ${printTerm(ctx', t1)})`
    }
    | App(t1, t2) => {
      `${printTerm(ctx, t1)} ${printTerm(ctx, t2)}`
    }
    | Var(x, n) => {
      if List.length(ctx) == n {
        indexToName(ctx, x)
      } else {
        `[${String.make(List.length(ctx))} ${String.make(n)} bad index]`
      }
    }
    | Int(i) => Int.toString(i)
    | Bop(op, t1, t2) => {
      `${printTerm(ctx, t1)} ${bopToString(op)} ${printTerm(ctx, t2)}`
    }
    | True => "true"
    | False => "false"
    | If(t1, t2, t3) => {
      `if ${printTerm(ctx, t1)} then ${printTerm(ctx, t2)} else ${printTerm(ctx, t3)}`
    }
  }
}

let printContext = (ctx: context) => {
  if List.length(ctx) == 0 {
    "∅"
  } else {
    ctx->List.reduce("{", (_acc, pair) => {
      let acc = ref(_acc)
      if _acc !== "{" { acc := `${_acc}, ` }
      switch pair {
        | (name, VarBind) => `${acc.contents}${name}: VarBind}`
        | (name, NameBind) => `${acc.contents}${name}: NameBind`
      }
    })->String.concat("}")
  }
}

let shift = (d: deBruijnIndex, t) => {
  let rec walk = (c, t) => {
    switch t {
      | Var(k, n) => {
        if k >= c {
          // shift it because it's a free variable
          Var(k + d, n + d)
        } else {
          // don't shift it because it's a bound variable
          Var(k, n + d)
        }
      }
      | Abs(name, t1) => Abs(name, walk(c + 1, t1))
      | App(t1, t2) => App(walk(c, t1), walk(c, t2))
      | Int(i) => Int(i)
      | Bop(op, t1, t2) => Bop(op, walk(c, t1), walk(c, t2))
      | True => True
      | False => False
      | If(t1, t2, t3) => If(walk(c, t1), walk(c, t2), walk(c, t3))
    }
  }
  walk(0, t)
}

// Substitute [j -> s]t
let subst = (j: deBruijnIndex, s, t) => {
  let rec walk = (c, t) => {
    switch t {
      | Var(k, n) => {
        if k == j + c {
          shift(c, s)
        } else {
          Var(k, n)
        }
      }
      | Abs(name, t1) => Abs(name, walk(c + 1, t1))
      | App(t1, t2) => App(walk(c, t1), walk(c, t2))
      | Int(i) => Int(i)
      | Bop(op, t1, t2) => Bop(op, walk(c, t1), walk(c, t2))
      | True => True
      | False => False
      | If(t1, t2, t3) => If(walk(c, t1), walk(c, t2), walk(c, t3))
    }
  }
  walk(0, t)
}

// α-reduction
let substTop = (s, t) => {
  shift(-1, subst(0, shift(1, s), t))
}

// Values are lambda abstraction, True, False and Int.
let isVal = (_ctx, t) => {
  switch t {
    | Abs(_, _) => true
    | True => true
    | False => true
    | Int(_) => true
    | _ => false
  }
}

exception NoRuleApplies(term)

// one step evaluation
let rec eval1 = (ctx: context, t) => {
  switch t {
    // E-AppAbs
    | App(Abs(_, t12), v2) if isVal(ctx, v2) => substTop(v2, t12)
    // E-App2
    | App(v1, t2) if isVal(ctx, v1) => {
      let t2' = eval1(ctx, t2)
      App(v1, t2')
    }
    // E-App1
    | App(t1, v2) => {
      let t1' = eval1(ctx, t1)
      App(t1', v2)
    }
    // E-Bop
    | Bop(op, v1, v2) if isVal(ctx, v1) && isVal(ctx, v2) => {
      switch (op, v1, v2) {
        | (Plus, Int(i1), Int(i2)) => Int(i1 + i2)
        | (Minus, Int(i1), Int(i2)) => Int(i1 - i2)
        | _ => raise(NoRuleApplies(t))
      }
    }
    // E-Bop2
    | Bop(op, v1, t2) if isVal(ctx, v1) => {
      let t2' = eval1(ctx, t2)
      Bop(op, v1, t2')
    }
    // E-Bop1
    | Bop(op, t1, v2) => {
      let t1' = eval1(ctx, t1)
      Bop(op, t1', v2)
    }
    // E-IfTrue
    | If(True, t2, _) => t2
    // E-IfFalse
    | If(False, _, t3) => t3
    // E-If
    | If(t1, t2, t3) => {
      let t1' = eval1(ctx, t1)
      If(t1', t2, t3)
    }
    | _ => raise(NoRuleApplies(t))
  }
}

let rec eval = (ctx, t) => {
  // print terms at each step of the evaluation
  Console.log3(
    printContext(ctx),
    "|- Term: ",
    printTerm(ctx, t),
  )
  switch isVal(ctx, t) {
    | true  => t
    | false => eval(ctx, eval1(ctx, t))
  }
}

module Lam = {
  type rec t =
    | Int(int)
    | Var(varName)
    | Lam(varName, t)
    | App(t, t)
    | Bop(bop, t, t)
    | If(t, t, t)

  exception NoRuleApplies(t)

  let c = ref (-1)
  let fresh = {
    (str) => {
      c := c.contents + 1
      str ++ Int.toString(c.contents)
    }
  }
  let rename = {
    let rec go = (env) => (t) => {
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

// https://compiler.club/compiling-lambda-calculus/
module ANF = {
  // atoms
  type atom =
    | AtomInt(int)
    | AtomVar(deBruijnIndex, depth)
    | AtomGlob(deBruijnIndex, depth)

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
}

// (λx. x true) (λy. y)
let _ = eval(list{}, App(Abs("x", Bop(Plus, Var(0, 1), Int(10))), Bop(Minus, Int(25), Int(10))))
Console.log("")

// (λx:(Bool -> Bool). (λy:Bool. x)) (λz:Bool. z)
let _ = eval(list{}, App(Abs("x", Abs("y", Var(1, 2))), Abs("z", Var(0, 1))))
Console.log("")

// (λx:(Bool -> Bool). (λy:((Bool -> Bool) -> Bool). y x)) (λz:Bool. z)
let _ = eval(list{}, App(Abs("x", Abs("y", App((Var(0, 2), Var(1, 2))))), Abs("z", Var(0, 1))))
