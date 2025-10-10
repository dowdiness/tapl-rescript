// Implementation of Chapter 10 on Types and Programming Languages
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
