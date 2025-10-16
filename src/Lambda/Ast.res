// https://compiler.club/compiling-lambda-calculus/
type varName = string
type bop =
  | Plus
  | Minus

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

exception NoVarInEnv(t)

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
        | None => raise(NoVarInEnv(t))
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

// Pretty printing function for AST
let rec printLam = (t: t): string => {
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
