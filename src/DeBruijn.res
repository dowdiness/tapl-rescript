// Implementation of Chapter 7 on Types and Programming Languages
type deBruijnIndex = int
type depth = int
type varName = string
type rec term =
  // variable
  | Var(deBruijnIndex, depth)
  // lambda abstraction
  | Abs(varName, term)
  // application
  | App(term, term)

type binding = NameBind
type context = list<(varName, binding)>

// generate new name
let rec pickFreshName = (ctx: context, name): (context, varName) => {
  switch ctx->List.getBy(((varName, _binding)) => name == varName) {
    | Some(name, _binding) => pickFreshName(ctx, name ++ "'")
    | None => (ctx->List.add((name, NameBind)), name)
  }
}

// find variable name by deBruijnIndex
let indexToName = (ctx: context, x: deBruijnIndex) => {
  switch ctx->List.get(x) {
    | Some(name, _binding) => name
    | None => `[${String.make(x)} bad index]`
  }
}

// PrettyPrinter
let rec printTerm = (ctx: context, t: term) => {
  switch t {
    | Abs(k, t1) => {
      let (ctx', k') = pickFreshName(ctx, k)
      `(λ${k'}. ${printTerm(ctx', t1)})`
    }
    | App(t1, t2) => {
      `(${printTerm(ctx, t1)} ${printTerm(ctx, t2)})`
    }
    | Var(x, n) => {
      if List.length(ctx) == n {
        indexToName(ctx, x)
      } else {
        `[${String.make(List.length(ctx))} ${String.make(n)} bad index]`
      }
    }
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
      | Abs(x, t1) => Abs(x, walk(c + 1, t1))
      | App(t1, t2) => App(walk(c, t1), walk(c, t2))
    }
  }
  walk(0, t)
}

// [j -> s]t
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
      | Abs(x, t1) => Abs(x, walk(c + 1, t1))
      | App(t1, t2) => App(walk(c, t1), walk(c, t2))
    }
  }
  walk(0, t)
}

// α-reduction
let substTop = (s, t) => {
  shift(-1, subst(0, shift(1, s), t))
}

// value is lambda abstraction
let isVal = (_ctx, t) => {
  switch t {
    | Abs(_, _) => true
    | _ => false
  }
}

exception NoRuleApplies(term)

// one step evaluation
let rec eval1 = (ctx: context, t) => {
  switch t {
    | App(Abs(_, t12), v2) if isVal(ctx, v2) => substTop(v2, t12)
    | App(v1, t2) if isVal(ctx, v1) => {
      let t2' = eval1(ctx, t2)
      App(v1, t2')
    }
    | App(t1, t2) => {
      let t1' = eval1(ctx, t1)
      App(t1', t2)
    }
    | _ => raise(NoRuleApplies(t))
  }
}

let rec eval = (ctx, t) => {
  // print terms at each step of the evaluation
  Console.log(printTerm(ctx, t))
  switch isVal(ctx, t) {
    | true  => t
    | false => eval(ctx, eval1(ctx, t))
  }
}

// ((λ x. x) (λ y. y))
let _ = eval(list{}, App(Abs("x", Var(0, 1)), Abs("x", Var(0, 1))))

// ((λ x. (λ y. x)) (λ z. z))
let _ = eval(list{}, App(Abs("x", Abs("y", Var(1, 2))), Abs("z", Var(0, 1))))

// if terms have same name variable, create fresh name variable
// ((λ x. (λ x. (λ x. x))) (λ x. x)) -> ((λ x. (λ x'. (λ x''. x))) (λ x. x))
// (λ x. (λ x. (λ x. x))) -> (λ x. (λ x'. (λ x''. x'')))
let _ = eval(list{}, App(Abs("x", Abs("x", Abs("x", Var(2, 3)))), Abs("x", Var(0, 1))))
