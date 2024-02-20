// Implementation of Chapter 10 on Types and Programming Languages
type deBrujinIndex = int
type depth = int

type rec ty =
  // Arrow
  | TyArr(ty, ty)
  | TyBool
  | TyError(string)

type varName = string
type rec term =
  // variabler
  | Var(deBrujinIndex, depth)
  // lambda abstraction
  | Abs(varName, ty, term)
  // application
  | App(term, term)
  | True
  | False
  | If(term, term, term)

type binding =
  | NameBind
  | VarBind(ty)

// typing context
type context = list<(varName, binding)>

let addBinding = (ctx: context, name, bind): context => ctx->List.add((name, bind))

let getBinding = (ctx: context, n: deBrujinIndex) => {
  switch ctx->List.get(n) {
    | Some(_, binding) => Some(binding)
    | None => None
  }
}

let getTypeFromContext = (ctx, i: deBrujinIndex) => {
  switch getBinding(ctx, i) {
    | Some(VarBind(ty)) => Some(ty)
    | _ => None
  }
}

let rec typeOf = (ctx, t) => {
  switch t {
    | Var(i, _) => getTypeFromContext(ctx, i)
    | Abs(name, tyT1, t2) => {
      let ctx' = addBinding(ctx, name, VarBind((tyT1)))
      switch typeOf(ctx', t2) {
        | Some(tyT2) => Some(TyArr(tyT1, tyT2))
        | None => None
      }
    }
    | App(t1, t2) => {
      switch typeOf(ctx, t1) {
        | Some(TyArr(tyT11, tyT12)) => {
          switch typeOf(ctx, t2) {
            | Some(tyT2) => {
              if tyT2 == tyT11 {
                Some(tyT12)
              } else {
                None
              }
            }
            | None => None
          }
        }
        | _ => None
      }
    }
    | True => Some(TyBool)
    | False => Some(TyBool)
    | If(t1, t2, t3) => {
      switch (typeOf(ctx, t1), typeOf(ctx, t2), typeOf(ctx, t3)) {
        | (Some(TyBool), Some(ty2), Some(ty3)) => ty2 == ty3 ? Some(ty2) : None
        | _ => None
      }
    }
  }
}

// generate new name
let rec pickFreshName = (ctx: context, name): (context, varName) => {
  switch ctx->List.getBy(((varName, _binding)) => name == varName) {
    | Some(name, _binding) => pickFreshName(ctx, name ++ "'")
    | None => (ctx->List.add((name, NameBind)), name)
  }
}

// find variable name by deBrujinIndex
let indexToName = (ctx: context, x: deBrujinIndex) => {
  switch ctx->List.get(x) {
    | Some(name, _binding) => name
    | None => `[${String.make(x)} bad index]`
  }
}

let rec printTY = (ty: ty) => {
  switch ty {
    | TyArr(ty1, ty2) => `(${printTY(ty1)} -> ${printTY(ty2)})`
    | TyBool => "Bool"
    | TyError(message) => message
  }
}

// PrettyPrinter
let rec printTerm = (ctx: context, t: term) => {
  switch t {
    | Abs(name, ty, t1) => {
      let (ctx', name') = pickFreshName(ctx, name)
      `(λ${name'}:${printTY(ty)}. ${printTerm(ctx', t1)})`
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
    | True => "true"
    | False => "false"
    | If(t1, t2, t3) => {
      `if ${printTerm(ctx, t1)} then ${printTerm(ctx, t2)} else ${printTerm(ctx, t3)}`
    }
  }
}

let shift = (d: deBrujinIndex, t) => {
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
      | Abs(name, ty, t1) => Abs(name, ty, walk(c + 1, t1))
      | App(t1, t2) => App(walk(c, t1), walk(c, t2))
      | True => True
      | False => False
      | If(t1, t2, t3) => If(walk(c, t1), walk(c, t2), walk(c, t3))
    }
  }
  walk(0, t)
}

// [j -> s]t
let subst = (j: deBrujinIndex, s, t) => {
  let rec walk = (c, t) => {
    switch t {
      | Var(k, n) => {
        if k == j + c {
          shift(c, s)
        } else {
          Var(k, n)
        }
      }
      | Abs(name, ty, t1) => Abs(name, ty, walk(c + 1, t1))
      | App(t1, t2) => App(walk(c, t1), walk(c, t2))
      | True => True
      | False => False
      | If(t1, t2, t3) => If(walk(c, t1), walk(c, t2), walk(c, t3))
    }
  }
  walk(0, t)
}

// β-reduction
let substTop = (s, t) => {
  shift(-1, subst(0, shift(1, s), t))
}

// value is lambda abstraction
let isVal = (_ctx, t) => {
  switch t {
    | Abs(_, _, _) => true
    | True => true
    | False => true
    | _ => false
  }
}

exception NoRuleApplies(term)

// one step evaluation
let rec eval1 = (ctx, t) => {
  switch t {
    // E-AppAbs
    | App(Abs(_, _, t12), v2) if isVal(ctx, v2) => substTop(v2, t12)
    // E-App2
    | App(v1, t2) if isVal(ctx, v1) => {
      let t2' = eval1(ctx, t2)
      App(v1, t2')
    }
    // E-App1
    | App(t1, t2) => {
      let t1' = eval1(ctx, t1)
      App(t1', t2)
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
    printTerm(ctx, t),
    "Type is",
    printTY(Option.getOr(
        typeOf(ctx, t),
        TyError("error!")
      )
    )
  )
  switch isVal(ctx, t) {
    | true  => t
    | false => eval(ctx, eval1(ctx, t))
  }
}

// ((λx:(Bool -> Bool). x) (λy:Bool. y))
let _ = eval(list{}, App(Abs("x", TyArr(TyBool, TyBool), App(Var(0, 1), True)), Abs("y", TyBool, Var(0, 1))))

// ((λx:Bool. (λ y:Bool. x)) (λz:Bool. z))
let _ = eval(list{}, App(Abs("x", TyArr(TyBool, TyBool), Abs("y", TyBool, Var(1, 2))), Abs("z", TyBool, Var(0, 1))))

// ((λx:(Bool -> Bool). (λy:((Bool -> Bool) -> Bool). (y x))) (λz:Bool. z))
let _ = eval(list{}, App(Abs("x", TyArr(TyBool, TyBool), Abs("y", TyArr(TyArr(TyBool, TyBool), TyBool), App((Var(0, 2), Var(1, 2))))), Abs("z", TyBool, Var(0, 1))))
