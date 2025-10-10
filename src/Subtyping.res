// Implementation of Chapter 10 on Types and Programming Languages
type deBruijnIndex = int
type depth = int

// Type
type rec ty =
  | TyRecord(list<(string, ty)>)
  | TyTop // Top type
  | TyArr(ty, ty) // Type Arrow
  | TyBool
  | TyError(string)

type varName = string
// Term
type rec term =
  // variable
  | Var(deBruijnIndex, depth)
  // lambda abstraction
  | Abs(varName, ty, term)
  // application
  | App(term, term)
  // Record
  | Record(list<(string, term)>)
  // Record Method
  | Proj(term, string)
  | True
  | False
  | If(term, term, term)

type binding =
  | NameBind
  | VarBind(ty)

// Typing context
type context = list<(varName, binding)>

let addBinding = (ctx: context, name, bind): context => ctx->List.add((name, bind))

let getBinding = (ctx: context, n: deBruijnIndex) => {
  switch ctx->List.get(n) {
    | Some(_, binding) => Some(binding)
    | None => None
  }
}

let getTypeFromContext = (ctx, i: deBruijnIndex) => {
  switch getBinding(ctx, i) {
    | Some(VarBind(ty)) => Some(ty)
    | _ => None
  }
}

// S ≤ T が成り立つのか判定する関数
// 部分型関係は基本的に前順序であり、以下の二つの二項関係が成り立つ
// 反射律：P の任意の元 a に対し、a ≤ a が成り立つ。
// 推移律：P の任意の元 a, b, c に対し、a ≤ b かつ b ≤ c ならば a ≤ c が成り立つ。
// https://ja.wikipedia.org/wiki/%E9%A0%86%E5%BA%8F%E9%9B%86%E5%90%88
let rec subtype = (tyS, tyT) => {
  // 反射律によりSとTの型が同じなら部分型関係が成り立つ
  if tyS == tyT {
    true
  } else {
    switch (tyS, tyT) {
      // Topは全ての型の上位型なのでTがTop型なら必ず部分型関係が成り立つ
      | (_, TyTop) => true
      | (TyArr(tyS1, tyS2), TyArr(tyT1, tyT2)) => {
        subtype(tyT1, tyS1) && subtype(tyS2, tyT2)
      }
      | (TyRecord(fS), TyRecord(fT)) => {
        fT->List.every(((label, tyTi)) => {
          switch fS->List.getAssoc(label, (k, item) => k == item) {
            | Some(tySi) => subtype(tySi, tyTi)
            | None => false
          }
        })
      }
      | (_, _) => false
    }
  }
}

// Type Check
let rec typeOf = (ctx, t) => {
  switch t {
    // T-Var
    | Var(i, _) => getTypeFromContext(ctx, i)
    // T-Abs
    | Abs(name, tyT1, t2) => {
      let ctx' = addBinding(ctx, name, VarBind((tyT1)))
      switch typeOf(ctx', t2) {
        | Some(tyT2) => Some(TyArr(tyT1, tyT2))
        | None => None
      }
    }
    // T-App
    | App(t1, t2) => {
      switch (typeOf(ctx, t1), typeOf(ctx, t2)) {
        | (Some(TyArr(tyT11, tyT12)), Some(tyT2)) => {
          if !subtype(tyT2, tyT11) {
            Console.log2(tyT2, tyT11)
          }
          subtype(tyT2, tyT11) ? Some(tyT12) : None
        }
        | (_, _) => {
          Console.log2(t1, t2)
          None
        }
      }
    }
    // T-True
    | True => Some(TyBool)
    // T-False
    | False => Some(TyBool)
    // T-If
    | If(t1, t2, t3) => {
      switch (typeOf(ctx, t1), typeOf(ctx, t2), typeOf(ctx, t3)) {
        // ty2 and ty3 need to have same type
        | (Some(TyBool), Some(ty2), Some(ty3)) => ty2 == ty3 ? Some(ty2) : None
        | _ => None
      }
    }
    // T-Record
    | Record(fields) => {
        let fieldTypes = fields->List.map(((label, fieldTerm)) => {
          (label, Option.getOr(typeOf(ctx, fieldTerm), TyError(`${label} error!`)))
        })
        Some(TyRecord(fieldTypes))
      }
    // T-Proj
    | Proj(t1, label) => {
        switch typeOf(ctx, t1) {
          | Some(TyRecord(fieldTys)) => {
              switch fieldTys->List.getAssoc(label, (k, item) => k == item) {
                | Some(ty) => Some(ty)
                | None => {
                  Console.log(label)
                  None
                }
              }
            }
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

// Find variable name by De Bruijn index
let indexToName = (ctx: context, x: deBruijnIndex) => {
  switch ctx->List.get(x) {
    | Some(name, _binding) => name
    | None => `[${String.make(x)} bad index]`
  }
}

// PrettyPrinter for type
let rec printTY = (ty: ty) => {
  switch ty {
    | TyArr(ty1, ty2) => `(${printTY(ty1)} -> ${printTY(ty2)})`
    | TyBool => "Bool"
    | TyTop => "⊤"
    | TyRecord(fields) => {
        let record = fields
          ->List.map(((label, fieldTy)) => `${label}: ${printTY(fieldTy)}`)
          ->List.toArray
          ->Array.joinWith(", ")
        `{ ${record} }`
      }
    | TyError(message) => message
  }
}

// PrettyPrinter for term
let rec printTerm = (ctx: context, t: term) => {
  switch t {
    | Abs(name, ty, t1) => {
      let (ctx', name') = pickFreshName(ctx, name)
      `(λ${name'}:${printTY(ty)}. ${printTerm(ctx', t1)})`
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
    | True => "true"
    | False => "false"
    | If(t1, t2, t3) => {
      `if ${printTerm(ctx, t1)} then ${printTerm(ctx, t2)} else ${printTerm(ctx, t3)}`
    }
    | Record(fields) => {
        let record = fields
          ->List.map(((label, fieldTerm)) => `${label}: ${printTerm(ctx, fieldTerm)}`)
          ->List.toArray
          ->Array.joinWith(", ")
        `{ ${record} }`
      }
    | Proj(t1, label) => {
        `${printTerm(ctx, t1)}.${label}`
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
        | (name, VarBind(ty)) => `${acc.contents}${name}: ${printTY(ty)}`
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
      | Abs(name, ty, t1) => Abs(name, ty, walk(c + 1, t1))
      | App(t1, t2) => App(walk(c, t1), walk(c, t2))
      | True => True
      | False => False
      | If(t1, t2, t3) => If(walk(c, t1), walk(c, t2), walk(c, t3))
      | Record(fields) => Record(fields->List.map(((label, fieldTerm)) => (label, walk(c, fieldTerm))))
      | Proj(t1, label) => Proj(walk(c, t1), label)
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
      | Abs(name, ty, t1) => Abs(name, ty, walk(c + 1, t1))
      | App(t1, t2) => App(walk(c, t1), walk(c, t2))
      | True => True
      | False => False
      | If(t1, t2, t3) => If(walk(c, t1), walk(c, t2), walk(c, t3))
      | Record(fields) => Record(fields->List.map(((label, fieldTerm)) => (label, walk(c, fieldTerm))))
      | Proj(t1, label) => Proj(walk(c, t1), label)
    }
  }
  walk(0, t)
}

// α-reduction
let substTop = (s, t) => {
  shift(-1, subst(0, shift(1, s), t))
}

// Values are lambda abstraction, True and False.
let rec isVal = (_ctx, t) => {
  switch t {
    | Abs(_, _, _) => true
    | True => true
    | False => true
    | Record(fields) => {
      fields->List.every(((_, fieldTerm)) => isVal(_ctx, fieldTerm))
    }
    | _ => false
  }
}

exception NoRuleApplies(term)

// one step evaluation
let rec eval1 = (ctx: context, t) => {
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
    // E-Record
    | Record(fields) => {
        let rec evalFields = (fields) => {
          switch fields {
            | list{} => list{}
            | list{(label, fieldTerm), ...rest} => {
                // Evaluate the field that is not a value
                if isVal(ctx, fieldTerm) {
                  list{(label, fieldTerm), ...evalFields(rest)}
                } else {
                  list{(label, eval1(ctx, fieldTerm)), ...rest}
                }
              }
          }
        }
        Record(evalFields(fields))
      }
    // E-ProjRcd
    | Proj(Record(fields), label) => {
        switch fields->List.getAssoc(label, (k, item) => k == item) {
          | Some(fieldTerm) => fieldTerm
          | None => raise(NoRuleApplies(t))
        }
      }
    // E-Proj
    | Proj(t1, label) => {
        let t1' = eval1(ctx, t1)
        Proj(t1', label)
      }
    | _ => raise(NoRuleApplies(t))
  }
}

let rec eval = (ctx, t) => {
  // print terms at each step of the evaluation
  Console.log5(
    printContext(ctx),
    "|- Term: ",
    printTerm(ctx, t),
    "| Type: ",
    printTY(Option.getOr(
        typeOf(ctx, t),
        TyError("type error!")
      )
    )
  )
  switch isVal(ctx, t) {
    | true  => t
    | false => eval(ctx, eval1(ctx, t))
  }
}

// (λx:(Bool -> Bool). x true) (λy:Bool. y)
let _ = eval(list{}, App(Abs("x", TyArr(TyBool, TyBool), App(Var(0, 1), True)), Abs("y", TyBool, Var(0, 1))))
Console.log("")

// (λx:(Bool -> Bool). (λy:Bool. x)) (λz:Bool. z)
let _ = eval(list{}, App(Abs("x", TyArr(TyBool, TyBool), Abs("y", TyBool, Var(1, 2))), Abs("z", TyBool, Var(0, 1))))
Console.log("")

// (λx:(Bool -> Bool). (λy:((Bool -> Bool) -> Bool). y x)) (λz:Bool. z)
let _ = eval(list{}, App(Abs("x", TyArr(TyBool, TyBool), Abs("y", TyArr(TyArr(TyBool, TyBool), TyBool), App((Var(0, 2), Var(1, 2))))), Abs("z", TyBool, Var(0, 1))))
Console.log("")

// (λx:{ a: Bool, b: Bool }. x.b) { a: true, b: false, c: true }
let recordFunc = Abs("x", TyRecord(list{("a", TyBool), ("b", TyBool)}), Proj(Var(0, 1), "b"))
let _ = eval(list{}, App(recordFunc, Record(list{("nonUse", True), ("a", False), ("b", True)})))
Console.log("")
