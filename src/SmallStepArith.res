// Implementation of Chapter 4 on Types and Programming Languages
type rec term =
  | True
  | False
  | If(term, term, term)
  | Zero
  | Succ(term)
  | Pred(term)
  | IsZero(term)

let rec isNumericVal = (term: term) => {
  switch term {
    | Zero => true
    | Succ(t1) => isNumericVal(t1)
    | _ => false
  }
}

let isVal = (term: term) => {
  switch term {
    | True => true
    | False => true
    | _ if isNumericVal(term) => true
    | _ => false
  }
}

exception NoRuleApplies(term)

let rec eval1 = (term: term) => {
  switch term {
    // E-IfTrue
    | If(True, t2, _t3) => t2
    // E-IfFalse
    | If(False, _t2, t3) => t3
    // E-If
    | If(t1, t2, t3) => {
      let t1' = eval1(t1)
      If(t1', t2, t3)
    }
    // E-Succ
    | Succ(t1) => {
      let t1' = eval1(t1)
      Succ(t1')
    }
    // E-PredZero
    | Pred(Zero) => Zero
    // E-PredSucc
    | Pred(Succ(nv1)) if isNumericVal(nv1) => nv1
    // E-Pred
    | Pred(t1) => {
      let t1' = eval1(t1)
      Pred(t1')
    }
    // E-IsZeroZero
    | IsZero(Zero) => True
    // E-IsZeroSucc
    | IsZero(Succ(nv1)) if isNumericVal(nv1) => False
    // E-IsZero
    | IsZero(t1) => {
      let t1' = eval1(t1)
      IsZero(t1')
    }
    | _ => raise(NoRuleApplies(term))
  }
}

let rec eval = (term: term) => {
  // 1ステップ毎の評価結果を表示する
  Console.log(term)
  if isVal(term) {
    term
  } else {
    switch eval1(term) {
      | t' => eval(t')
    }
  }
}

Console.log("Example1: Succ")
let _ = eval(Succ(Succ(Succ(Succ(Succ(Zero))))))

Console.log("Example2: If")
let _ = eval(If(True, If(False, False, False), True))
