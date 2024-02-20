type deBrujinIndex = int
type depth = int
type varName = string
type rec term =
  // 変数
  | Var(deBrujinIndex, depth)
  // ラムダ抽象
  | Abs(varName, term)
  // 関数適用
  | App(term, term)

type binding = NameBind
type context = list<(varName, binding)>

// 新しい名前を生成
let rec pickFreshName = (ctx: context, name): (context, varName) => {
  switch ctx->List.getBy(((varName, _binding)) => name == varName) {
    | Some(name, _binding) => pickFreshName(ctx, name ++ "'")
    | None => (ctx->List.add((name, NameBind)), name)
  }
}

// deBrujinIndexを元にしてcontextから変数名を取得する
let indexToName = (ctx: context, x: deBrujinIndex) => {
  switch ctx->List.get(x) {
    | Some(name, _binding) => name
    | None => `[${String.make(x)} bad index]`
  }
}

// 項のPrettyPrinter
let rec printTerm = (ctx: context, t: term) => {
  switch t {
    | Abs(k, t1) => {
      let (ctx', k') = pickFreshName(ctx, k)
      `(λ ${k'}. ${printTerm(ctx', t1)})`
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

// シフトと代入

// シフト演算
let shift = (d: deBrujinIndex, t) => {
  let rec walk = (c, t) => {
    switch t {
      | Var(k, n) => {
        if k >= c {
          // 自由変数なのでシフトする
          Var(k + d, n + d)
        } else {
          // 束縛変数なのでシフトしない
          Var(k, n + d)
        }
      }
      | Abs(x, t1) => Abs(x, walk(c + 1, t1))
      | App(t1, t2) => App(walk(c, t1), walk(c, t2))
    }
  }
  walk(0, t)
}

// [j -> s]t の代入関数
// j: 代入する変数のindex, s: 代入される項, t:代入の発生する項
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
      | Abs(x, t1) => Abs(x, walk(c + 1, t1))
      | App(t1, t2) => App(walk(c, t1), walk(c, t2))
    }
  }
  walk(0, t)
}

// ベータ簡約
let substTop = (s, t) => {
  shift(-1, subst(0, shift(1, s), t))
}

// 値=ラムダ抽象
let isVal = (_ctx, t) => {
  switch t {
    | Abs(_, _) => true
    | _ => false
  }
}

exception NoRuleApplies(term)

// ワンステップ評価
let rec eval1 = (ctx, t) => {
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

// 評価
let rec eval = (ctx, t) => {
  Console.log(printTerm(ctx, t))
  if isVal(ctx, t) {
    t
  } else {
    switch eval1(ctx, t) {
      | t' => eval(ctx, t')
    }
  }
}

// ((λ x. x) (λ y. y))
let _ = eval(list{}, App(Abs("x", Var(0, 1)), Abs("y", Var(0, 1))))

// ((λ x. λ y. x) (λ z. z))
let _ = eval(list{}, App(Abs("x", Abs("y", Var(1, 2))), Abs("z", Var(0, 1))))
