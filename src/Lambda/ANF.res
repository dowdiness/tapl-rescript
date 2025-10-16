// https://compiler.club/anf-conversion/

// atoms, these are either variables or constants
type atom =
  | AtomInt(int)
  | AtomVar(Ast.varName)
  | AtomGlob(Ast.varName)

// ANF representation
type rec t =
  | Halt(atom)
  | Fun(Ast.varName, list<Ast.varName>, t, t)
  // https://en.wikipedia.org/wiki/Static_single-assignment_form
  // φ function
  // https://pages.cs.wisc.edu/~fischer/cs701.f08/lectures/Lecture23.4up.pdf
  | Join(Ast.varName, option<Ast.varName>, t, t)
  // Branch
  | Jump(Ast.varName, option<atom>)
  | App(Ast.varName, Ast.varName, list<atom>, t)
  | Bop(Ast.varName, Ast.bop, atom, atom, t)
  | If(atom, t, t)
  // For Colosure Conversion
  | Tuple(Ast.varName, list<atom>, t)
  | Proj(Ast.varName, Ast.varName, int, t)

// Pretty printing function for atom
let printAtom = (atom: atom): string => {
  switch atom {
  | AtomInt(i) => Int.toString(i)
  | AtomVar(x) => x
  | AtomGlob(x) => `@${x}`
  }
}

// Pretty printing function for ANF
let rec printANF = (t: t): string => {
  switch t {
  | Halt(atom) => `halt ${printAtom(atom)}`
  | Fun(f, xs, e, e') => {
      let params = xs->List.toArray->Array.join(", ")
      `fun ${f}(${params}) =\n  ${printANF(e)}\nin\n${printANF(e')}`
    }
  | Join(j, Some(p), e, e') => `join ${j}(${p}) =\n  ${printANF(e)}\nin\n${printANF(e')}`
  | Join(j, None, e, e') => `join ${j} =\n  ${printANF(e)}\nin\n${printANF(e')}`
  | Jump(j, Some(atom)) => `jump ${j}(${printAtom(atom)})`
  | Jump(j, None) => `jump ${j}`
  | App(r, f, vs, e) => {
      let args = vs->List.map(printAtom)->List.toArray->Array.join(", ")
      `let ${r} = ${f}(${args}) in\n${printANF(e)}`
    }
  | Bop(r, Plus, x, y, e) => `let ${r} = ${printAtom(x)} + ${printAtom(y)} in\n${printANF(e)}`
  | Bop(r, Minus, x, y, e) => `let ${r} = ${printAtom(x)} - ${printAtom(y)} in\n${printANF(e)}`
  | If(atom, t, f) => `if ${printAtom(atom)} then\n  ${printANF(t)}\nelse\n  ${printANF(f)}`
  | Tuple(r, vs, e) => {
      let values = vs->List.map(printAtom)->List.toArray->Array.join(", ")
      `let ${r} = (${values}) in\n${printANF(e)}`
    }
  | Proj(r, x, i, e) => `let ${r} = ${x}.${Int.toString(i)} in\n${printANF(e)}`
  }
}

exception MustApplyVar(atom)

// Helper function to create Halt
let mkHalt = (v: atom) => Halt(v)

// let* operator for continuation-passing style
let letStar = (f, k) => f(k)

// ANF conversion algorithm
let convert = {
  let rec go = (e: Ast.t, k: atom => t): t => {
    switch e {
    | Int(i) => k(AtomInt(i))
    | Var(x) => k(AtomVar(x))
    | Lam(x, t) => {
        let f = Ast.fresh("f")
        let t' = go(t, v => mkHalt(v))
        Fun(f, list{x}, t', k(AtomVar(f)))
      }
    | App(f, x) => {
        letStar(go(f, _), fAtom => {
          letStar(go(x, _), xAtom => {
            switch fAtom {
            | AtomVar(fVar) => {
                let r = Ast.fresh("r")
                App(r, fVar, list{xAtom}, k(AtomVar(r)))
              }
            | atom => raise(MustApplyVar(atom))
            }
          })
        })
      }
    | Bop(op, x, y) => {
        letStar(go(x, _), xAtom => {
          letStar(go(y, _), yAtom => {
            let r = Ast.fresh("r")
            Bop(r, op, xAtom, yAtom, k(AtomVar(r)))
          })
        })
      }
    | If(e, t, f) => {
      // We introduce Join Point here
      // https://compiler.club/compiling-lambda-calculus/#:~:text=we%20introduce%20a-,join%20point,-%3A
        letStar(go(e, _), eAtom => {
          // 1 + if 2 then 3 else 4
          //        ↓
          // join j1(p0) =
          //   let r2 = 1 + p0 in
          //   return r2
          // in
          // if(2) then
          //   let r3 = 3
          //   jump j1(r3)
          // else
          //   let r4 = 4
          //   jump j1(r4)

          // IFの分岐ブランチ先からIFの定義元へと飛ぶ関数
          let j = Ast.fresh("j")
          // Ifの分岐ブランチであるthen, elseの評価結果を入れる変数
          let p = Ast.fresh("p")
          // Jumpはthen, elseの本体で最終的に呼ばれる
          let joinVar = Jump(j, Some(AtomVar(p)))
          // Join(joinの変数名, phi関数の結果の入った引数, Joinの本体, then, else)
          Join(j, Some(p), k(AtomVar(p)), If(eAtom, go(t, _ => joinVar), go(f, _ => joinVar)))
        })
      }
    }
  }

  // Entry point for conversion
  (e: Ast.t) => go(e, mkHalt)
}
