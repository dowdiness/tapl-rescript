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
  | Join(Ast.varName, option<Ast.varName>, t, t)
  | Jump(Ast.varName, option<atom>)
  | App(Ast.varName, Ast.varName, list<atom>, t)
  | Bop(Ast.varName, Ast.bop, atom, atom, t)
  | If(atom, t, t)
  | Tuple(Ast.varName, list<atom>, t)
  | Proj(Ast.varName, Ast.varName, int, t)

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
            | _ => failwith("Must apply named value!")
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
          let j = Ast.fresh("j")
          let p = Ast.fresh("p")
          let joinVar = Jump(j, Some(AtomVar(p)))
          Join(j, Some(p), k(AtomVar(p)), If(eAtom, go(t, _ => joinVar), go(f, _ => joinVar)))
        })
      }
    }
  }

  // Entry point for conversion
  (e: Ast.t) => go(e, mkHalt)
}
