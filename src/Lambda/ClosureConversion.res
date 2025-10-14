// Free variables computation
let rec compute = (t: ANF.t): Belt.Set.String.t => {
  switch t {
  | Halt(AtomVar(x)) => Belt.Set.String.fromArray([x])
  | Halt(_) => Belt.Set.String.empty
  | Fun(f, xs, e, e') => {
      let fvE = compute(e)
      let fvE' = compute(e')
      let bound = Belt.Set.String.fromArray(Array.concat([f], List.toArray(xs)))
      Belt.Set.String.union(
        Belt.Set.String.diff(fvE, bound),
        Belt.Set.String.diff(fvE', Belt.Set.String.fromArray([f])),
      )
    }
  | Join(j, Some(p), e, e') => {
      let fvE = compute(e)
      let fvE' = compute(e')
      let bound = Belt.Set.String.fromArray([j, p])
      Belt.Set.String.union(
        Belt.Set.String.diff(fvE, bound),
        Belt.Set.String.diff(fvE', bound),
      )
    }
  | Join(j, None, e, e') => {
      let fvE = compute(e)
      let fvE' = compute(e')
      let bound = Belt.Set.String.fromArray([j])
      Belt.Set.String.union(
        Belt.Set.String.diff(fvE, bound),
        Belt.Set.String.diff(fvE', bound),
      )
    }
  | Jump(j, Some(AtomVar(x))) => Belt.Set.String.fromArray([j, x])
  | Jump(j, Some(_)) => Belt.Set.String.fromArray([j])
  | Jump(j, None) => Belt.Set.String.fromArray([j])
  | App(r, f, vs, e) => {
      let atomVars =
        vs
        ->List.map(atom =>
          switch atom {
          | AtomVar(x) => [x]
          | _ => []
          }
        )
        ->List.toArray
        ->Array.flat
      let fvE = compute(e)
      Belt.Set.String.union(
        Belt.Set.String.fromArray(Array.concat([f], atomVars)),
        Belt.Set.String.diff(fvE, Belt.Set.String.fromArray([r])),
      )
    }
  | Bop(r, _, x, y, e) => {
      let atomVars =
        [x, y]
        ->Array.map(atom =>
          switch atom {
          | AtomVar(x) => [x]
          | _ => []
          }
        )
        ->Array.flat
      let fvE = compute(e)
      Belt.Set.String.union(
        Belt.Set.String.fromArray(atomVars),
        Belt.Set.String.diff(fvE, Belt.Set.String.fromArray([r])),
      )
    }
  | If(AtomVar(x), t, f) => {
      Belt.Set.String.union(
        Belt.Set.String.fromArray([x]),
        Belt.Set.String.union(compute(t), compute(f)),
      )
    }
  | If(_, t, f) => Belt.Set.String.union(compute(t), compute(f))
  | Tuple(r, vs, e) => {
      let atomVars =
        vs
        ->List.map(atom =>
          switch atom {
          | AtomVar(x) => [x]
          | _ => []
          }
        )
        ->List.toArray
        ->Array.flat
      let fvE = compute(e)
      Belt.Set.String.union(
        Belt.Set.String.fromArray(atomVars),
        Belt.Set.String.diff(fvE, Belt.Set.String.fromArray([r])),
      )
    }
  | Proj(r, x, _, e) => {
      let fvE = compute(e)
      Belt.Set.String.union(
        Belt.Set.String.fromArray([x]),
        Belt.Set.String.diff(fvE, Belt.Set.String.fromArray([r])),
      )
    }
  }
}

// Closure conversion
let convert = {
  let rec go = (t: ANF.t): ANF.t => {
    switch t {
    | Fun(f, xs, e, e') => {
        let env = Ast.fresh("env")
        let allFvs = compute(e)
        let params = Belt.Set.String.fromArray(List.toArray(xs))
        let fvs = Belt.Set.String.diff(allFvs, params)->Belt.Set.String.toArray->List.fromArray

        // Create projections for free variables in the function body
        let rec addProjections = (body, fvList, index) => {
          switch fvList {
          | list{} => body
          | list{x, ...rest} => ANF.Proj(x, env, index, addProjections(body, rest, index + 1))
          }
        }

        let transformedBody = addProjections(go(e), fvs, 1)
        let vs = List.map(fvs, x => ANF.AtomVar(x))
        let closureTuple = ANF.Tuple(f, list{ANF.AtomGlob(f), ...vs}, go(e'))
        ANF.Fun(f, list{env, ...xs}, transformedBody, closureTuple)
      }
    | App(r, f, vs, e) => {
        let ptr = Ast.fresh("f")
        ANF.Proj(ptr, f, 0, ANF.App(r, ptr, list{ANF.AtomVar(f), ...vs}, go(e)))
      }
    | Join(j, p, e, e') => ANF.Join(j, p, go(e), go(e'))
    | Bop(r, op, x, y, e) => ANF.Bop(r, op, x, y, go(e))
    | If(x, t, f) => ANF.If(x, go(t), go(f))
    | Tuple(r, vs, e) => ANF.Tuple(r, vs, go(e))
    | Proj(r, x, i, e) => ANF.Proj(r, x, i, go(e))
    | Halt(_) | Jump(_, _) => t
    }
  }
  go
}
