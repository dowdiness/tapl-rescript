// Hoisting transformation
exception NotFoundJointPoint(ANF.t)
// Join point information
type join_point = {
  name: Ast.varName,
  param: option<Ast.varName>,
  body: ANF.t,
}

// Substitute a variable with an atom in ANF term
let rec substituteVar = (term: ANF.t, var: Ast.varName, replacement: ANF.atom): ANF.t => {
  let substAtom = (atom: ANF.atom): ANF.atom => {
    switch atom {
    | AtomVar(x) when x == var => replacement
    | _ => atom
    }
  }

  switch term {
  | Halt(atom) => Halt(substAtom(atom))
  | Fun(f, params, body, cont) => {
      // Don't substitute if var is bound by function parameters
      if List.some(params, p => p == var) || f == var {
        Fun(f, params, body, substituteVar(cont, var, replacement))
      } else {
        Fun(f, params, substituteVar(body, var, replacement), substituteVar(cont, var, replacement))
      }
    }
  | App(r, f, args, cont) => {
      let newArgs = List.map(args, substAtom)
      let newF = if f == var {
        switch replacement {
        | AtomVar(newF) => newF
        | _ => f // Can't substitute non-variable
        }
      } else {
        f
      }
      App(r, newF, newArgs, substituteVar(cont, var, replacement))
    }
  | Bop(r, op, x, y, cont) =>
      Bop(r, op, substAtom(x), substAtom(y), substituteVar(cont, var, replacement))
  | If(cond, thenBranch, elseBranch) =>
      If(substAtom(cond), substituteVar(thenBranch, var, replacement), substituteVar(elseBranch, var, replacement))
  | Tuple(r, atoms, cont) =>
      Tuple(r, List.map(atoms, substAtom), substituteVar(cont, var, replacement))
  | Proj(r, x, i, cont) => {
      let newX = if x == var {
        switch replacement {
        | AtomVar(newX) => newX
        | _ => x // Can't substitute non-variable
        }
      } else {
        x
      }
      Proj(r, newX, i, substituteVar(cont, var, replacement))
    }
  | Join(j, param, body, cont) =>
      Join(j, param, substituteVar(body, var, replacement), substituteVar(cont, var, replacement))
  | Jump(j, arg) => Jump(j, Option.map(arg, substAtom))
  }
}

// Collect all join points from ANF term
let collectJoinPoints = (term: ANF.t): Belt.Map.String.t<join_point> => {
  let joinPoints = ref(Belt.Map.String.empty)

  let rec collect = (t: ANF.t) => {
    switch t {
    | Join(j, param, body, cont) => {
        let jp = {name: j, param: param, body: body}
        joinPoints := Belt.Map.String.set(joinPoints.contents, j, jp)
        collect(body)
        collect(cont)
      }
    | Fun(_, _, body, cont) => {
        collect(body)
        collect(cont)
      }
    | App(_, _, _, cont) => collect(cont)
    | Bop(_, _, _, _, cont) => collect(cont)
    | If(_, thenBranch, elseBranch) => {
        collect(thenBranch)
        collect(elseBranch)
      }
    | Tuple(_, _, cont) => collect(cont)
    | Proj(_, _, _, cont) => collect(cont)
    | Halt(_) | Jump(_, _) => ()
    }
  }

  collect(term)
  joinPoints.contents
}

// Eliminate join points by substituting jumps with inlined bodies
let eliminateJoinPoints = (term: ANF.t): ANF.t => {
  let joinPoints = collectJoinPoints(term)

  let rec eliminate = (t: ANF.t): ANF.t => {
    switch t {
    | Jump(j, arg) => {
        switch Belt.Map.String.get(joinPoints, j) {
        | Some({body, param: Some(p), _}) => {
            switch arg {
            | Some(atom) => eliminate(substituteVar(body, p, atom))
            | None => eliminate(body) // Should not happen with Some(p)
            }
          }
        | Some({body, param: None, _}) => eliminate(body)
        // Should not happen in well-formed code
        | None => raise(NotFoundJointPoint(t))
        }
      }
    | Join(_, _, _, cont) => eliminate(cont) // Remove join point, keep continuation
    | Fun(f, params, body, cont) => Fun(f, params, eliminate(body), eliminate(cont))
    | App(r, f, args, cont) => App(r, f, args, eliminate(cont))
    | Bop(r, op, x, y, cont) => Bop(r, op, x, y, eliminate(cont))
    | If(cond, thenBranch, elseBranch) => If(cond, eliminate(thenBranch), eliminate(elseBranch))
    | Tuple(r, atoms, cont) => Tuple(r, atoms, eliminate(cont))
    | Proj(r, x, i, cont) => Proj(r, x, i, eliminate(cont))
    | Halt(_) => t
    }
  }

  eliminate(term)
}

// Extract all functions to top level
let extractFunctions = (term: ANF.t): (list<(Ast.varName, list<Ast.varName>, ANF.t)>, ANF.t) => {
  let functions = ref(list{})

  let rec extract = (t: ANF.t): ANF.t => {
    switch t {
    | Fun(f, params, body, cont) => {
        let hoistedBody = extract(body)
        functions := list{(f, params, hoistedBody), ...functions.contents}
        extract(cont) // Continue with rest, function is removed from main flow
      }
    | App(r, f, args, cont) => App(r, f, args, extract(cont))
    | Bop(r, op, x, y, cont) => Bop(r, op, x, y, extract(cont))
    | If(cond, thenBranch, elseBranch) => If(cond, extract(thenBranch), extract(elseBranch))
    | Tuple(r, atoms, cont) => Tuple(r, atoms, extract(cont))
    | Proj(r, x, i, cont) => Proj(r, x, i, extract(cont))
    | Join(j, param, body, cont) => Join(j, param, extract(body), extract(cont))
    | Jump(_, _) | Halt(_) => t
    }
  }

  let mainFlow = extract(term)
  (List.reverse(functions.contents), mainFlow)
}

// Reconstruct ANF with functions at top level
let reconstructWithFunctions = (functions: list<(Ast.varName, list<Ast.varName>, ANF.t)>, mainFlow: ANF.t): ANF.t => {
  let rec reconstruct = (funcs: list<(Ast.varName, list<Ast.varName>, ANF.t)>, acc: ANF.t): ANF.t => {
    switch funcs {
    | list{} => acc
    | list{(f, params, body), ...rest} => Fun(f, params, body, reconstruct(rest, acc))
    }
  }
  reconstruct(functions, mainFlow)
}

// Main hoisting function - creates straightline code
let hoist = (term: ANF.t): ANF.t => {
  term
    -> eliminateJoinPoints  // Step 1: Remove all Join/Jump constructs
    -> extractFunctions     // Step 2: Extract functions to top level
    -> (((functions, mainFlow)) => reconstructWithFunctions(functions, mainFlow))
}
