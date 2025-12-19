// ANF (A-Normal Form) Conversion
// https://compiler.club/anf-conversion/

///| Atoms - these are either variables or constants
pub enum Atom {
  AtomInt(Int)
  AtomVar(@parser.VarName)
  AtomGlob(@parser.VarName)
} derive(Show, Eq)

///| ANF representation
pub enum ANF {
  // Halt with a value
  Halt(Atom)
  // Function definition: Fun(name, params, body, continuation)
  Fun(@parser.VarName, @list.List[@parser.VarName], ANF, ANF)
  // Join point (φ function for SSA):
  // Join(label, param, body, continuation)
  // https://pages.cs.wisc.edu/~fischer/cs701.f08/lectures/Lecture23.4up.pdf
  Join(@parser.VarName, @parser.VarName?, ANF, ANF)
  // Jump to a join point: Jump(label, argument)
  Jump(@parser.VarName, Atom?)
  // Function application: App(result, function, arguments, continuation)
  App(@parser.VarName, @parser.VarName, @list.List[Atom], ANF)
  // Binary operation: Bop(result, operator, left, right, continuation)
  Bop(@parser.VarName, @parser.Bop, Atom, Atom, ANF)
  // Conditional: If(condition, then_branch, else_branch)
  If(Atom, ANF, ANF)
  // Tuple construction (for closure conversion): Tuple(result, values, continuation)
  Tuple(@parser.VarName, @list.List[Atom], ANF)
  // Tuple projection: Proj(result, tuple, index, continuation)
  Proj(@parser.VarName, @parser.VarName, Int, ANF)
} derive(Show, Eq)

///| Exception for invalid atom application
pub suberror MustApplyVarError Atom

///| Pretty print an atom
pub fn print_atom(atom : Atom) -> String {
  match atom {
    AtomInt(i) => i.to_string()
    AtomVar(x) => x
    AtomGlob(x) => "@" + x
  }
}

///|
test "print_atom" {
  inspect(print_atom(AtomInt(42)), content="42")
  inspect(print_atom(AtomInt(-10)), content="-10")
  inspect(print_atom(AtomInt(0)), content="0")
  inspect(print_atom(AtomVar("x")), content="x")
  inspect(print_atom(AtomVar("variable_name")), content="variable_name")
  inspect(print_atom(AtomGlob("global")), content="@global")
  inspect(print_atom(AtomGlob("sys")), content="@sys")
}

struct Indent {
  level: Int
  space: String
} derive(Eq)

pub impl Default for Indent with default() {
  Indent::{ level: 0, space: "  " }
}

impl Show for Indent with output(self, logger) {
  logger.write_string(self.space.repeat(self.level))
}

fn Indent::incr(self: Self) -> Self {
  { ..self, level: self.level + 1 }
}

fn Indent::decr(self: Self) -> Self {
  { ..self, level: self.level -1 }
}

///| Pretty print ANF
pub fn print_anf(anf : ANF) -> String {
  fn go(a : ANF, indent: Indent) -> String {
    let decredIndent = indent.decr()
    let incredIndent = indent.incr()
    match a {
      Halt(atom) => "halt \{print_atom(atom)}"
      Fun(f, xs, e, k) => {
        let params = xs.to_array().join(", ")
        "fun \{f}(\{params}) =\n\{incredIndent}\{go(e, incredIndent)}\n\{indent}in\n\{indent}\{go(k, indent)}"
      }
      Join(j, Some(p), e, k) => "join \{j} (\{p}) =\n\{incredIndent}\{go(e, incredIndent)}\n\{indent}in\n\{indent}\{go(k, incredIndent)}"
      Join(j, None, e, k) => "join \{j} =\n\{incredIndent}\{go(e, incredIndent)}\n\{indent}in\n\{indent}\{go(k, incredIndent)}"
      Jump(j, Some(atom)) => "jump \{j}(\{print_atom(atom)})"
      Jump(j, None) => "jump \{j}"
      App(r, f, vs, e) => {
        let args = vs.map(print_atom).to_array().join(", ")
        "let \{r} = \{f}(\{args}) in\n\{indent}\{go(e, indent)}"
      }
      Bop(r, Plus, x, y, e) => "let \{r} = \{print_atom(x)} + \{print_atom(y)} in\n\{indent}\{go(e, indent)}"
      Bop(r, Minus, x, y, e) => "let \{r} = \{print_atom(x)} - \{print_atom(y)} in\n\{indent}\{go(e, indent)}"
      If(atom, t, f) => "if \{print_atom(atom)} then\n\{indent}\{go(t, indent)} \n\{decredIndent}else\n\{indent}\{go(f, indent)}"
      Tuple(r, vs, e) => {
        let values = vs.map(print_atom).to_array().join(", ")
        "let \{r} = (\{values}) in\n\{indent}\{go(e, indent)}"
      }
      Proj(r, x, i, e) => "let \{r} = \{x}.\{i.to_string()} in\n\{indent}\{go(e, indent)}"
    }
  }

  go(anf, Indent::default())
}

///|
test "print_anf/basic_constructs" {
  // Test Halt
  inspect(print_anf(Halt(AtomInt(42))), content="halt 42")

  // Test Jump with and without arguments
  inspect(
    print_anf(Jump("label1", Some(AtomVar("x")))),
    content="jump label1(x)",
  )
  inspect(print_anf(Jump("label2", None)), content="jump label2")
}

///|
test "print_anf/operations" {
  // Test binary operations (Plus and Minus)
  let plus_expr = Bop(
    "result",
    @parser.Plus,
    AtomInt(1),
    AtomInt(2),
    Halt(AtomVar("result")),
  )
  inspect(print_anf(plus_expr), content="let result = 1 + 2 in\nhalt result")
  let minus_expr = Bop(
    "result",
    @parser.Minus,
    AtomVar("x"),
    AtomInt(5),
    Halt(AtomVar("result")),
  )
  inspect(print_anf(minus_expr), content="let result = x - 5 in\nhalt result")

  // Test If expression
  let if_expr = If(AtomVar("cond"), Halt(AtomInt(1)), Halt(AtomInt(2)))
  inspect(print_anf(if_expr), content="if cond then\nhalt 1 \nelse\nhalt 2")
}

///|
test "print_anf/complex_constructs" {
  // Test Fun (function definition)
  let fun_expr = Fun(
    "f",
    @list.from_array(["x", "y"]),
    Halt(AtomVar("x")),
    Halt(AtomVar("f")),
  )
  inspect(print_anf(fun_expr), content="fun f(x, y) =\n  halt x\nin\nhalt f")

  // Test App (function application)
  let app_expr = App(
    "result",
    "func",
    @list.from_array([AtomInt(1), AtomVar("y")]),
    Halt(AtomVar("result")),
  )
  inspect(
    print_anf(app_expr),
    content="let result = func(1, y) in\nhalt result",
  )

  // Test Join with parameter and without parameter
  let join_with_param = Join(
    "j1",
    Some("p"),
    Halt(AtomVar("p")),
    Jump("j1", Some(AtomInt(42))),
  )
  inspect(
    print_anf(join_with_param),
    content="join j1 (p) =\n  halt p\nin\njump j1(42)",
  )
  let join_without_param = Join("j2", None, Halt(AtomInt(0)), Jump("j2", None))
  inspect(
    print_anf(join_without_param),
    content="join j2 =\n  halt 0\nin\njump j2",
  )

  // Test Tuple and Proj
  let tuple_expr = Tuple(
    "t",
    @list.from_array([AtomInt(1), AtomVar("x")]),
    Halt(AtomVar("t")),
  )
  inspect(print_anf(tuple_expr), content="let t = (1, x) in\nhalt t")
  let proj_expr = Proj("elem", "tuple", 0, Halt(AtomVar("elem")))
  inspect(print_anf(proj_expr), content="let elem = tuple.0 in\nhalt elem")
}

///| Helper function to create Halt
#warnings("-unused_error_type")
fn mk_halt(v : Atom) -> ANF raise? {
  Halt(v)
}

///|
test "mk_halt" {
  inspect(mk_halt(AtomInt(42)), content="Halt(AtomInt(42))")
  inspect(mk_halt(AtomVar("x")), content="Halt(AtomVar(\"x\"))")
  inspect(mk_halt(AtomGlob("global")), content="Halt(AtomGlob(\"global\"))")
}

///| let* operator for continuation-passing style in direct style
fn[A, B] letStar(f : (A) -> B raise?, k : A) -> B raise? {
  f(k)
}

///| ANF conversion algorithm using continuation-passing style (CPS)
pub fn convert(term : @parser.Term) -> ANF raise {
  // Main conversion function with continuation
  letrec go = fn(e : @parser.Term, k : (Atom) -> ANF raise) -> ANF raise {
    match e {
      // Integer literals: just pass to continuation
      Int(i) => k(AtomInt(i))
      // Variables: just pass to continuation
      Var(x) => k(AtomVar(x))
      // Lambda abstraction: create a function definition
      Lam(x, t) => {
        let f = @rename.fresh("f")
        let evaled_t = go(t, fn(v) { mk_halt(v) })
        let params = @list.from_array([x])
        Fun(f, params, evaled_t, k(AtomVar(f)))
      }
      // Function application
      App(f_term, x_term) => app_go(f_term, x_term, k)
      Bop(op, x_term, y_term) => bop_go(op, x_term, y_term, k)
      If(e_cond, t_then, t_else) => if_go(e_cond, t_then, t_else, k)
    }
  }

  and app_go = fn(f_term, x_term, k) raise {
    letStar(go(f_term, _), fn(f_atom) -> ANF raise {
      letStar(go(x_term, _), fn(x_atom) -> ANF raise {
        match f_atom {
          AtomVar(f_var) => {
            let r = @rename.fresh("r")
            let args = @list.from_array([x_atom])
            App(r, f_var, args, k(AtomVar(r)))
          }
          atom => raise MustApplyVarError(atom)
        }
      })
    })
  }

  // Binary operations
  and bop_go = fn(op, x_term, y_term, k) raise {
    // Convert left operand
    letStar(go(x_term, _), fn(x_atom) {
      // Then convert right operand
      letStar(go(y_term, _), fn(y_atom) {
        let r = @rename.fresh("r")
        Bop(r, op, x_atom, y_atom, k(AtomVar(r)))
      })
    })
  }

  // If-then-else: introduce join point
  // Example transformation:
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
  and if_go = fn(e_cond, t_then, t_else, k) raise {
    // We introduce Join Point here
    // https://compiler.club/compiling-lambda-calculus/#:~:text=we%20introduce%20a-,join%20point,-%3A
    letStar(go(e_cond, _), fn(e_atom) {
      // Create join point label
      let j = @rename.fresh("j")
      // Parameter to hold result from then/else branches
      let p = @rename.fresh("p")
      // Jump that will be called at the end of each branch
      let join_var = Jump(j, Some(AtomVar(p)))
      // Create the join point with both branches
      Join(
        j,
        Some(p),
        k(AtomVar(p)),
        If(
          e_atom,
          go(t_then, fn(_) { join_var }),
          go(t_else, fn(_) { join_var }),
        ),
      )
    })
  }

  // Entry point: convert term with halt continuation
  go(term, mk_halt)
}
