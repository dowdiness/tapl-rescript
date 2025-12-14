// AST types for Lambda Calculus

pub type VarName = String

pub enum Bop {
  Plus
  Minus
} derive(Show, Eq)

pub enum Term {
  // Integer
  Int(Int)
  // Variable
  Var(VarName)
  // Lambda abstraction
  Lam(VarName, Term)
  // Application
  App(Term, Term)
  // Binary operation
  Bop(Bop, Term, Term)
  // If-then-else
  If(Term, Term, Term)
} derive(Show, Eq)

let counter : Ref[Int] = {val: -1}

pub fn fresh(prefix : String) -> String {
  counter.val = counter.val + 1
  prefix + counter.val.to_string()
}

pub fn rename(term : Term) -> Result[Term, String] {
  fn go(env : Map[String, String], t : Term) -> Result[Term, String] {
    match t {
      Int(i) => Ok(Int(i))
      Var(name) => {
        match env.get(name) {
          Some(var_name) => Ok(Var(var_name))
          None => {
            let msg = "Variable not found in environment: " + name
            Err(msg)
          }
        }
      }
      Lam(name, t) => {
        let feesh_name = fresh(name)
        env.set(name, feesh_name)
        match go(env, t) {
          Ok(evaled_t) => Ok(Lam(feesh_name, evaled_t))
          Err(e) => return Err(e)
        }
      }
      App(t1, t2) => {
        let t1_prime = go(env, t1)
        let t2_prime = go(env, t2)
        match (t1_prime, t2_prime) {
          (Ok(t1), Ok(t2)) => Ok(App(t1, t2))
          (Err(e), _) => Err(e)
          (_, Err(e)) => Err(e)
        }
      }
      Bop(op, t1, t2) => {
        let t1_prime = go(env, t1)
        let t2_prime = go(env, t2)
        match (t1_prime, t2_prime) {
          (Ok(t1), Ok(t2)) => Ok(Bop(op, t1, t2))
          (Err(e), _) => Err(e)
          (_, Err(e)) => Err(e)
        }
      }
      If(t1, t2, t3) => {
        let cond = go(env, t1)
        let then_branch = go(env, t2)
        let else_branch = go(env, t3)
        match (cond, then_branch, else_branch) {
          (Ok(cond), Ok(then_branch), Ok(else_branch)) => Ok(If(cond, then_branch, else_branch))
          _ => Err("Error in If expression")
        }
      }
    }
  }
  let empty_env : Map[String, String] = Map::new()
  go(empty_env, term)
}

pub fn print_term(term : Term) -> String {
  fn go(t : Term) -> String {
    match t {
      Int(i) => i.to_string()
      Var(x) => x
      Lam(x, t) => "(λ" + x + ". " + go(t) + ")"
      App(t1, t2) => "(" + go(t1) + " " + go(t2) + ")"
      Bop(Plus, t1, t2) => "(" + go(t1) + " + " + go(t2) + ")"
      Bop(Minus, t1, t2) => "(" + go(t1) + " - " + go(t2) + ")"
      If(t1, t2, t3) => "if " + go(t1) + " then " + go(t2) + " else " + go(t3)
    }
  }
  go(term)
}
