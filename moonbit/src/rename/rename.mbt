// Uniquify variable names in a term to ensure all variables have unique names.

pub suberror RenameError String
///|
let counter : Ref[Int] = { val: -1 }

///|
pub fn fresh(prefix : String) -> String {
  counter.val = counter.val + 1
  prefix + counter.val.to_string()
}

pub fn reset() -> Unit {
  counter.val = -1
}

///|
pub fn rename(term : @parser.Term) -> @parser.Term raise RenameError {
  fn go(env : Map[String, String], t : @parser.Term) -> @parser.Term raise RenameError {
    match t {
      Int(i) => Int(i)
      Var(name) => {
        match env.get(name) {
          Some(var_name) => Var(var_name)
          None => {
            let msg = "Variable not found in environment: " + name
            raise RenameError(msg)
          }
        }
      }
      Lam(name, t) => {
        let feesh_name = fresh(name)
        env.set(name, feesh_name)
        let evaled_t = go(env, t)
        Lam(feesh_name, evaled_t)
      }
      App(t1, t2) => {
        let evaled_t1 = go(env, t1)
        let evaled_t2 = go(env, t2)
        App(evaled_t1, evaled_t2)
      }
      Bop(op, t1, t2) => {
        let evaled_t1 = go(env, t1)
        let evaled_t2 = go(env, t2)
        Bop(op, evaled_t1, evaled_t2)
      }
      If(t1, t2, t3) => {
        let cond = go(env, t1)
        let then_branch = go(env, t2)
        let else_branch = go(env, t3)
        If(cond, then_branch, else_branch)
      }
    }
  }

  let empty_env : Map[String, String] = Map::new()
  go(empty_env, term)
}
