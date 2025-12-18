// Helper function to convert atom to string representation

// Basic arithmetic operations and primitives

pub suberror LoweringError String

// Helper function to convert atom to string representation
fn atomToASM(atom: @compile.Atom) -> String {
  match atom {
    AtomInt(i) => "$\{i.to_string()}"
    AtomVar(x) => "%\{x}"
    AtomGlob(x) => "@\{x}"
  }
}

// let atomToTypedLLVM = atom => {
//   `i64 ${atomToLLVM(atom)}`
// }

fn bopToASM(_: @parser.VarName, bop: @parser.Bop, t1: @compile.Atom, t2: @compile.Atom) -> String {
  let t1 = atomToASM(t1)
  let t2 = atomToASM(t2)
  match bop {
    Plus => {
      $|    mov  eax, \{t1}
      $|    add  eax, \{t2}
    }
    Minus => {
      $|    mov  eax, \{t1}
      $|    sub  eax, \{t2}
    }
  }
}
pub fn lowering(anf: @compile.ANF) -> String raise {
  fn go(anf: @compile.ANF, insts: @list.List[String]) -> @list.List[String] raise {
    match anf {
      Halt(_) => insts
      Bop(r, bop, t1, t2, k) => go(k, insts.add(bopToASM(r, bop, t1, t2)))
      // App(_) => ...
      // Lam(_) => ...
      // Var(x) => "    mov  %rax, " + x
      // If(_) => ...
      e => raise LoweringError(@compile.print_anf(e))
    }
  }

  let body = go(anf, @list.empty()).rev().to_array().join("\n  ")

  let section =
    #|section .text
    #|    global _start
    #|

  let start =
    #|_start:

  let exit_code =
    #|    mov  rax, 60
    #|    mov  edi, 3
    #|    syscall

  "\{section}\n\{start}\n\{body}\n\{exit_code}"
}
