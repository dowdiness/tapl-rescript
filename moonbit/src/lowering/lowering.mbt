// Helper function to convert atom to string representation

// Basic arithmetic operations and primitives

pub suberror LoweringError String
pub fn lowering(expr: @parser.Term) -> String raise {
  fn go(expr: @parser.Term) -> String raise {
    match expr {
      Bop(Plus, t1, t2) => {
        let t1 = go(t1)
        let t2 = go(t2)
        "    mov  eax, \{t1}\n    add  eax, \{t2}\n"
      }
      // App(_) => ...
      // Lam(_) => ...
      // Var(x) => "    mov  %rax, " + x
      Int(i) => i.to_string()
      // If(_) => ...
      e => raise LoweringError(@parser.print_term(e))
    }
  }

  let body = go(expr)

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
