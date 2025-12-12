// LLVMlite Lowering
module LLVMLowering = {
  // Helper function to convert atom to string representation
  let atomToLLVM = (atom: ANF.atom): string => {
    switch atom {
    | AtomInt(i) => Int.toString(i)
    | AtomVar(x) => `%${x}`
    | AtomGlob(x) => `@${x}`
    }
  }

  let atomToTypedLLVM = atom => {
    `i64 ${atomToLLVM(atom)}`
  }

  let bopToLLVM = (r: Ast.varName, bop: Ast.bop, x, y): string => {
    let x = atomToLLVM(x)
    let y = atomToLLVM(y)
    switch bop {
      | Plus => `%${r} = add i64 ${x}, ${y}`
      | Minus => `%${r} = sub i64 ${x}, ${y}`
    }
  }
  // Phase 1: Basic arithmetic operations and primitives
  let lowerPhase1 = (anf: ANF.t): string => {
    let rec go = (t: ANF.t, insts: list<string>) => {
      switch t {
      | Halt(AtomInt(n)) => insts->List.add(`ret i64 ${Int.toString(n)}`)
      | Halt(AtomVar(x)) => insts->List.add(`ret i64 %${x}`)
      | Bop(r, bop, x, y, e) => {
          go(e, insts->List.add(bopToLLVM(r, bop, x, y)))
        }
      | _ => failwith("Phase 1: Unsupported ANF construct")
      }
    }

    let insts = go(anf, list{})

    let body = insts->List.reverse->List.toArray->Array.join("\n  ")
    `define i64 @main() {\nentry:\n  ${body}\n}`
  }

  // Phase 2: Function definitions and calls
  let lowerPhase2 = (anf: ANF.t): string => {
    let functions = ref(list{})
    let mainInstructions = ref(list{})

    let rec extractFunctions = (t: ANF.t) => {
      switch t {
      | Fun(f, params, body, cont) => {
          // Generate function definition
          let paramList = params->List.map(p => `i64 %${p}`)->List.toArray->Array.join(", ")
          let bodyInstructions = ref(list{})

          let rec generateBody = (bodyTerm: ANF.t) => {
            switch bodyTerm {
            | Halt(AtomInt(n)) =>
                bodyInstructions := list{`ret i64 ${Int.toString(n)}`, ...bodyInstructions.contents}
            | Halt(AtomVar(x)) =>
                bodyInstructions := list{`ret i64 %${x}`, ...bodyInstructions.contents}
            | Bop(r, Plus, x, y, e) => {
                bodyInstructions := list{`%${r} = add i64 ${atomToLLVM(x)}, ${atomToLLVM(y)}`, ...bodyInstructions.contents}
                generateBody(e)
              }
            | Bop(r, Minus, x, y, e) => {
                bodyInstructions := list{`%${r} = sub i64 ${atomToLLVM(x)}, ${atomToLLVM(y)}`, ...bodyInstructions.contents}
                generateBody(e)
              }
            | _ => failwith("Phase 2: Unsupported construct in function body")
            }
          }

          generateBody(body)

          let bodyStr = bodyInstructions.contents->List.reverse->List.toArray->Array.join("\n  ")
          let funcDef = `define i64 @${f}(${paramList}) {\nentry:\n  ${bodyStr}\n}`

          functions := list{funcDef, ...functions.contents}
          extractFunctions(cont)
        }
      | Halt(AtomVar(x)) => {
          // Check if this is a function reference by looking at the functions list
          let isFunctionName = functions.contents->List.some(funcDef =>
            Js.String2.includes(funcDef, `@${x}(`)
          )
          if isFunctionName {
            // Function reference - return function pointer (not supported in simple Phase 2)
            failwith(`Phase 2: Function references not yet supported: ${x}`)
          } else {
            // Variable reference - return the variable value
            mainInstructions := list{`ret i64 %${x}`, ...mainInstructions.contents}
          }
        }
      | Halt(AtomInt(n)) =>
          mainInstructions := list{`ret i64 ${Int.toString(n)}`, ...mainInstructions.contents}
      | App(r, f, args, e) => {
          // Simple direct function call (Phase 2 - no closures yet)
          let argList = args->List.map(atomToTypedLLVM)->List.toArray->Array.join(", ")
          mainInstructions := list{`%${r} = call i64 @${f}(${argList})`, ...mainInstructions.contents}
          extractFunctions(e)
        }
      | Bop(r, Plus, x, y, e) => {
          mainInstructions := list{`%${r} = add i64 ${atomToLLVM(x)}, ${atomToLLVM(y)}`, ...mainInstructions.contents}
          extractFunctions(e)
        }
      | Bop(r, Minus, x, y, e) => {
          mainInstructions := list{`%${r} = sub i64 ${atomToLLVM(x)}, ${atomToLLVM(y)}`, ...mainInstructions.contents}
          extractFunctions(e)
        }
      | _ => failwith("Phase 2: Unsupported ANF construct")
      }
    }

    extractFunctions(anf)

    let functionsStr = functions.contents->List.reverse->List.toArray->Array.join("\n\n")
    let mainBody = mainInstructions.contents->List.reverse->List.toArray->Array.join("\n  ")
    let mainFunc = `define i64 @main() {\nentry:\n  ${mainBody}\n}`

    if List.length(functions.contents) > 0 {
      `${functionsStr}\n\n${mainFunc}`
    } else {
      mainFunc
    }
  }
  // Phase 3: Closures and memory management (Tuple, Proj)
  let lowerPhase3 = (anf: ANF.t): string => {
    let functions = ref(list{})
    let mainInstructions = ref(list{})

    let rec extractFunctions = (t: ANF.t) => {
      switch t {
      | Fun(f, params, body, cont) => {
          // Generate function definition
          let paramList = params->List.map(p => `i64 %${p}`)->List.toArray->Array.join(", ")
          let bodyInstructions = ref(list{})

          let rec generateBody = (bodyTerm: ANF.t) => {
            switch bodyTerm {
            | Halt(AtomInt(n)) =>
                bodyInstructions := list{`ret i64 ${Int.toString(n)}`, ...bodyInstructions.contents}
            | Halt(AtomVar(x)) =>
                bodyInstructions := list{`ret i64 %${x}`, ...bodyInstructions.contents}
            | Bop(r, Plus, x, y, e) => {
                bodyInstructions := list{`%${r} = add i64 ${atomToLLVM(x)}, ${atomToLLVM(y)}`, ...bodyInstructions.contents}
                generateBody(e)
              }
            | Bop(r, Minus, x, y, e) => {
                bodyInstructions := list{`%${r} = sub i64 ${atomToLLVM(x)}, ${atomToLLVM(y)}`, ...bodyInstructions.contents}
                generateBody(e)
              }
            | Tuple(r, vs, e) => {
                // Create tuple structure
                let size = List.length(vs)

                // Allocate memory for tuple
                bodyInstructions := list{`%${r}_ptr = alloca { ${Array.make(~length=size, "i64")->Array.join(", ")} }`, ...bodyInstructions.contents}

                // Store each element
                vs->List.mapWithIndex((atom, i) => {
                  let gepInstr = `%${r}_gep${Int.toString(i)} = getelementptr { ${Array.make(~length=size, "i64")->Array.join(", ")} }, { ${Array.make(~length=size, "i64")->Array.join(", ")} }* %${r}_ptr, i32 0, i32 ${Int.toString(i)}`
                  // Convert function pointers to i64
                  let storeInstr = switch atom {
                  | AtomGlob(fname) => {
                      let tmpVar = `${r}_tmp${Int.toString(i)}`
                      bodyInstructions := list{`%${tmpVar} = ptrtoint i64 (i64, i64)* @${fname} to i64`, ...bodyInstructions.contents}
                      `store i64 %${tmpVar}, i64* %${r}_gep${Int.toString(i)}`
                    }
                  | _ => `store i64 ${atomToLLVM(atom)}, i64* %${r}_gep${Int.toString(i)}`
                  }
                  bodyInstructions := list{storeInstr, gepInstr, ...bodyInstructions.contents}
                })->ignore

                // Cast to i64 for compatibility (simplified approach)
                bodyInstructions := list{`%${r} = ptrtoint { ${Array.make(~length=size, "i64")->Array.join(", ")} }* %${r}_ptr to i64`, ...bodyInstructions.contents}
                generateBody(e)
              }
            | Proj(r, x, i, e) => {
                // Project from tuple - simplified approach (assume 3-element tuples)
                let ptrVar = `${x}_ptr_${r}`
                let gepVar = `${r}_gep`
                bodyInstructions := list{`%${ptrVar} = inttoptr i64 %${x} to { i64, i64, i64 }*`, ...bodyInstructions.contents}
                bodyInstructions := list{`%${gepVar} = getelementptr { i64, i64, i64 }, { i64, i64, i64 }* %${ptrVar}, i32 0, i32 ${Int.toString(i)}`, ...bodyInstructions.contents}
                bodyInstructions := list{`%${r} = load i64, i64* %${gepVar}`, ...bodyInstructions.contents}
                generateBody(e)
              }
            | _ => failwith("Phase 3: Unsupported construct in function body")
            }
          }

          generateBody(body)

          let bodyStr = bodyInstructions.contents->List.reverse->List.toArray->Array.join("\n  ")
          let funcDef = `define i64 @${f}(${paramList}) {\nentry:\n  ${bodyStr}\n}`

          functions := list{funcDef, ...functions.contents}
          extractFunctions(cont)
        }
      | Halt(AtomVar(x)) => {
          // Check if this is a function reference by looking at the functions list
          let isFunctionName = functions.contents->List.some(funcDef =>
            Js.String2.includes(funcDef, `@${x}(`)
          )
          if isFunctionName {
            // Function reference - return function pointer (not supported in simple Phase 3)
            failwith(`Phase 3: Function references not yet supported: ${x}`)
          } else {
            // Variable reference - return the variable value
            mainInstructions := list{`ret i64 %${x}`, ...mainInstructions.contents}
          }
        }
      | Halt(AtomInt(n)) =>
          mainInstructions := list{`ret i64 ${Int.toString(n)}`, ...mainInstructions.contents}
      | App(r, f, args, e) => {
          // Indirect function call through closure (Phase 3)
          let argList = args->List.map(atom => {
            switch atom {
            | AtomInt(i) => `i64 ${Int.toString(i)}`
            | AtomVar(x) => `i64 %${x}`
            | AtomGlob(x) => `i64 @${x}`
            }
          })->List.toArray->Array.join(", ")
          // Convert i64 to function pointer and make indirect call
          let fptrVar = `${f}_fptr`
          mainInstructions := list{`%${fptrVar} = inttoptr i64 %${f} to i64 (i64, i64)*`, ...mainInstructions.contents}
          mainInstructions := list{`%${r} = call i64 %${fptrVar}(${argList})`, ...mainInstructions.contents}
          extractFunctions(e)
        }
      | Bop(r, Plus, x, y, e) => {
          mainInstructions := list{`%${r} = add i64 ${atomToLLVM(x)}, ${atomToLLVM(y)}`, ...mainInstructions.contents}
          extractFunctions(e)
        }
      | Bop(r, Minus, x, y, e) => {
          mainInstructions := list{`%${r} = sub i64 ${atomToLLVM(x)}, ${atomToLLVM(y)}`, ...mainInstructions.contents}
          extractFunctions(e)
        }
      | Tuple(r, vs, e) => {
          // Create tuple structure in main
          let size = List.length(vs)

          // Allocate memory for tuple
          mainInstructions := list{`%${r}_ptr = alloca { ${Array.make(~length=size, "i64")->Array.join(", ")} }`, ...mainInstructions.contents}

          // Store each element
          vs->List.mapWithIndex((atom, i) => {
            let gepInstr = `%${r}_gep${Int.toString(i)} = getelementptr { ${Array.make(~length=size, "i64")->Array.join(", ")} }, { ${Array.make(~length=size, "i64")->Array.join(", ")} }* %${r}_ptr, i32 0, i32 ${Int.toString(i)}`
            // Convert function pointers to i64
            let storeInstr = switch atom {
            | AtomGlob(fname) => {
                let tmpVar = `${r}_tmp${Int.toString(i)}`
                mainInstructions := list{`%${tmpVar} = ptrtoint i64 (i64, i64)* @${fname} to i64`, ...mainInstructions.contents}
                `store i64 %${tmpVar}, i64* %${r}_gep${Int.toString(i)}`
              }
            | _ => `store i64 ${atomToLLVM(atom)}, i64* %${r}_gep${Int.toString(i)}`
            }
            mainInstructions := list{storeInstr, gepInstr, ...mainInstructions.contents}
          })->ignore

          // Cast to i64 for compatibility
          mainInstructions := list{`%${r} = ptrtoint { ${Array.make(~length=size, "i64")->Array.join(", ")} }* %${r}_ptr to i64`, ...mainInstructions.contents}
          extractFunctions(e)
        }
      | Proj(r, x, i, e) => {
          // Project from tuple in main - we need to know the tuple size
          // For simplicity, assume all tuples have the same structure for now
          let ptrVar = `${x}_ptr_${r}`
          let gepVar = `${r}_gep`
          mainInstructions := list{`%${ptrVar} = inttoptr i64 %${x} to { i64, i64, i64 }*`, ...mainInstructions.contents}
          mainInstructions := list{`%${gepVar} = getelementptr { i64, i64, i64 }, { i64, i64, i64 }* %${ptrVar}, i32 0, i32 ${Int.toString(i)}`, ...mainInstructions.contents}
          mainInstructions := list{`%${r} = load i64, i64* %${gepVar}`, ...mainInstructions.contents}
          extractFunctions(e)
        }
      | _ => failwith("Phase 3: Unsupported ANF construct")
      }
    }

    extractFunctions(anf)

    let functionsStr = functions.contents->List.reverse->List.toArray->Array.join("\n\n")
    let mainBody = mainInstructions.contents->List.reverse->List.toArray->Array.join("\n  ")
    let mainFunc = `define i64 @main() {\nentry:\n  ${mainBody}\n}`

    if List.length(functions.contents) > 0 {
      `${functionsStr}\n\n${mainFunc}`
    } else {
      mainFunc
    }
  }

  // Phase 4: Control flow (If, Join, Jump)
  let lowerPhase4 = (anf: ANF.t): string => {
    let functions = ref(list{})
    let mainInstructions = ref(list{})
    let labelCounter = ref(0)

    let getNextLabel = (prefix: string) => {
      labelCounter := labelCounter.contents + 1
      `${prefix}${Int.toString(labelCounter.contents)}`
    }

    // https://llvm.org/docs/LangRef.html#br-instruction
    // let br = (j) => `br label %${j}`

    let rec extractFunctions = (t: ANF.t) => {
      switch t {
      | Fun(f, params, body, cont) => {
          // Generate function definition
          let paramList = params->List.map(p => `i64 %${p}`)->List.toArray->Array.join(", ")
          let bodyInstructions = ref(list{})

          let rec generateBody = (bodyTerm: ANF.t) => {
            switch bodyTerm {
            | Halt(AtomInt(n)) =>
                bodyInstructions := list{`ret i64 ${Int.toString(n)}`, ...bodyInstructions.contents}
            | Halt(AtomVar(x)) =>
                bodyInstructions := list{`ret i64 %${x}`, ...bodyInstructions.contents}
            | Bop(r, Plus, x, y, e) => {
                bodyInstructions := list{`%${r} = add i64 ${atomToLLVM(x)}, ${atomToLLVM(y)}`, ...bodyInstructions.contents}
                generateBody(e)
              }
            | Bop(r, Minus, x, y, e) => {
                bodyInstructions := list{`%${r} = sub i64 ${atomToLLVM(x)}, ${atomToLLVM(y)}`, ...bodyInstructions.contents}
                generateBody(e)
              }
            | If(cond, thenBranch, elseBranch) => {
                let thenLabel = getNextLabel("then")
                let elseLabel = getNextLabel("else")

                // Generate condition check
                bodyInstructions := list{`%cond = icmp ne i64 ${atomToLLVM(cond)}, 0`, ...bodyInstructions.contents}
                bodyInstructions := list{`br i1 %cond, label %${thenLabel}, label %${elseLabel}`, ...bodyInstructions.contents}

                // Generate then branch
                bodyInstructions := list{`${thenLabel}:`, ...bodyInstructions.contents}
                generateBody(thenBranch)

                // Generate else branch
                bodyInstructions := list{`${elseLabel}:`, ...bodyInstructions.contents}
                generateBody(elseBranch)
              }
            | term => failwith(`Phase 4: Unsupported construct in function body: ${ANF.printANF(term)}`)
            }
          }

          generateBody(body)

          let bodyStr = bodyInstructions.contents->List.reverse->List.toArray->Array.join("\n  ")
          let funcDef = `define i64 @${f}(${paramList}) {\nentry:\n  ${bodyStr}\n}`

          functions := list{funcDef, ...functions.contents}
          extractFunctions(cont)
        }
      | Halt(AtomVar(x)) => {
          // Check if this is a function reference by looking at the functions list
          let isFunctionName = functions.contents->List.some(funcDef =>
            Js.String2.includes(funcDef, `@${x}(`)
          )
          if isFunctionName {
            // Function reference - return function pointer (not supported in simple Phase 4)
            failwith(`Phase 4: Function references not yet supported: ${x}`)
          } else {
            // Variable reference - return the variable value
            mainInstructions := list{`ret i64 %${x}`, ...mainInstructions.contents}
          }
        }
      | Halt(AtomInt(n)) =>
          mainInstructions := list{`ret i64 ${Int.toString(n)}`, ...mainInstructions.contents}
      | If(cond, thenBranch, elseBranch) => {
          let thenLabel = getNextLabel("then")
          let elseLabel = getNextLabel("else")

          // Generate condition check
          mainInstructions := list{`%cond = icmp ne i64 ${atomToLLVM(cond)}, 0`, ...mainInstructions.contents}
          mainInstructions := list{`br i1 %cond, label %${thenLabel}, label %${elseLabel}`, ...mainInstructions.contents}

          // Generate then branch
          mainInstructions := list{`${thenLabel}:`, ...mainInstructions.contents}
          extractFunctions(thenBranch)

          // Generate else branch
          mainInstructions := list{`${elseLabel}:`, ...mainInstructions.contents}
          extractFunctions(elseBranch)
        }
      | Bop(r, Plus, x, y, e) => {
          mainInstructions := list{`%${r} = add i64 ${atomToLLVM(x)}, ${atomToLLVM(y)}`, ...mainInstructions.contents}
          extractFunctions(e)
        }
      | Bop(r, Minus, x, y, e) => {
          mainInstructions := list{`%${r} = sub i64 ${atomToLLVM(x)}, ${atomToLLVM(y)}`, ...mainInstructions.contents}
          extractFunctions(e)
        }
      | Jump(j, None) => {
        mainInstructions := list{`br label %${j}`, ...mainInstructions.contents}
      }
      | Join(_) => failwith("Code must be straightline!")
      | _ => failwith("Phase 4: Unsupported ANF construct")
      }
    }

    extractFunctions(anf)

    let functionsStr = functions.contents->List.reverse->List.toArray->Array.join("\n\n")
    let mainBody = mainInstructions.contents->List.reverse->List.toArray->Array.join("\n  ")
    let mainFunc = `define i64 @main() {\nentry:\n  ${mainBody}\n}`

    if List.length(functions.contents) > 0 {
      `${functionsStr}\n\n${mainFunc}`
    } else {
      mainFunc
    }
  }
}

module Compiler = {
  let compile = (term: Ast.t) => {
    term->Ast.rename->ANF.convert->ClosureConversion.convert->Hoisting.hoist
  }

  let compileToLLVM = (term: Ast.t, phase: int) => {
    let anf = compile(term)
    switch phase {
    | 1 => LLVMLowering.lowerPhase1(anf)
    | 2 => LLVMLowering.lowerPhase2(anf)
    | 3 => LLVMLowering.lowerPhase3(anf)
    | 4 => LLVMLowering.lowerPhase4(anf)
    | _ => failwith("Unsupported LLVM lowering phase")
    }
  }
}
