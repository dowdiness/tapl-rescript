# Lambda Compiler

A lambda calculus compiler that generates LLVM IR. Use the CLI for easy compilation:

```bash
# Install and build
pnpm install
pnpm run build

# Compile lambda expressions
lambda compile "((λx.x + 8) (12 - 5))"

# Show different phases
lambda compile --phase=1 "1 + 2"       # Basic arithmetic
lambda compile --phase=2 "λx.x"         # Functions
lambda compile --phase=3 "((λx.x + 8) (12 - 5))"  # Closures (default)
lambda compile --phase=4 "if 1 then 10 else 20"   # Control flow

# Show intermediate representations
lambda compile --ast "λx.x + 1"  # Abstract Syntax Tree
lambda compile --anf "λx.x + 1"  # A-Normal Form
```

### Programmatic Usage

```js
let source = "((λx.x + 8) (12 - 5))"
let testApp = parse(source)
let llvm = Compile.Compiler.compileToLLVM(testApp, 3)
Console.log(llvm)
```

```llvm
define i64 @f1(i64 %env5, i64 %x0) {
entry:
  %r2 = add i64 %x0, 8
  ret i64 %r2
}

define i64 @main() {
entry:
  %f1_ptr = alloca { i64 }
  %f1_tmp0 = ptrtoint i64 (i64, i64)* @f1 to i64
  %f1_gep0 = getelementptr { i64 }, { i64 }* %f1_ptr, i32 0, i32 0
  store i64 %f1_tmp0, i64* %f1_gep0
  %f1 = ptrtoint { i64 }* %f1_ptr to i64
  %r3 = sub i64 12, 5
  %f1_ptr_f6 = inttoptr i64 %f1 to { i64, i64, i64 }*
  %f6_gep = getelementptr { i64, i64, i64 }, { i64, i64, i64 }* %f1_ptr_f6, i32 0, i32 0
  %f6 = load i64, i64* %f6_gep
  %f6_fptr = inttoptr i64 %f6 to i64 (i64, i64)*
  %r4 = call i64 %f6_fptr(i64 %f1, i64 %r3)
  ret i64 %r4
}
```

### Running LLVM Output

```sh
# Compile expression to LLVM
lambda compile "((λx.x + 8) (12 - 5))" > main.ll

# Compile and run with LLVM
llc main.ll && gcc main.s -o main && ./main; echo $?
```
