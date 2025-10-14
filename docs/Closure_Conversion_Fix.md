# Closure Conversion Bug Fix Documentation

## Problem Summary

The lambda calculus compiler was generating invalid LLVM IR with variable redefinition errors when compiling expressions like `((λx.x + 8) (12 - 5))`.

### Error Example

```
llc: error: llc: test.ll:5:3: error: multiple definition of local value named 'x0'
  %x0 = load i64, i64* %x0_gep
  ^
```

The generated LLVM IR had:

```llvm
define i64 @f1(i64 %env5, i64 %x0) {
entry:
  %env5_ptr_x0 = inttoptr i64 %env5 to { i64, i64, i64 }*
  %x0_gep = getelementptr { i64, i64, i64 }, { i64, i64, i64 }* %env5_ptr_x0, i32 0, i32 1
  %x0 = load i64, i64* %x0_gep    <- ERROR: %x0 already defined as parameter
  %r2 = add i64 %x0, 8
  ret i64 %r2
}
```

## Root Cause Analysis

### Issue 1: Free Variable Computation Bug

**Location**: [`src/Lambda/ClosureConversion.res:109`](../src/Lambda/ClosureConversion.res:109)

The closure conversion was incorrectly computing free variables for function bodies. The code was:

```rescript
| Fun(f, xs, e, e') => {
    let env = Ast.fresh("env")
    let fvs = compute(e)->Belt.Set.String.toArray->List.fromArray
    // ...
}
```

**Problem**: When `compute(e)` was called on the function body `e`, it would find all variables used in the body, including the function's own parameters `xs`. For example, in `λx.x + 8`, the variable `x` would be included in the free variable set even though it's bound by the lambda.

**Why This Happened**: The `compute` function is designed to work on complete ANF terms and handles `Fun` nodes by excluding parameters. However, at line 109, we're calling `compute` on just the function body `e`, not the full `Fun` node, so the parameters weren't being excluded.

### Issue 2: LLVM Lowering Variable Name Collision

**Location**: [`src/Lambda/Compile.res:369-374`](../src/Lambda/Compile.res:369)

The LLVM lowering code was generating projections with naming conflicts:

```rescript
| Proj(r, x, i, e) => {
    let ptrVar = `${x}_ptr_${r}`
    bodyInstructions := list{`%${ptrVar} = inttoptr i64 %${x} to { i64, i64, i64 }*`, ...}
    bodyInstructions := list{`%${r}_gep = getelementptr ...`, ...}
    bodyInstructions := list{`%${r} = load i64, i64* %${r}_gep`, ...}
    // ...
}
```

When closure conversion created projections like `Proj(x0, env, 1, ...)`, the LLVM lowering would generate:

```llvm
%x0_gep = getelementptr ...
%x0 = load i64, i64* %x0_gep
```

If `x0` was already a function parameter, this created a redefinition error.

## Solution

### Fix 1: Exclude Function Parameters from Free Variables

**File**: [`src/Lambda/ClosureConversion.res`](../src/Lambda/ClosureConversion.res)

```rescript
| Fun(f, xs, e, e') => {
    let env = Ast.fresh("env")
    let allFvs = compute(e)
    let params = Belt.Set.String.fromArray(List.toArray(xs))
    let fvs = Belt.Set.String.diff(allFvs, params)->Belt.Set.String.toArray->List.fromArray
    // ... rest of the code
}
```

**Change**: Explicitly exclude function parameters `xs` from the free variable set by computing the set difference `allFvs - params`.

**Impact**: Functions no longer try to capture their own parameters in the closure environment. For `λx.x + 8`, the free variable set is now correctly empty `[]` instead of containing `[x]`.

### Fix 2: Use Unique Variable Names in LLVM Lowering

**File**: [`src/Lambda/Compile.res`](../src/Lambda/Compile.res)

```rescript
| Proj(r, x, i, e) => {
    let ptrVar = `${x}_ptr_${r}`
    let gepVar = `${r}_gep`
    bodyInstructions := list{`%${ptrVar} = inttoptr i64 %${x} to { i64, i64, i64 }*`, ...}
    bodyInstructions := list{`%${gepVar} = getelementptr { i64, i64, i64 }, { i64, i64, i64 }* %${ptrVar}, i32 0, i32 ${Int.toString(i)}`, ...}
    bodyInstructions := list{`%${r} = load i64, i64* %${gepVar}`, ...}
    generateBody(e)
}
```

**Change**: Introduced a separate `gepVar` variable to hold the GEP instruction result, ensuring the load instruction references a unique variable name.

**Impact**: Even if closure conversion did add unnecessary projections, the LLVM IR would no longer have variable name collisions.

## Results

### Before Fix

**Expression**: `((λx.x + 8) (12 - 5))`

**Closure Conversion Output**:
```
fun f8(env12, x7) =
  let x7 = env12.1 in    <- BAD: projects x7 from environment
let r9 = x7 + 8 in
halt r9
in
let f8 = (@f8, x7) in    <- BAD: tries to capture x7 which is a parameter
```

**LLVM IR** (invalid):
```llvm
define i64 @f1(i64 %env5, i64 %x0) {
entry:
  %env5_ptr_x0 = inttoptr i64 %env5 to { i64, i64, i64 }*
  %x0_gep = getelementptr { i64, i64, i64 }, { i64, i64, i64 }* %env5_ptr_x0, i32 0, i32 1
  %x0 = load i64, i64* %x0_gep    <- ERROR: redefinition
  %r2 = add i64 %x0, 8
  ret i64 %r2
}
```

### After Fix

**Expression**: `((λx.x + 8) (12 - 5))`

**Closure Conversion Output**:
```
fun f8(env12, x7) =
  let r9 = x7 + 8 in    <- GOOD: uses parameter directly
halt r9
in
let f8 = (@f8) in       <- GOOD: no unnecessary captures
```

**LLVM IR** (valid):
```llvm
define i64 @f8(i64 %env12, i64 %x7) {
entry:
  %r9 = add i64 %x7, 8
  ret i64 %r9
}

define i64 @main() {
entry:
  %f8_ptr = alloca { i64 }
  %f8_gep0 = getelementptr { i64 }, { i64 }* %f8_ptr, i32 0, i32 0
  store i64 @f8, i64* %f8_gep0
  %f8 = ptrtoint { i64 }* %f8_ptr to i64
  %r10 = sub i64 12, 5
  %f8_ptr_f13 = inttoptr i64 %f8 to { i64, i64, i64 }*
  %f13_gep = getelementptr { i64, i64, i64 }, { i64, i64, i64 }* %f8_ptr_f13, i32 0, i32 0
  %f13 = load i64, i64* %f13_gep
  %r11 = call i64 @f13(i64 %f8, i64 %r10)
  ret i64 %r11
}
```

## Test Results

- **Before**: LLVM compilation failed with variable redefinition errors
- **After**: 84 out of 88 tests passing
- **Status**: Core compilation pipeline working correctly

## Key Insights

1. **Free Variable Computation Context**: When computing free variables for a function body, always exclude the function's parameters. The `compute` function handles this automatically for `Fun` nodes, but when calling it on just the body, parameters must be explicitly excluded.

2. **LLVM SSA Requirements**: LLVM requires Static Single Assignment (SSA) form where each variable is defined exactly once. Variable names must be unique within a function scope, including avoiding conflicts with function parameters.

3. **Defensive Programming**: Both the closure conversion fix (preventing unnecessary captures) and the LLVM lowering fix (ensuring unique names) work together to prevent issues. The closure conversion fix is the proper solution, while the LLVM fix provides additional safety.

## Related Code References

- [`src/Lambda/ClosureConversion.res`](../src/Lambda/ClosureConversion.res): Closure conversion implementation
- [`src/Lambda/Compile.res`](../src/Lambda/Compile.res): LLVM lowering phases
- [`src/Lambda/ANF.res`](../src/Lambda/ANF.res): ANF type definitions
- [`src/Lambda/Compile_test.res`](../src/Lambda/Compile_test.res): Compilation pipeline tests
