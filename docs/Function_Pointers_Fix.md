# Function Pointers Fix Documentation

## Problem Summary

The lambda calculus compiler was generating invalid LLVM IR when storing function pointers in tuples during closure creation (Phase 3). The issue occurred when attempting to store a function pointer directly into a tuple without proper type conversion.

### Error Example

When compiling `((λx.x + 8) (12 - 5))`, the original code would attempt to generate:

```llvm
; INVALID - Cannot store function pointer directly into i64 slot
%f8_gep0 = getelementptr { i64 }, { i64 }* %f8_ptr, i32 0, i32 0
store i64 @f8, i64* %f8_gep0    ; ERROR: @f8 is a function, not i64
```

LLVM would reject this with an error like:
```
error: stored value and pointer type do not match
store i64 @f8, i64* %f8_gep0
```

## Root Cause Analysis

### Issue: Direct Storage of Function Pointers

**Location**: [`src/Lambda/Compile.res:362-369`](../src/Lambda/Compile.res:362) (Phase 3 tuple creation)

The original code attempted to store function pointers directly into i64 tuple slots:

```rescript
// ORIGINAL CODE (BROKEN)
| Tuple(r, vs, e) => {
    // ...
    vs->List.mapWithIndex((i, atom) => {
      let gepInstr = `%${r}_gep${Int.toString(i)} = getelementptr ...`
      let storeInstr = `store i64 ${atomToString(atom)}, i64* %${r}_gep${Int.toString(i)}`
      // This generates: store i64 @func, i64* ... when atom is AtomGlob
      bodyInstructions := list{storeInstr, gepInstr, ...bodyInstructions.contents}
    })->ignore
}
```

**Problem**: When `atom` is `AtomGlob(fname)` (a global function reference), `atomToString` returns `@fname`. This creates an instruction like `store i64 @fname, i64* %ptr`, which is invalid because:

1. `@fname` is a function pointer with type `i64 (i64, i64)*` (or similar)
2. The store destination expects a raw `i64` value
3. LLVM requires an explicit `ptrtoint` conversion to cast function pointers to integers

### Why This Happened

The closure conversion pass correctly identifies functions that need to be stored in closure tuples using the `AtomGlob` variant:

```rescript
// From ClosureConversion.res
let closureTuple = ANF.Tuple(f, list{ANF.AtomGlob(f), ...vs}, go(e'))
```

However, the LLVM lowering didn't handle the special case where function pointers need type conversion before storage.

## Solution

### Fix: Convert Function Pointers to i64 Before Storage

**File**: [`src/Lambda/Compile.res:362-369`](../src/Lambda/Compile.res:362)

```rescript
// FIXED CODE
vs->List.mapWithIndex((i, atom) => {
  let gepInstr = `%${r}_gep${Int.toString(i)} = getelementptr { ${Array.make(~length=size, "i64")->Array.joinWith(", ")} }, { ${Array.make(~length=size, "i64")->Array.joinWith(", ")} }* %${r}_ptr, i32 0, i32 ${Int.toString(i)}`

  // Convert function pointers to i64
  let storeInstr = switch atom {
  | AtomGlob(fname) => {
      let tmpVar = `${r}_tmp${Int.toString(i)}`
      bodyInstructions := list{`%${tmpVar} = ptrtoint i64 (i64, i64)* @${fname} to i64`, ...bodyInstructions.contents}
      `store i64 %${tmpVar}, i64* %${r}_gep${Int.toString(i)}`
    }
  | _ => `store i64 ${atomToString(atom)}, i64* %${r}_gep${Int.toString(i)}`
  }
  bodyInstructions := list{storeInstr, gepInstr, ...bodyInstructions.contents}
})->ignore
```

**Changes**:
1. **Pattern match on atom type**: Explicitly check if the atom is `AtomGlob` (function reference)
2. **Generate ptrtoint instruction**: Convert the function pointer to i64 using `ptrtoint i64 (i64, i64)* @fname to i64`
3. **Use temporary variable**: Store the converted value in a temporary variable `%${r}_tmp${i}`
4. **Store the i64 value**: Use the temporary variable in the store instruction instead of the raw function reference

**Impact**: Function pointers are now properly converted to integer values before being stored in tuples, satisfying LLVM's type system requirements.

### Applied in Multiple Locations

The same fix was applied in three locations:

1. **Phase 3 - Function body tuple creation** (lines 362-369)
2. **Phase 3 - Main function tuple creation** (lines 447-454)
3. Both handle the case where closures are created with function pointers

## Results

### Before Fix

**Expression**: `((λx.x + 8) (12 - 5))`

**Generated LLVM** (invalid):
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
  store i64 @f8, i64* %f8_gep0           ; ERROR: Type mismatch
  %f8 = ptrtoint { i64 }* %f8_ptr to i64
  ; ... rest of code
}
```

**Error**: `error: stored value and pointer type do not match`

### After Fix

**Expression**: `((λx.x + 8) (12 - 5))`

**Generated LLVM** (valid):
```llvm
define i64 @f8(i64 %env12, i64 %x7) {
entry:
  %r9 = add i64 %x7, 8
  ret i64 %r9
}

define i64 @main() {
entry:
  %f8_ptr = alloca { i64 }
  %f8_tmp0 = ptrtoint i64 (i64, i64)* @f8 to i64    ; GOOD: Convert function pointer
  %f8_gep0 = getelementptr { i64 }, { i64 }* %f8_ptr, i32 0, i32 0
  store i64 %f8_tmp0, i64* %f8_gep0                 ; GOOD: Store i64 value
  %f8 = ptrtoint { i64 }* %f8_ptr to i64
  %r10 = sub i64 12, 5
  %f8_ptr_f13 = inttoptr i64 %f8 to { i64, i64, i64 }*
  %f13_gep = getelementptr { i64, i64, i64 }, { i64, i64, i64 }* %f8_ptr_f13, i32 0, i32 0
  %f13 = load i64, i64* %f13_gep
  %f13_fptr = inttoptr i64 %f13 to i64 (i64, i64)*  ; GOOD: Convert back to function pointer
  %r11 = call i64 %f13_fptr(i64 %f8, i64 %r10)      ; GOOD: Indirect call
  ret i64 %r11
}
```

**Compilation**: Successfully compiles and executes, returning `15` (correct result: 7 + 8)

## Technical Details

### LLVM Type System

LLVM has strict type requirements:

1. **Function types**: Functions have pointer types like `i64 (i64, i64)*`
2. **Integer types**: Raw integers have types like `i64`
3. **No implicit conversions**: Must use explicit `ptrtoint` and `inttoptr` casts

### Closure Representation

Closures are represented as tuples where:
- **Position 0**: Function pointer (code)
- **Positions 1+**: Captured environment variables

The tuple structure `{ i64, i64, i64 }` stores everything as `i64`, so function pointers must be converted to integers.

### Conversion Cycle

```
Function Definition (@fname)
      ↓ ptrtoint
   i64 value (stored in tuple)
      ↓ load from tuple
   i64 value (function as integer)
      ↓ inttoptr
Function Pointer (i64 (i64, i64)*)
      ↓ indirect call
   Function Execution
```

## Key Insights

1. **Type Safety**: LLVM enforces strict type matching. Function pointers and integers are distinct types that require explicit conversion.

2. **Closure Implementation**: First-class functions require treating function pointers as data, which means converting them to integers for storage.

3. **Pattern Matching**: The fix uses ReScript's pattern matching to handle the `AtomGlob` case specially, demonstrating good functional programming practice.

4. **Temporary Variables**: Using temporary variables (`%f8_tmp0`) for the converted function pointer keeps the IR clean and readable.

## Related Fixes

This fix works in conjunction with:

1. **Closure Conversion Fix** ([docs/Closure_Conversion_Fix.md](./Closure_Conversion_Fix.md)): Ensures parameters aren't incorrectly captured
2. **Hoisting Implementation**: Extracts functions to top level for LLVM lowering
3. **Indirect Call Support** (Phase 3): Properly handles function calls through closure projections

## Test Results

- **Test**: `((λx.x + 8) (12 - 5))`
- **Expected**: `15`
- **Before Fix**: LLVM compilation error
- **After Fix**: Successfully compiles and executes, returning `15`

## References

- [LLVM Language Reference - ptrtoint](https://llvm.org/docs/LangRef.html#ptrtoint-to-instruction)
- [LLVM Language Reference - inttoptr](https://llvm.org/docs/LangRef.html#inttoptr-to-instruction)
- [`src/Lambda/Compile.res`](../src/Lambda/Compile.res): LLVM lowering implementation
- [`src/Lambda/ClosureConversion.res`](../src/Lambda/ClosureConversion.res): Closure conversion with AtomGlob
- [`src/Lambda/ANF.res`](../src/Lambda/ANF.res): ANF atom types definition
