# Hoisting Implementation Plan

## 📋 Problem Analysis

Based on the code analysis, the current hoisting implementation has critical flaws that prevent proper straightline code generation:

### Current Issues

1. **Join Points Not Eliminated** (lines 310-314 in LambdaCompile.res)
   - `Join` constructs are preserved instead of being inlined
   - Jump targets are not substituted with their bodies

2. **If Statements Not Flattened** (lines 315-319)
   - `If` constructs remain in the output
   - Should be converted to straightline code with direct jumps

3. **Functions Not Fully Extracted** (lines 285-290)
   - Functions are collected but not completely removed from main flow
   - LLVM lowering still encounters `Fun` constructs

4. **LLVM Lowering Expects Straightline Code**
   - Phase 4 has `Join(_) => failwith("Code must be straightline!")` (line 809)
   - But hoisting doesn't produce straightline code

## 🎯 Theoretical Requirements

According to [compiler.club](https://compiler.club/compiling-lambda-calculus/), proper hoisting should:

1. **Extract all function definitions** to top-level
2. **Eliminate join points** by inlining bodies at jump sites
3. **Convert control flow** to straightline code
4. **Result in only**: `Halt`, `App`, `Bop`, `Tuple`, `Proj` constructs

## 🏗️ Implementation Strategy

### Phase 1: Join Point Elimination

**Algorithm**: Substitute jump targets with their bodies

```rescript
// Before:
Join(j, Some(p), body,
  If(cond, Jump(j, Some(arg1)), Jump(j, Some(arg2))))

// After:
If(cond,
  substitute(body, p, arg1),
  substitute(body, p, arg2))
```

**Key Steps**:
1. Collect all join point definitions
2. Find all jump sites that target each join point
3. Substitute jump with inlined join body
4. Remove join point definitions

### Phase 2: If Statement Flattening

**Algorithm**: Convert If to direct branching

```rescript
// Before:
If(cond, thenBranch, elseBranch)

// After: (conceptually - actual implementation uses continuation)
let result = if cond then thenBranch else elseBranch
```

**Key Steps**:
1. Identify all If constructs
2. Flatten nested If statements
3. Convert to straightline code with explicit control flow

### Phase 3: Function Extraction

**Algorithm**: Move all functions to top-level

```rescript
// Before:
Fun(f, params, body, continuation)

// After:
// Top-level: Fun(f, params, hoisted_body)
// Main flow: continuation (with f available)
```

**Key Steps**:
1. Collect all function definitions recursively
2. Hoist function bodies recursively
3. Remove functions from main control flow
4. Reconstruct with functions at top-level

## 🔧 Detailed Implementation Plan

### Step 1: Enhanced Data Structures

```rescript
module Hoisting = {
  type join_point = {
    name: varName,
    param: option<varName>,
    body: ANF.t,
  }

  type jump_site = {
    target: varName,
    arg: option<ANF.atom>,
    context: ANF.t => ANF.t, // Continuation context
  }

  type hoisting_context = {
    functions: list<(varName, list<varName>, ANF.t)>,
    join_points: Belt.Map.String.t<join_point>,
    jump_sites: list<jump_site>,
  }
}
```

### Step 2: Join Point Collection and Elimination

```rescript
let collectJoinPoints = (t: ANF.t): Belt.Map.String.t<join_point> => {
  let rec go = (term: ANF.t, acc: ref<Belt.Map.String.t<join_point>>) => {
    switch term {
    | Join(j, p, body, cont) => {
        let join_point = {name: j, param: p, body: body}
        acc := Belt.Map.String.set(acc.contents, j, join_point)
        go(cont, acc)
      }
    | _ => traverseAndCollect(term, acc)
    }
  }
  // Implementation details...
}

let eliminateJoinPoints = (t: ANF.t, joinPoints: Belt.Map.String.t<join_point>): ANF.t => {
  let rec substitute = (term: ANF.t): ANF.t => {
    switch term {
    | Jump(j, arg) => {
        switch Belt.Map.String.get(joinPoints, j) {
        | Some({body, param: Some(p), _}) =>
            // Substitute parameter with argument in body
            substituteVar(body, p, arg)
        | Some({body, param: None, _}) => body
        | None => term // Should not happen in well-formed code
        }
      }
    | Join(j, _, _, cont) => substitute(cont) // Remove join point
    | _ => mapOverSubterms(term, substitute)
    }
  }
  substitute(t)
}
```

### Step 3: If Statement Flattening

```rescript
let flattenIfStatements = (t: ANF.t): ANF.t => {
  let rec flatten = (term: ANF.t): ANF.t => {
    switch term {
    | If(cond, thenBranch, elseBranch) => {
        // Convert to straightline code using a fresh variable
        let result = Lam.fresh("if_result")
        let flatThen = flatten(thenBranch)
        let flatElse = flatten(elseBranch)

        // Create straightline equivalent
        createStraightlineIf(cond, flatThen, flatElse, result)
      }
    | _ => mapOverSubterms(term, flatten)
    }
  }
  flatten(t)
}
```

### Step 4: Complete Function Extraction

```rescript
let extractFunctions = (t: ANF.t): (list<(varName, list<varName>, ANF.t)>, ANF.t) => {
  let functions = ref(list{})

  let rec extract = (term: ANF.t): ANF.t => {
    switch term {
    | Fun(f, params, body, cont) => {
        let hoistedBody = extract(body)
        functions := list{(f, params, hoistedBody), ...functions.contents}
        extract(cont) // Continue with the rest, function is removed
      }
    | _ => mapOverSubterms(term, extract)
    }
  }

  let mainFlow = extract(t)
  (functions.contents, mainFlow)
}
```

### Step 5: Straightline Code Generation

```rescript
let generateStraightlineCode = (t: ANF.t): ANF.t => {
  t
  |> collectJoinPoints
  |> eliminateJoinPoints
  |> flattenIfStatements
  |> extractFunctions
  |> reconstructWithFunctionsAtTop
}
```

## 🧪 Testing Strategy

### Test Cases for Hoisting Correctness

1. **Join Point Elimination Test**
```rescript
// Input: Join with multiple jump sites
Join("j", Some("p"), Halt(AtomVar("p")),
  If(AtomInt(1), Jump("j", Some(AtomInt(10))), Jump("j", Some(AtomInt(20)))))

// Expected Output: Straightline If
If(AtomInt(1), Halt(AtomInt(10)), Halt(AtomInt(20)))
```

2. **Function Extraction Test**
```rescript
// Input: Nested functions
Fun("outer", list{"x"},
  Fun("inner", list{"y"}, Halt(AtomVar("y")), Halt(AtomVar("inner"))),
  App("result", "outer", list{AtomInt(5)}, Halt(AtomVar("result"))))

// Expected Output: Functions at top-level, straightline main
```

3. **Complete Pipeline Test**
```rescript
// Input: Complex expression with all constructs
// Expected: Only Halt, App, Bop, Tuple, Proj in output
```

### Validation Criteria

1. **No Control Flow Constructs**: Output should contain no `Fun`, `Join`, `Jump`, or `If`
2. **Functions at Top-Level**: All functions extracted to separate definitions
3. **Straightline Main Flow**: Main computation is linear sequence of operations
4. **LLVM Compatibility**: Output should not trigger "Code must be straightline!" error

## 📅 Implementation Roadmap

### Week 1: Foundation
- [ ] Implement join point collection
- [ ] Implement basic join point elimination
- [ ] Add comprehensive test cases

### Week 2: If Flattening
- [ ] Design If statement flattening algorithm
- [ ] Implement straightline code generation
- [ ] Test with nested If statements

### Week 3: Function Extraction
- [ ] Implement complete function extraction
- [ ] Handle recursive functions correctly
- [ ] Test with complex function hierarchies

### Week 4: Integration & Testing
- [ ] Integrate all phases
- [ ] Comprehensive testing with existing test suite
- [ ] Verify LLVM lowering compatibility
- [ ] Performance optimization

## 🎯 Success Metrics

1. **Functional Correctness**: All existing tests pass
2. **Straightline Property**: No control flow constructs in hoisted output
3. **LLVM Compatibility**: All LLVM lowering phases work without "unsupported construct" errors
4. **Performance**: Hoisting completes in reasonable time for complex expressions

## 🔄 Iterative Development

1. **Start with simplest case**: Single join point elimination
2. **Add complexity gradually**: Multiple join points, nested structures
3. **Test at each step**: Ensure correctness before adding features
4. **Refactor as needed**: Optimize for clarity and performance

This plan provides a systematic approach to fixing the hoisting implementation and achieving proper straightline code generation as required by the theoretical foundation.
