# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This repository contains implementations of exercises from "Types and Programming Languages" (TAPL) by Benjamin C. Pierce, written in ReScript. The project includes a lambda calculus compiler that transforms source code through multiple intermediate representations to LLVM IR.

## Build Commands

- **Build everything**: `pnpm run build` (builds both ReScript and CLI)
- **Build ReScript**: `pnpm run res:build` or `rescript`
- **Build CLI**: `pnpm run cli:build`
- **Clean build artifacts**: `pnpm run res:clean`
- **Format code**: `pnpm run res:format`
- **Watch mode for development**: `pnpm run res:dev`
- **Watch CLI development**: `pnpm run cli:dev`
- **Run tests**: `pnpm run test` or `vitest`
- **Run tests in watch mode**: `pnpm run test:watch`
- **Run tests with UI**: `pnpm run test:ui`
- **Run tests once**: `pnpm run test:run`

## Architecture

### Lambda Compiler Pipeline (src/Lambda/)

The lambda calculus compiler implements a multi-stage compilation pipeline:

1. **Parsing** (`Parser.res`) → **AST** (`Ast.res`)
   - Tokenizes and parses lambda calculus syntax: `λx.e`, `(e1 e2)`, `e1 + e2`, `if e1 then e2 else e3`
   - Produces an abstract syntax tree with types: `Int`, `Var`, `Lam`, `App`, `Bop`, `If`

2. **Alpha Renaming** (`Ast.res:rename`)
   - Renames all bound variables to unique names to avoid capture
   - Critical preprocessing step before ANF conversion

3. **ANF Conversion** (`ANF.res:convert`)
   - Converts AST to A-Normal Form, making all intermediate computations explicit
   - Introduces join points for control flow (`Join`/`Jump` constructs)
   - ANF types: `Halt`, `Fun`, `Join`, `Jump`, `App`, `Bop`, `If`, `Tuple`, `Proj`

4. **Closure Conversion** (`ClosureConversion.res:convert`)
   - Computes free variables for each function
   - Transforms functions to capture their environment as tuples
   - Functions receive an extra `env` parameter
   - Function calls project the code pointer from position 0 of closure tuples

5. **Hoisting** (`Compile.res:Hoisting.hoist`)
   - Eliminates join points by inlining jump bodies
   - Extracts all function definitions to top level
   - Produces "straightline code" required for LLVM lowering

6. **LLVM Lowering** (`Compile.res:LLVMLowering`)
   - Phase 1: Basic arithmetic and primitives
   - Phase 2: Function definitions and direct calls
   - Phase 3: Closures with tuples and memory management
   - Phase 4: Control flow (if/then/else with basic blocks)

### Other TAPL Implementations

- `SmallStepArith.res`: Untyped arithmetic expressions (Chapter 4)
- `DeBruijn.res`: Untyped lambda calculus with de Bruijn indices (Chapter 7)
- `Typedlambda.res`: Simply typed lambda calculus (Chapter 10)
- `Subtyping.res`: Typed lambda calculus with subtyping (Chapter 15)

## Testing

Tests are located in `*_test.res` files and use Vitest. Key test files:

- `src/Lambda/Parser_test.res`: Parser tokenization and parsing
- `src/Lambda/Compile_test.res`: Complete compilation pipeline tests for all phases

## Key Implementation Notes

### Compilation Pipeline Order

The complete compilation function (`Compiler.compile`) must execute transformations in this exact order:

```rescript
term->Ast.rename->ANF.convert->ClosureConversion.convert->Hoisting.hoist
```

### Hoisting Implementation

The hoisting pass performs two critical operations:
1. `eliminateJoinPoints`: Removes all `Join`/`Jump` constructs by inlining
2. `extractFunctions`: Moves all `Fun` definitions to top level

This creates straightline code that LLVM Phase 4 can handle. Without proper hoisting, code with join points will fail LLVM lowering with "Code must be straightline!" error.

### Free Variable Computation

Closure conversion relies on accurate free variable computation (`ClosureConversion.compute`). The algorithm must:
- Track bound variables through function parameters and let bindings
- Exclude function names from their own free variable sets
- Handle join point parameters correctly

### ANF Atom Types

ANF uses three atom types for values:
- `AtomInt(int)`: Literal integers
- `AtomVar(varName)`: Local variables (prefixed with `%` in LLVM)
- `AtomGlob(varName)`: Global function references (prefixed with `@` in LLVM)

### CLI Usage

The project includes a Pastel-based CLI for easy compilation:

```bash
# Basic usage
lambda compile "((λx.x + 8) (12 - 5))"

# Specify LLVM phase (1-4)
lambda compile --phase=1 "1 + 2"

# Show intermediate representations
lambda compile --ast "λx.x + 1"  # Show AST
lambda compile --anf "λx.x + 1"  # Show ANF after hoisting
```

The CLI is built with:
- **Pastel**: File-based CLI framework (commands in `cli/commands/`)
- **Ink**: React for terminal UIs
- **TypeScript**: Type-safe CLI code
- **Zod**: Schema validation for options/arguments

The CLI executable is at `dist/cli/cli.js` after building, and is linked via `package.json` `bin` field.
