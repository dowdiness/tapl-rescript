# Lambda Calculus Compiler CLI

A beautiful command-line interface for the Lambda calculus compiler built with [Pastel](https://github.com/vadimdemedes/pastel) and [Ink](https://github.com/vadimdemedes/ink).

## Installation

```bash
# Install dependencies
pnpm install

# Build the CLI
pnpm run build
```

## Usage

```bash
# Show help
lambda

# Compile a lambda expression to LLVM IR (Phase 3 by default)
lambda compile "((λx.x + 8) (12 - 5))"

# Specify LLVM lowering phase (1-4)
lambda compile --phase=1 "1 + 2"
lambda compile --phase=2 "λx.x"
lambda compile --phase=3 "((λx.x + 8) (12 - 5))"
lambda compile --phase=4 "if 1 then 10 else 20"

# Show ANF (A-Normal Form) representation
lambda compile --anf "λx.x + 1"

# Show AST (Abstract Syntax Tree)
lambda compile --ast "if 1 then 2 else 3"
```

## LLVM Lowering Phases

The compiler supports 4 phases of LLVM lowering:

- **Phase 1**: Basic arithmetic operations and primitives (`+`, `-`, integers)
- **Phase 2**: Function definitions and direct calls
- **Phase 3**: Closures with tuples and memory management (full lambda calculus support)
- **Phase 4**: Control flow (if/then/else with basic blocks and branching)

## Examples

### Basic Arithmetic
```bash
lambda compile "1 + 2"
# Output: LLVM IR with simple addition

lambda compile "10 - 5 + 3"
# Output: LLVM IR with chained operations
```

### Lambda Functions
```bash
lambda compile "λx.x + 1"
# Output: Function definition with closure

lambda compile "((λx.x + 8) (12 - 5))"
# Output: Function application with argument
```

### Control Flow
```bash
lambda compile --phase=4 "if 1 then 10 else 20"
# Output: LLVM IR with conditional branching
```

### Intermediate Representations
```bash
# View AST
lambda compile --ast "λx.x + 1"

# View ANF (after closure conversion and hoisting)
lambda compile --anf "λx.x + 1"
```

## Development

```bash
# Build ReScript code
pnpm run res:build

# Build CLI TypeScript code
pnpm run cli:build

# Build both
pnpm run build

# Watch mode for CLI
pnpm run cli:dev
```

## Architecture

The CLI is built using:
- **Pastel**: Next.js-like framework for CLIs with file-based routing
- **Ink**: React for terminal UIs with beautiful rendering
- **Zod**: Type-safe option and argument parsing
- **TypeScript**: Type safety for CLI code

The CLI calls the ReScript-compiled Lambda compiler modules:
- `Parser`: Tokenizes and parses lambda expressions
- `Compiler`: Multi-stage compilation pipeline (AST → ANF → Closure → LLVM)
