# llang

`llang` is an educational compiler and interpreter project developed for the Language and Compiler Design course at NOVA FCT. It explores the main stages of a language implementation, from lexing and parsing to type checking, interpretation, and LLVM code generation.

The OCaml implementation is the active version. A parallel Rust implementation is included as work in progress.

## Current OCaml features

- Integer and boolean literals
- Arithmetic operators: `+`, `-`, `*`, `/`, and `%`
- Integer comparisons and boolean equality
- Unary negation and logical negation
- Short-circuit `&&` and `||`
- Operator precedence and parentheses
- Static type synthesis using a typed AST
- Direct AST interpretation
- LLVM text generation
- LLVM bitcode generation through the OCaml LLVM API
- Basic blocks, conditional branches, and phi nodes for short-circuit control flow

## Project structure

```text
llang/
└── L0/
    ├── ocaml/   Active OCaml lexer, parser, interpreter, type checker, and compiler
    └── rust/    Rust implementation in progress
```

The main OCaml modules are under `llang/L0/ocaml/lib`:

- `ast.ml` and `ast_typed.ml`: untyped and typed syntax trees
- `lexer.mll` and `parser.mly`: source-language frontend
- `typing.ml`: type synthesis
- `interpreter.ml`: direct evaluation
- `compiler_llvm.ml`: internal LLVM-like instruction and basic-block generation
- `llvm_utils.ml`: textual LLVM emission
- `llvm_api_utils.ml`: LLVM module and bitcode construction

## Requirements

The OCaml version currently expects:

- OCaml
- Dune 3.23 or newer
- LLVM 18 and its OCaml bindings
- OUnit2

An opam environment is recommended for installing the OCaml dependencies.

## Build and run

From the OCaml project directory:

```bash
cd llang/L0/ocaml
dune build
dune exec llang
```

The REPL displays the parsed expression, interpreter result, inferred type, and generated LLVM. It also writes textual LLVM to `output.ll` and bitcode to `output.bc`.

For example:

```text
> false && (1 / 0 = 0)
```

The interpreter returns `false` without evaluating the division because logical AND uses short-circuit semantics.

## Tests

Run the OUnit suite with:

```bash
dune runtest
```

The course scaffold currently contains tests for later language features such as `if` and `let`, which are not yet implemented in the L0 frontend. Those tests are expected to remain pending while the language is developed incrementally.

## Status

This repository is a learning project and evolves alongside the course material. The current focus is the OCaml implementation; the Rust version will be revisited after the OCaml pipeline is complete.
