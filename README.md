# llang

An interpreter, type checker and LLVM compiler for **L**, a small functional language, written in OCaml for the Language and Compiler Design course (NOVA FCT, 2026/27).

## Requirements

- OCaml and Dune 3.23 or newer
- OUnit2 (`opam install ounit2`), for the unit tests
- LLVM 18's `clang`, to build the LLVM IR the compiler generates

## Build and run

```bash
dune build
```

| Command | Behaviour |
|---|---|
| `llang -i prog.lcd` | type check, then interpret; print the result |
| `llang -i` | interactive prompt: type check and evaluate each line |
| `llang -c prog.lcd` | type check, then compile; write LLVM IR to stdout |
| `llang -c` | same, reading the program from stdin |
| `llang --cc [prog.lcd]` | debug: print the program after closure conversion (hoisted functions and their environments) |

Run it through dune (`dune exec llang -- -i prog.lcd`) or directly as `_build/default/bin/main.exe`. To run a compiled program:

```bash
dune exec llang -- -c prog.lcd > prog.ll && clang -w prog.ll -o prog && ./prog
```

A program prints its value followed by a newline: integers in decimal, `true`/`false`, and `<fun>` for functions. Parse, type and runtime errors are printed to stderr, and the tool exits with status 1.

## Tests

```bash
./run_tests.sh
```

runs every program in `tests/` through the interpreter (`-i`) and through the compiler and `clang` (`-c`), and compares the results. Use `-i` or `-c` to run one mode only, and folder names to run part of the suite (`./run_tests.sh -c closures`); `--help` lists the options.

| Folder | Contents |
|---|---|
| `tests/core/` | the core language without functions; `<name>.out` is the expected output |
| `tests/closures/` | first-class functions and closures, including the examples from the assignment |
| `tests/errors/` | programs that must fail; `<name>.err` is the expected start of the error message (e.g. `Type error`) |

The OUnit unit tests (lexer, parser, interpreter) run with:

```bash
dune runtest
```

## Project structure

```text
bin/main.ml           command line: -i, -c, the REPL, error reporting
lib/
  lexer.mll           lexer
  parser.mly          parser (ocamlyacc)
  parser_utils.ml     parse_string and the unparser
  ast.ml              untyped AST
  ast_typed.ml        typed AST
  typing.ml           type checker (synthesises the type of each node)
  interpreter.ml      environment-based interpreter; functions evaluate to closures
  environment.ml      scoped environments
  symbols.ml          identifier interning
  closure_ir.ml       the program after closure conversion: hoisted functions and their environments
  closure_conv.ml     free-variable analysis and closure conversion
  compiler_llvm.ml    compiles the converted program to basic blocks of LLVM instructions
  llvm_utils.ml       prints the LLVM IR module, including the result printing
test/test_llang.ml    OUnit unit tests
tests/                end-to-end test programs (see above)
run_tests.sh          end-to-end test runner
```

## Status

| Part | Status |
|---|---|
| Integers, booleans, arithmetic (`+ - * / %`, unary `-`), comparisons, `not` | interpreter and compiler |
| Short-circuit `&&` and `\|\|` | interpreter and compiler |
| `let`, `if` | interpreter and compiler |
| `fun` and application (closures) | interpreter and compiler (closure conversion, then heap-allocated closures) |
| Division by zero in compiled code | not yet detected (the interpreter reports it) |

A Rust version of the starter code is kept on the `rust` branch.
