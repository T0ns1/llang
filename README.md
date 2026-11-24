# llang (rebuild)

An interpreter, type checker and LLVM compiler for **L**, a small functional language, written in OCaml for the Language and Compiler Design course (NOVA FCT, 2026/27).

This branch rebuilds the project step by step from the course's L0 starter code. The completed core is kept at the tag `checkpoint-core` and on the `main` branch.

## Build and test

```bash
dune build          # build
dune runtest        # OUnit unit tests
./run_tests.sh      # end-to-end tests in tests/ (needs the llang -i / -c command line)
```

Requires OCaml with Dune 3.23 or newer, OUnit2, the LLVM 18 OCaml bindings (`opam install llvm`), and LLVM 18's `clang`.
