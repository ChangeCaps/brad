# BRAD Language

## Project Overview

- `diagnostic` crate provides error handling and reporting.
- `solve` crate contains the type solver for the language.
- `src` crate contains the core language implementation.
    - `src/v1` is the first version of the language (drad).
    - `src/*` contains the latest version of the language (brad).
- `tests` contains the test suite for the language.
- `fuzz` contains fuzzing tests for the language.
- `bench` contains benchmarks for the language.
- `docs` contains documentation for the language.
- `tree-sitter-brad` contains the tree-sitter grammar for the language.
- `lib` contains the standard library for the language.
- `rt` contains the language runtimes.
    - `rt/c` contains the C runtime.
    - `rt/lua` contains the Lua runtime.

## TODO

- Inline comments do not work.
- Fix float/int behavior
- Match kinda useless unless tuple matching works
- Missing map implementation
- Inline match is parsed weird.