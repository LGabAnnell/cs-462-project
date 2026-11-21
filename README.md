# A DSL for UI generation

This project aims to define the specifications and show a sample implementation of a user interface (UI) generating language with a reactive runtime.

## Specification 

The specification can be found in [./proposal](./proposal). Note that if you are building with TexStudio, you will need to change the bibliography backend to `Biber`.

## Implementation

The (currently partial) implementation can be found in [uigen](./uigen).

1. The parser and evaluator are implemented as a library under [uigen/src](./uigen/src).
2. Under [uigen/bin](./uigen/bin) there is a caller program that uses the library and can run programs.
3. For a few examples of the language itself, see [uigen/src/tests/programs](./uigen/src/tests/programs).

## Build and run

Build:

```bash
$ cd uigen
$ dune build
```

Run (example with a program):

```bash
cd uigen
$ dune exec uigen -- tests/programs/functions.uigen
```
