# uigen

Run a program from a file:

```sh
dune exec uigen -- program.ui
```

With no file argument, `uigen` reads the complete program from standard input:

```sh
printf 'let x = 3\nrender [x; x + 1]\n' | dune exec uigen --
```

By default, programs without event declarations print the evaluated result,
for example `[3; 4]`. Programs with event declarations print the constructed
reactive graph, including each node's parents, contents, dependents, and dirty
status, followed by the value of `render`. Try:

```sh
dune exec uigen -- tests/programs/reactive_graph.ui
```

Use `--mode auto` (the default), `--mode eval` to use the core evaluator, or
`--mode construct` to explicitly construct a graph, including for programs
without event declarations. All modes accept a file or standard input.
Strings are quoted and escaped, and functions are displayed as `<function>`.
Errors go to standard error and cause a nonzero exit status.

Construction stops before dirty nodes or reactive callbacks are evaluated;
event ticks, type checking, and HTML rendering are not yet
implemented. The `show`, `text`, `button`, `column`, `row`, and `dyn` built-ins construct UI values. Type annotations are ignored by the evaluator.

A `let ... in ...` expression can appear as an application argument. Where
this overlaps with consecutive top-level declarations, the runner first
accepts the existing parser's interpretation of the whole program. If that
fails, it uses the first sequence of declaration boundaries that parses the
entire program.

The `examples` directory contains a counter, a filtered temperature display,
and a snapshot example. Run one with:

```sh
dune exec uigen -- examples/counter.ui
```

These examples print the constructed graph. Event ticks and HTML rendering are
not implemented yet.
