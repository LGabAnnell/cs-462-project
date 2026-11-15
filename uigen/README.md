# uigen

Run a program from a file:

```sh
dune exec uigen -- program.ui
```

With no file argument, `uigen` reads the complete program from standard input:

```sh
printf 'let x = 3\nrender [x; x + 1]\n' | dune exec uigen --
```

The command prints the evaluated result, for example `[3; 4]`. Strings are
quoted and escaped, and functions are displayed as `<function>`. Errors are
written to standard error and cause a nonzero exit status.

The runner supports the current core expression evaluator and sequential `let`
declarations. Type annotations are ignored. Event declarations, reactive
execution, built-ins, type checking, and HTML rendering are not implemented yet.

A `let ... in ...` expression can appear as an application argument. Where
this overlaps with consecutive top-level declarations, the runner first
accepts the existing parser's interpretation of the whole program. If that
fails, it uses the first sequence of declaration boundaries that parses the
entire program.
