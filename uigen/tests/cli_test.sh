#!/usr/bin/env bash
set -eu

exe=$1
tmpdir=$(mktemp -d)
trap 'rm -rf "$tmpdir"' EXIT

fail() { printf 'CLI test failed: %s\n' "$1" >&2; exit 1; }
expect_output() {
  expected=$1
  if ! "$exe" >"$tmpdir/out" 2>"$tmpdir/err"; then fail "unexpected failure: $(cat "$tmpdir/err")"; fi
  [ "$(cat "$tmpdir/out")" = "$expected" ] || fail "expected $expected, got $(cat "$tmpdir/out")"
  [ ! -s "$tmpdir/err" ] || fail "unexpected stderr"
}
expect_error() {
  expected=$1
  shift
  if "$exe" "$@" >"$tmpdir/out" 2>"$tmpdir/err"; then fail "expected failure: $expected"; fi
  [ ! -s "$tmpdir/out" ] || fail "unexpected stdout on failure"
  grep -Fq "$expected" "$tmpdir/err" || fail "missing error '$expected': $(cat "$tmpdir/err")"
}

printf 'let x = 3\nlet f = fn (y : Int) -> x + y\nlet x = 99\nrender [f 1; f 2; x]\n' >"$tmpdir/program.ui"
"$exe" "$tmpdir/program.ui" >"$tmpdir/out" 2>"$tmpdir/err" || fail 'file execution'
[ "$(cat "$tmpdir/out")" = '[4; 5; 99]' ] || fail 'file output'
[ ! -s "$tmpdir/err" ] || fail 'file stderr'
printf 'render [true; (); "a\\n\\\"b"; fn (x : Int) -> x]\n' | expect_output '[true; (); "a\n\"b"; <function>]'
printf 'render let f = fn (x : Int) -> x in f let y = 3 in y\n' | expect_output '3'
printf 'let x = 3\nlet y = 4\nrender @\n' | expect_error '<stdin>:3:8: lexical error:'
printf 'render 1\n@\n' | expect_error '<stdin>:2:1: lexical error:'
printf 'render 1 +\n' | expect_error '<stdin>:2:1: syntax error'
printf 'render missing\n' | expect_error 'unbound variable: missing'
printf 'render 1 true\n' | expect_error 'cannot apply non-function value: 1'
printf 'render 1 + true\n' | expect_error 'invalid operands for +: 1 and true'
printf 'render if 1 then 2 else 3\n' | expect_error 'condition is not a boolean: 1'
printf 'event click : Unit\nrender 1\n' | expect_error 'reactive execution is not implemented'
expect_error 'Usage: uigen [FILE]' one two
expect_error "$tmpdir/missing.ui" "$tmpdir/missing.ui"

expect_program() {
  file=$1
  expected=$2
  if ! "$exe" "$file" >"$tmpdir/out" 2>"$tmpdir/err"; then fail "$file: $(cat "$tmpdir/err")"; fi
  [ "$(cat "$tmpdir/out")" = "$expected" ] || fail "$file: expected $expected, got $(cat "$tmpdir/out")"
  [ ! -s "$tmpdir/err" ] || fail "$file: unexpected stderr"
}
expect_program programs/arithmetic.ui '[31; 18; false]'
expect_program programs/closures.ui '[11; 15; 100]'
expect_program programs/conditionals.ui '["pass"; true; "safe"]'
expect_program programs/functions.ui '[7; 11; 16]'
expect_program programs/strings.ui '["Hello, world!"; "line\nnext"; (); [true; false]]'
