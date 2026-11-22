#!/usr/bin/env bash
set -eu

exe=$(realpath "$1")
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
printf 'render 42\n' >"$tmpdir/-demo.ui"
(cd "$tmpdir" && "$exe" -- -demo.ui >out 2>err) || fail 'dash-prefixed file'
[ "$(cat "$tmpdir/out")" = '42' ] || fail 'dash-prefixed file output'
(cd "$tmpdir" && "$exe" --mode eval -- -demo.ui >out 2>err) || fail 'dash-prefixed file with mode'
[ "$(cat "$tmpdir/out")" = '42' ] || fail 'dash-prefixed file with mode output'
printf 'render [true; (); "a\\n\\\"b"; fn (x : Int) -> x]\n' | expect_output '[true; (); "a\n\"b"; <function>]'
printf 'render let f = fn (x : Int) -> x in f let y = 3 in y\n' | expect_output '3'
printf 'render let s = show 42 in column [text "Hi"; text s]\n' | expect_output 'column [text "Hi"; text "42"]'
printf 'render text "Hi"\n' | expect_error 'unbound variable: text' --mode eval
printf 'let x = 3\nlet y = 4\nrender @\n' | expect_error '<stdin>:3:8: lexical error:'
printf 'render 1\n@\n' | expect_error '<stdin>:2:1: lexical error:'
printf 'render 1 +\n' | expect_error '<stdin>:2:1: syntax error'
printf 'render missing\n' | expect_error 'unbound variable: missing'
printf 'render 1 true\n' | expect_error 'cannot apply non-function value: 1'
printf 'render 1 + true\n' | expect_error 'invalid operands for +: 1 and true'
printf 'render if 1 then 2 else 3\n' | expect_error 'condition is not a boolean: 1'
printf 'event click : Unit\nrender 1\n' | expect_error 'reactive execution is not implemented' --mode eval
printf 'event e : Unit\nevent e : Int\nrender 1\n' | expect_error 'duplicate event source: e' --mode construct
printf 'event e : Unit\nrender map 1 e\n' | expect_error 'reactive callback is not callable' --mode construct
printf 'event e : Unit\nrender let f = fn (x : Int) -> x in map f e\n' | expect_error 'invalid reactive parent' --mode construct
expect_error 'Usage: uigen [--mode auto|eval|construct] [FILE]' one two
expect_error 'Usage: uigen [--mode auto|eval|construct] [FILE]' --mode invalid
expect_error 'Usage: uigen [--mode auto|eval|construct] [FILE]' --mode
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

expected_graph='Constructed graph (reactive nodes have not been evaluated):
node 0: source click : Unit | parents: [] | contents: no occurrence | dependents: [1] | dirty: no
node 1: mapE | parents: [0] | contents: no occurrence | dependents: [2] | dirty: no
node 2: hold | parents: [1] | contents: 0 | dependents: [3] | dirty: no
node 3: map | parents: [2] | contents: uninitialized | dependents: [] | dirty: yes
render: column [button "Click" <node 0>; dyn <node 3>]'
expect_program programs/reactive_graph.ui "$expected_graph"
"$exe" --mode construct programs/reactive_graph.ui >"$tmpdir/out" 2>"$tmpdir/err" || fail 'explicit construction mode'
[ "$(cat "$tmpdir/out")" = "$expected_graph" ] || fail 'explicit construction output'
"$exe" --mode auto < programs/reactive_graph.ui >"$tmpdir/out" 2>"$tmpdir/err" || fail 'reactive stdin'
[ "$(cat "$tmpdir/out")" = "$expected_graph" ] || fail 'reactive stdin output'
printf 'render 42\n' | "$exe" --mode construct >"$tmpdir/out" 2>"$tmpdir/err" || fail 'pure construction'
[ "$(cat "$tmpdir/out")" = 'Constructed graph (reactive nodes have not been evaluated):
render: 42' ] || fail 'pure construction output'
printf 'render 42\n' | "$exe" --mode eval >"$tmpdir/out" 2>"$tmpdir/err" || fail 'explicit evaluation'
[ "$(cat "$tmpdir/out")" = '42' ] || fail 'explicit evaluation output'
printf 'event e : Unit\nrender let b = hold 0 e in let f = fn (x : Int) -> x in map2 f b b\n' | "$exe" >"$tmpdir/out" 2>"$tmpdir/err" || fail 'repeated parents'
grep -Fq 'node 1: hold | parents: [0] | contents: 0 | dependents: [2] | dirty: no' "$tmpdir/out" || fail 'unique dependent'
grep -Fq 'node 2: map2 | parents: [1; 1] | contents: uninitialized | dependents: [] | dirty: yes' "$tmpdir/out" || fail 'ordered repeated parents'
