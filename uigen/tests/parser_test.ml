open Uigen
open Ast

let id name = Identifier name
let binary op left right = Binary (op, left, right)
let apply left right = Apply (left, right)

let cases =
  [ ( "f x * y + z ^ a == b < c",
      binary Less_than
        (binary Equal
           (binary Concat
              (binary Add (binary Multiply (apply (id "f") (id "x")) (id "y"))
                 (id "z"))
              (id "a"))
           (id "b"))
        (id "c") );
    ( "a - b - c * d",
      binary Subtract (binary Subtract (id "a") (id "b"))
        (binary Multiply (id "c") (id "d")) );
    ( "a ^ b ^ c",
      binary Concat (id "a") (binary Concat (id "b") (id "c")) );
    ( "f x y", apply (apply (id "f") (id "x")) (id "y") );
    ( "fn (x : Int) -> x + y * z",
      Lambda ("x", Int, binary Add (id "x") (binary Multiply (id "y") (id "z"))) );
    ( "f fn (x : Int) -> x y + z",
      apply (id "f")
        (Lambda ("x", Int, binary Add (apply (id "x") (id "y")) (id "z"))) );
    ( "let x = f y in x + z * w",
      Let_in ("x", None, apply (id "f") (id "y"),
              binary Add (id "x") (binary Multiply (id "z") (id "w"))) );
    ( "if a then if b then c else d else e + f",
      If (id "a", If (id "b", id "c", id "d"), binary Add (id "e") (id "f")) );
    ( "if a then b else let x = c in x y + z",
      If (id "a", id "b",
          Let_in ("x", None, id "c", binary Add (apply (id "x") (id "y")) (id "z"))) );
    ( "[fn (x : Int) -> x + y; f z]",
      List_literal
        [Lambda ("x", Int, binary Add (id "x") (id "y")); apply (id "f") (id "z")] ) ]

let () =
  List.iter
    (fun (source, expected) ->
      let program = Parser.program Lexer.token (Lexing.from_string ("render " ^ source)) in
      if program.render <> expected then failwith ("Unexpected parse: " ^ source))
    cases
