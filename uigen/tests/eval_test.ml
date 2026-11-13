open Uigen
open Ast
open Eval

let int n = Literal (Integer n)
let bool b = Literal (Boolean b)
let str s = Literal (String_literal s)
let unit = Literal Unit_literal
let id name = Identifier name
let bin op x y = Binary (op, x, y)

let check name env expression expected =
  if eval env expression <> expected then failwith ("Failed: " ^ name)

let () =
  List.iter (fun (name, expression, expected) -> check name [] expression (Ok expected))
    [ "integer", int 42, VInt 42;
      "true", bool true, VBool true;
      "false", bool false, VBool false;
      "string", str "hello", VString "hello";
      "unit", unit, VUnit;
      "add", bin Add (int 2) (int 3), VInt 5;
      "subtract", bin Subtract (int 2) (int 3), VInt (-1);
      "multiply", bin Multiply (int 2) (int 3), VInt 6;
      "less true", bin Less_than (int 2) (int 3), VBool true;
      "less false", bin Less_than (int 3) (int 2), VBool false;
      "concat", bin Concat (str "a") (str "b"), VString "ab";
      "int equal", bin Equal (int 3) (int 3), VBool true;
      "int unequal", bin Equal (int 3) (int 2), VBool false;
      "bool equal", bin Equal (bool true) (bool true), VBool true;
      "bool unequal", bin Equal (bool true) (bool false), VBool false;
      "string equal", bin Equal (str "a") (str "a"), VBool true;
      "string unequal", bin Equal (str "a") (str "b"), VBool false ];
  check "invalid add" [] (bin Add (int 1) (bool true))
    (Error (Invalid_operands (Add, VInt 1, VBool true)));
  check "invalid equality" [] (bin Equal unit unit)
    (Error (Invalid_operands (Equal, VUnit, VUnit)));
  check "mixed equality" [] (bin Equal (int 1) (bool true))
    (Error (Invalid_operands (Equal, VInt 1, VBool true)));
  check "lookup first" ["x", VInt 2; "x", VInt 3] (id "x") (Ok (VInt 2));
  check "unbound" [] (id "missing") (Error (Unbound_variable "missing"));
  check "shadowing" ["x", VInt 3] (Let_in ("x", None, int 9, id "x")) (Ok (VInt 9));
  check "initializer scope" ["x", VInt 3]
    (Let_in ("x", Some Bool, bin Add (id "x") (int 1), id "x")) (Ok (VInt 4));
  check "nonrecursive initializer" [] (Let_in ("x", None, id "x", int 1))
    (Error (Unbound_variable "x"));
  check "true branch only" [] (If (bool true, int 1, id "missing")) (Ok (VInt 1));
  check "false branch only" [] (If (bool false, id "missing", int 2)) (Ok (VInt 2));
  check "nonboolean condition" [] (If (int 1, int 2, int 3))
    (Error (Non_boolean_condition (VInt 1)));
  check "condition error" [] (If (id "missing", int 2, int 3))
    (Error (Unbound_variable "missing"));
  check "left error first" [] (bin Add (id "left") (id "right"))
    (Error (Unbound_variable "left"));
  check "right error" [] (bin Add (int 1) (id "right"))
    (Error (Unbound_variable "right"));
  List.iter (fun expression -> check "unsupported" [] expression (Error (Unsupported_expression expression)))
    [Lambda ("x", Int, id "x"); Apply (id "missing", id "alsoMissing"); List_literal [id "missing"]];
  let program = Parser.program Lexer.token
      (Lexing.from_string "render let x = 3 in if x < 5 then x * 2 else 0") in
  check "parsed program" [] program.render (Ok (VInt 6))
