open Uigen
open Ast
open Eval

let int n = Literal (Integer n)
let bool b = Literal (Boolean b)
let str s = Literal (String_literal s)
let unit = Literal Unit_literal
let id name = Identifier name
let bin op x y = Binary (op, x, y)
let lambda name body = Lambda (name, Int, body)
let apply fn arg = Apply (fn, arg)
let let_in name value body = Let_in (name, None, value, body)

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
  check "closure captures environment" ["x", VInt 3] (lambda "y" (id "x"))
    (Ok (VClosure ("y", id "x", ["x", VInt 3])));
  check "lexical scope" []
    (let_in "x" (int 3)
       (let_in "f" (lambda "y" (bin Add (id "x") (id "y")))
          (let_in "x" (int 100) (apply (id "f") (int 4)))))
    (Ok (VInt 7));
  check "parameter shadows captured binding" ["x", VInt 3]
    (apply (lambda "x" (id "x")) (int 9)) (Ok (VInt 9));
  check "curried application" []
    (apply (apply (lambda "x" (lambda "y" (bin Add (id "x") (id "y")))) (int 2)) (int 5))
    (Ok (VInt 7));
  check "function evaluated first" [] (apply (id "function") (id "argument"))
    (Error (Unbound_variable "function"));
  check "argument evaluated before non-function error" [] (apply (int 1) (id "argument"))
    (Error (Unbound_variable "argument"));
  check "non-function application" [] (apply (int 1) (int 2))
    (Error (Non_function_application (VInt 1)));
  check "body error" [] (apply (lambda "x" (id "missing")) (int 2))
    (Error (Unbound_variable "missing"));
  check "empty list" [] (List_literal []) (Ok (VList []));
  check "nested lists and order" []
    (List_literal [int 1; List_literal [int 2; int 3]; int 4])
    (Ok (VList [VInt 1; VList [VInt 2; VInt 3]; VInt 4]));
  check "list contains closure" ["x", VInt 3] (List_literal [lambda "y" (id "x")])
    (Ok (VList [VClosure ("y", id "x", ["x", VInt 3])]));
  check "first list error" [] (List_literal [int 1; id "first"; id "second"])
    (Error (Unbound_variable "first"));
  let program = Parser.program Lexer.token
      (Lexing.from_string "render let x = 3 in if x < 5 then x * 2 else 0") in
  check "parsed program" [] program.render (Ok (VInt 6));
  let program = Parser.program Lexer.token
      (Lexing.from_string "render let x = 3 in let f = fn (y : Int) -> x + y in [f 1; f 2]") in
  check "parsed functions and lists" [] program.render (Ok (VList [VInt 4; VInt 5]))
