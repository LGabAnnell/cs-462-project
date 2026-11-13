open Ast

type value = VInt of int | VBool of bool | VString of string | VUnit

type environment = (string * value) list

type error =
  | Unbound_variable of string
  | Invalid_operands of Ast.binop * value * value
  | Non_boolean_condition of value
  | Unsupported_expression of Ast.expression

let lookup environment name =
  match List.assoc_opt name environment with
  | Some value -> Ok value
  | None -> Error (Unbound_variable name)

let eval_binary op left right =
  match (op, left, right) with
  | Add, VInt x, VInt y -> Ok (VInt (x + y))
  | Subtract, VInt x, VInt y -> Ok (VInt (x - y))
  | Multiply, VInt x, VInt y -> Ok (VInt (x * y))
  | Less_than, VInt x, VInt y -> Ok (VBool (x < y))
  | Concat, VString x, VString y -> Ok (VString (x ^ y))
  | Equal, VInt x, VInt y -> Ok (VBool (x = y))
  | Equal, VBool x, VBool y -> Ok (VBool (x = y))
  | Equal, VString x, VString y -> Ok (VBool (x = y))
  | _ -> Error (Invalid_operands (op, left, right))

let rec eval environment expression =
  match expression with
  | Literal (Integer n) -> Ok (VInt n)
  | Literal (Boolean b) -> Ok (VBool b)
  | Literal (String_literal s) -> Ok (VString s)
  | Literal Unit_literal -> Ok VUnit
  | Identifier name -> lookup environment name
  | Let_in (name, _, bound_expression, body) ->
      (match eval environment bound_expression with
       | Error _ as error -> error
       | Ok value -> eval ((name, value) :: environment) body)
  | Binary (op, left, right) ->
      (match eval environment left with
       | Error _ as error -> error
       | Ok left_value ->
           match eval environment right with
           | Error _ as error -> error
           | Ok right_value -> eval_binary op left_value right_value)
  | If (condition, then_branch, else_branch) ->
      (match eval environment condition with
       | Error _ as error -> error
       | Ok (VBool true) -> eval environment then_branch
       | Ok (VBool false) -> eval environment else_branch
       | Ok value -> Error (Non_boolean_condition value))
  | (Lambda _ | Apply _ | List_literal _) as unsupported ->
      Error (Unsupported_expression unsupported)
