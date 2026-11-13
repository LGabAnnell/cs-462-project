(** Values and errors produced by the basic expression evaluator. *)
type value = VInt of int | VBool of bool | VString of string | VUnit

type environment = (string * value) list

type error =
  | Unbound_variable of string
  | Invalid_operands of Ast.binop * value * value
  | Non_boolean_condition of value
  | Unsupported_expression of Ast.expression

(** Evaluate an expression in an environment. Let annotations are ignored. *)
val eval : environment -> Ast.expression -> (value, error) result
