(** Values and errors produced by the basic expression evaluator. *)
type value =
  | VInt of int | VBool of bool | VString of string | VUnit
  | VClosure of string * Ast.expression * environment
  | VList of value list
and environment = (string * value) list

type error =
  | Unbound_variable of string
  | Invalid_operands of Ast.binop * value * value
  | Non_boolean_condition of value
  | Non_function_application of value

(** Evaluate an expression in an environment. Type annotations are ignored. *)
val eval : environment -> Ast.expression -> (value, error) result
