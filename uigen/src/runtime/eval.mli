(** Values and errors produced by the basic expression evaluator. *)
type builtin = Show | Text | Button | Column | Row | Dyn | Map | Map2 | MapE | FilterE | MergeWith | Fold | Hold | Snapshot

type parent_kind = Event_parent | Behaviour_parent

type value =
  | VInt of int | VBool of bool | VString of string | VUnit
  | VClosure of string * Ast.expression * environment
  | VList of value list
  | VUI of ui
  | VNode of int
  | VBuiltin of builtin * value list
and ui = UIText of string | UIButton of string * int | UIColumn of ui list | UIRow of ui list | UIDyn of int
and environment = (string * value) list

type error =
  | Unbound_variable of string
  | Invalid_operands of Ast.binop * value * value
  | Non_boolean_condition of value
  | Non_function_application of value
  | Invalid_builtin_arguments of builtin * value list
  | Invalid_callback of value
  | Invalid_parent of parent_kind * value

(** Evaluate an expression in an environment. Type annotations are ignored. *)
val eval : environment -> Ast.expression -> (value, error) result

(** Evaluate with a handler for saturated reactive built-ins. *)
val eval_with_builtins :
  (builtin -> value list -> (value, error) result) ->
  environment -> Ast.expression -> (value, error) result
