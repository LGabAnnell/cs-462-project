open Ast

type builtin = Show | Text | Button | Column | Row | Dyn | Map | Map2 | MapE | FilterE | MergeWith | Fold | Hold | Snapshot

type parent_kind = Event_parent | Behaviour_parent

type value =
  | VInt of int | VBool of bool | VString of string | VUnit
  | VClosure of string * expression * environment
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

let builtin_arity = function
  | Show | Text | Column | Row | Dyn -> 1
  | Button | Map | MapE | FilterE | Hold -> 2
  | Map2 | MergeWith | Fold | Snapshot -> 3

let eval_with_builtins apply_builtin environment expression =
  let rec eval environment expression =
  match expression with
  | Literal (Integer n) -> Ok (VInt n)
  | Literal (Boolean b) -> Ok (VBool b)
  | Literal (String_literal s) -> Ok (VString s)
  | Literal Unit_literal -> Ok VUnit
  | Identifier name -> lookup environment name
  | Lambda (parameter, _, body) -> Ok (VClosure (parameter, body, environment))
  | Apply (fn, argument) ->
      (match eval environment fn with
       | Error _ as error -> error
       | Ok fn_value ->
           match eval environment argument with
           | Error _ as error -> error
           | Ok argument_value ->
               match fn_value with
               | VClosure (parameter, body, closure_environment) ->
                   eval ((parameter, argument_value) :: closure_environment) body
               | VBuiltin (builtin, args) ->
                   let args = args @ [argument_value] in
                   if List.length args < builtin_arity builtin then
                     Ok (VBuiltin (builtin, args))
                   else apply_builtin builtin args
               | _ -> Error (Non_function_application fn_value))
  | List_literal expressions ->
      let rec eval_elements values = function
        | [] -> Ok (VList (List.rev values))
        | expression :: rest ->
            match eval environment expression with
            | Error _ as error -> error
            | Ok value -> eval_elements (value :: values) rest
      in
      eval_elements [] expressions
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

  in eval environment expression

let eval environment expression =
  eval_with_builtins
    (fun builtin args -> Error (Non_function_application (VBuiltin (builtin, args))))
    environment expression
