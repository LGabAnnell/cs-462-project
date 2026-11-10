(** Abstract syntax for uigen programs. *)
type typ =
  | Int
  | Bool
  | String
  | UI
  | Unit
  | List of typ
  | Behaviour of typ
  | Event of typ
  | Arrow of typ * typ

type literal = Integer of int | Boolean of bool | String_literal of string | Unit_literal

type binop = Add | Subtract | Multiply | Equal | Less_than | Concat

type expression =
  | Literal of literal
  | Identifier of string
  | Lambda of string * typ * expression
  | Apply of expression * expression
  | Let_in of string * typ option * expression * expression
  | If of expression * expression * expression
  | Binary of binop * expression * expression
  | List_literal of expression list

type declaration =
  | Let of string * typ option * expression
  | Event_declaration of string * typ

type program = { declarations : declaration list; render : expression }
