(** Construction of a reactive graph without evaluating its dirty nodes. *)
module Node_set : Set.S with type elt = int

type node_id = int

type node_kind =
  | Source of string * Ast.typ
  | Map of Eval.value * node_id
  | Map2 of Eval.value * node_id * node_id
  | MapE of Eval.value * node_id
  | FilterE of Eval.value * node_id
  | MergeWith of Eval.value * node_id * node_id
  | Fold of Eval.value * node_id
  | Hold of node_id
  | Snapshot of Eval.value * node_id * node_id

type contents = Uninitialized | No_occurrence | Value of Eval.value

type node = {
  id : node_id;
  kind : node_kind;
  contents : contents;
  dependents : Node_set.t;
}

type source = { name : string; payload_type : Ast.typ; id : node_id }
type graph

type construction = { graph : graph; dirty : Node_set.t; value : Eval.value }
type error = Duplicate_source of string | Evaluation_error of Eval.error

(** Nodes and sources are returned in allocation and declaration order. *)
val nodes : graph -> node list
val find_node : graph -> node_id -> node option
val sources : graph -> source list
val find_source : graph -> string -> source option
val run : Ast.program -> (construction, error) result
