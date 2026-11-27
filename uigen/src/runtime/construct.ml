module Node_set = Set.Make (Int)
module Node_map = Map.Make (Int)
module String_set = Set.Make (String)

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
type graph = { nodes_by_id : node Node_map.t; declared_sources : source list }
type construction = { graph : graph; dirty : Node_set.t; value : Eval.value }
type error = Duplicate_source of string | Evaluation_error of Eval.error

let nodes graph = List.map snd (Node_map.bindings graph.nodes_by_id)
let find_node graph id = Node_map.find_opt id graph.nodes_by_id
let sources graph = graph.declared_sources
let find_source graph name = List.find_opt (fun source -> source.name = name) graph.declared_sources

let is_event = function
  | Source _ | MapE _ | FilterE _ | MergeWith _ | Snapshot _ -> true
  | Map _ | Map2 _ | Fold _ | Hold _ -> false

let run program =
  let store = ref Node_map.empty in
  let next_id = ref 0 in
  let dirty = ref Node_set.empty in
  let declared_sources = ref [] in
  let source_names = ref String_set.empty in
  (* Validate all parents before changing the store, then register distinct parents. *)
  let allocate parents make_kind contents initially_dirty =
    let rec validate ids = function
      | [] -> Ok (List.rev ids)
      | (expected, value) :: rest ->
          (match value with
           | Eval.VNode id ->
               (match Node_map.find_opt id !store with
                | Some node when is_event node.kind = (expected = Eval.Event_parent) ->
                    validate (id :: ids) rest
                | _ -> Error (Eval.Invalid_parent (expected, value)))
           | _ -> Error (Eval.Invalid_parent (expected, value)))
    in
    match validate [] parents with
    | Error _ as error -> error
    | Ok ids ->
        let id = !next_id in
        incr next_id;
        let kind = make_kind ids in
        store := Node_map.add id { id; kind; contents; dependents = Node_set.empty } !store;
        List.iter (fun parent ->
          let node = Node_map.find parent !store in
          store := Node_map.add parent
              { node with dependents = Node_set.add id node.dependents } !store)
          (Node_set.elements (List.fold_left (fun set id -> Node_set.add id set) Node_set.empty ids));
        if initially_dirty then dirty := Node_set.add id !dirty;
        Ok (Eval.VNode id)
  in
  let callable = function Eval.VClosure _ | Eval.VBuiltin _ -> true | _ -> false in
  let apply builtin args =
    let event value = (Eval.Event_parent, value) in
    let behaviour value = (Eval.Behaviour_parent, value) in
    let with_callback callback f =
      if callable callback then f () else Error (Eval.Invalid_callback callback)
    in
    let invalid () = Error (Eval.Invalid_builtin_arguments (builtin, args)) in
    let ui_list values =
      let rec collect = function
        | [] -> Some []
        | Eval.VUI ui :: rest -> Option.map (fun uis -> ui :: uis) (collect rest)
        | _ -> None
      in collect values
    in
    match builtin, args with
    | Eval.Show, [Eval.VInt n] -> Ok (Eval.VString (string_of_int n))
    | Eval.Text, [Eval.VString s] -> Ok (Eval.VUI (Eval.UIText s))
    | Eval.Button, [Eval.VString s; Eval.VNode id] ->
        (match Node_map.find_opt id !store with
         | Some { kind = Source (_, Ast.Unit); _ } -> Ok (Eval.VUI (Eval.UIButton (s, id)))
         | _ -> invalid ())
    | (Eval.Column | Eval.Row), [Eval.VList values] ->
        (match ui_list values with
         | Some uis -> Ok (Eval.VUI (if builtin = Eval.Column then Eval.UIColumn uis else Eval.UIRow uis))
         | None -> invalid ())
    | Eval.Dyn, [Eval.VNode id] ->
        (match Node_map.find_opt id !store with
         | Some node when not (is_event node.kind) -> Ok (Eval.VUI (Eval.UIDyn id))
         | _ -> invalid ())
    | (Eval.Show | Eval.Text | Eval.Button | Eval.Column | Eval.Row | Eval.Dyn), _ -> invalid ()
    | Eval.Map, [f; b] -> with_callback f (fun () ->
        allocate [behaviour b] (function [b] -> Map (f, b) | _ -> assert false) Uninitialized true)
    | Eval.Map2, [f; b1; b2] -> with_callback f (fun () ->
        allocate [behaviour b1; behaviour b2]
          (function [b1; b2] -> Map2 (f, b1, b2) | _ -> assert false) Uninitialized true)
    | Eval.MapE, [f; e] -> with_callback f (fun () ->
        allocate [event e] (function [e] -> MapE (f, e) | _ -> assert false) No_occurrence false)
    | Eval.FilterE, [f; e] -> with_callback f (fun () ->
        allocate [event e] (function [e] -> FilterE (f, e) | _ -> assert false) No_occurrence false)
    | Eval.MergeWith, [f; e1; e2] -> with_callback f (fun () ->
        allocate [event e1; event e2]
          (function [e1; e2] -> MergeWith (f, e1, e2) | _ -> assert false) No_occurrence false)
    | Eval.Fold, [seed; f; e] -> with_callback f (fun () ->
        allocate [event e] (function [e] -> Fold (f, e) | _ -> assert false) (Value seed) false)
    | Eval.Hold, [seed; e] ->
        allocate [event e] (function [e] -> Hold e | _ -> assert false) (Value seed) false
    | Eval.Snapshot, [f; e; b] -> with_callback f (fun () ->
        allocate [event e; behaviour b]
          (function [e; b] -> Snapshot (f, e, b) | _ -> assert false) No_occurrence false)
    | _ -> assert false
  in
  let builtins = List.map (fun (name, builtin) -> name, Eval.VBuiltin (builtin, []))
      ["show", Eval.Show; "text", Eval.Text; "button", Eval.Button;
       "column", Eval.Column; "row", Eval.Row; "dyn", Eval.Dyn; "map", Eval.Map; "map2", Eval.Map2; "mapE", Eval.MapE;
       "filterE", Eval.FilterE; "mergeWith", Eval.MergeWith; "fold", Eval.Fold;
       "hold", Eval.Hold; "snapshot", Eval.Snapshot] in
  let rec collect environment = function
    | [] -> Ok environment
    | Ast.Let _ :: rest -> collect environment rest
    | Ast.Event_declaration (name, payload_type) :: rest ->
        if String_set.mem name !source_names then
          Error (Duplicate_source name)
        else
          match allocate [] (fun _ -> Source (name, payload_type)) No_occurrence false with
          | Ok (Eval.VNode id as value) ->
              source_names := String_set.add name !source_names;
              declared_sources := { name; payload_type; id } :: !declared_sources;
              collect ((name, value) :: environment) rest
          | _ -> assert false
  in
  let evaluate environment expression =
    Result.map_error (fun error -> Evaluation_error error)
      (Eval.eval_with_builtins apply environment expression)
  in
  let rec declarations environment = function
    | [] -> evaluate environment program.Ast.render
    | Ast.Event_declaration _ :: rest -> declarations environment rest
    | Ast.Let (name, _, expression) :: rest ->
        match evaluate environment expression with
        | Error _ as error -> error
        | Ok value -> declarations ((name, value) :: environment) rest
  in
  match collect builtins program.Ast.declarations with
  | Error _ as error -> error
  | Ok environment ->
      match declarations environment program.Ast.declarations with
      | Error _ as error -> error
      | Ok value -> Ok { graph = { nodes_by_id = !store; declared_sources = List.rev !declared_sources };
                         dirty = !dirty; value }
