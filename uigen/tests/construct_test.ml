open Uigen
open Ast
open Construct

let id x = Identifier x
let int n = Literal (Integer n)
let app f args = List.fold_left (fun f x -> Apply (f, x)) (id f) args
let lambda body = Lambda ("x", Int, body)
let fn = lambda (id "x")
let event x = Event_declaration (x, Unit)
let let_ x e = Let (x, None, e)
let program declarations render = { declarations; render }
let run p = match Construct.run p with Ok result -> result | Error _ -> failwith "construction failed"
let check condition message = if not condition then failwith message
let node graph id = match find_node graph id with Some node -> node | None -> failwith "missing node"
let reference = function Eval.VNode id -> id | _ -> failwith "expected node reference"
let ids set = Node_set.elements set
let check_error p predicate =
  match Construct.run p with Error e when predicate e -> () | _ -> failwith "expected error"

let () =
  let p = program [event "click"; let_ "count" (app "hold" [int 0; id "click"])]
      (app "map" [lambda (Binary (Add, id "x", int 1)); id "count"]) in
  let result = run p in
  let graph = result.graph in
  check (List.length (nodes graph) = 3) "acceptance node count";
  check (result.value = Eval.VNode 2 && ids result.dirty = [2]) "acceptance value and dirty";
  check ((node graph 0).kind = Source ("click", Unit)) "source kind";
  check ((node graph 0).contents = No_occurrence && ids (node graph 0).dependents = [1]) "source state";
  check ((node graph 1).kind = Hold 0 && (node graph 1).contents = Value (Eval.VInt 0)) "hold state";
  check (ids (node graph 1).dependents = [2]) "hold dependent";
  check ((node graph 2).contents = Uninitialized) "map state";
  check (match (node graph 2).kind with Map (Eval.VClosure _, 1) -> true | _ -> false) "map kind";
  check (find_source graph "click" = Some {name = "click"; payload_type = Unit; id = 0}) "source lookup";
  check (find_source graph "absent" = None && find_node graph 99 = None) "missing lookups";
  let p = program
      [event "a"; event "b";
       let_ "hold" (app "hold" [int 4; id "a"]);
       let_ "m2" (app "map2" [fn; id "hold"; id "hold"]);
       let_ "me" (app "mapE" [fn; id "a"]);
       let_ "fe" (app "filterE" [fn; id "a"]);
       let_ "merge" (app "mergeWith" [fn; id "a"; id "b"]);
       let_ "folded" (app "fold" [int 7; fn; id "a"]);
       let_ "snap" (app "snapshot" [fn; id "b"; id "hold"])] (id "snap") in
  let r = run p in
  let g = r.graph in
  check (List.map (fun source -> source.name) (sources g) = ["a"; "b"]) "source order";
  check (List.length (nodes g) = 9 && ids r.dirty = [3]) "all combinators count and dirty";
  check (match (node g 3).kind with Map2 (_, 2, 2) -> true | _ -> false) "repeated map2 parents";
  check (ids (node g 2).dependents = [3; 8]) "distinct repeated parent registration and snapshot";
  check (match (node g 4).kind, (node g 4).contents with MapE (_, 0), No_occurrence -> true | _ -> false) "mapE";
  check (match (node g 5).kind, (node g 5).contents with FilterE (_, 0), No_occurrence -> true | _ -> false) "filterE";
  check (match (node g 6).kind, (node g 6).contents with MergeWith (_, 0, 1), No_occurrence -> true | _ -> false) "mergeWith";
  check (match (node g 7).kind, (node g 7).contents with Fold (_, 0), Value (Eval.VInt 7) -> true | _ -> false) "fold";
  check (match (node g 8).kind, (node g 8).contents with Snapshot (_, 1, 2), No_occurrence -> true | _ -> false) "snapshot";
  check (ids (node g 1).dependents = [6; 8]) "snapshot event dependency";
  let later = run (program [let_ "x" (id "future"); event "future"] (id "x")) in
  check (reference later.value = 0) "source visible before declaration";
  check (reference (run (program [event "again"] (id "again"))).value = 0) "fresh IDs";
  let alias = run (program [event "e"; let_ "x" (id "e"); let_ "h" (id "hold");
                            let_ "one" (app "h" [int 0; id "x"]);
                            let_ "two" (app "h" [int 0; id "e"])]
                     (List_literal [id "x"; id "one"; id "two"])) in
  check (alias.value = Eval.VList [Eval.VNode 0; Eval.VNode 1; Eval.VNode 2]) "alias and allocations";
  let partial = run (program [event "e"; let_ "partial" (app "mapE" [fn])]
                       (If (Literal (Boolean true), id "partial", app "mapE" [fn; id "e"]))) in
  check (List.length (nodes partial.graph) = 1) "partial and untaken branch allocate nothing";
  check (match partial.value with Eval.VBuiltin (Eval.MapE, [_]) -> true | _ -> false) "partial value";
  let captured = run (program [event "e"; let_ "x" (int 42);
                               let_ "m" (app "mapE" [lambda (id "missing"); id "e"])] (id "m")) in
  check (match (node captured.graph 1).kind with
    | MapE (Eval.VClosure (_, Identifier "missing", env), 0) -> List.assoc_opt "x" env = Some (Eval.VInt 42)
    | _ -> false) "callback captured without execution";
  let shadow = run (program [let_ "hold" (int 9)] (id "hold")) in
  check (shadow.value = Eval.VInt 9) "builtin shadowing";
  check_error (program [event "a"; event "a"] (int 0)) (function Duplicate_source "a" -> true | _ -> false);
  check_error (program [event "a"; event "b"; event "a"] (int 0)) (function Duplicate_source "a" -> true | _ -> false);
  check_error (program [event "e"] (app "map" [fn; id "e"]))
    (function Evaluation_error (Eval.Invalid_parent (Eval.Behaviour_parent, Eval.VNode 0)) -> true | _ -> false);
  check_error (program [event "e"] (app "mapE" [int 0; id "e"]))
    (function Evaluation_error (Eval.Invalid_callback (Eval.VInt 0)) -> true | _ -> false);
  check_error (program [] (id "missing"))
    (function Evaluation_error (Eval.Unbound_variable "missing") -> true | _ -> false);
  check_error (program [event "e"; let_ "b" (app "hold" [int 0; id "e"])]
                 (app "snapshot" [fn; id "b"; id "b"]))
    (function Evaluation_error (Eval.Invalid_parent (Eval.Event_parent, Eval.VNode 1)) -> true | _ -> false);
  let source_shadow = run (program [event "hold"] (id "hold")) in
  check (source_shadow.value = Eval.VNode 0) "source shadows builtin";
  let callback_builtin = run (program [event "e"] (app "mapE" [id "hold"; id "e"])) in
  check (match (node callback_builtin.graph 1).kind with
    | MapE (Eval.VBuiltin (Eval.Hold, []), 0) -> true | _ -> false) "builtin is callable"


let () =
  let p = program [event "click"; let_ "b" (app "hold" [int 0; id "click"]);
                   let_ "makeButton" (app "button" [Literal (String_literal "Go")])]
      (app "column" [List_literal [app "row" [List_literal [app "text" [app "show" [int 42]]]];
                                   app "makeButton" [id "click"]; app "dyn" [id "b"]]]) in
  check ((run p).value = Eval.VUI (Eval.UIColumn [Eval.UIRow [Eval.UIText "42"]; Eval.UIButton ("Go", 0); Eval.UIDyn 1])) "UI constructors";
  List.iter (fun expr -> check_error (program [event "e"] expr)
    (function Evaluation_error (Eval.Invalid_builtin_arguments _) -> true | _ -> false))
    [app "show" [id "e"]; app "text" [int 1]; app "button" [Literal (String_literal "x"); int 0];
     app "column" [List_literal [int 1]]; app "row" [int 1]; app "dyn" [id "e"]];
  check_error (program [event "e"; let_ "derived" (app "mapE" [fn; id "e"])]
    (app "button" [Literal (String_literal "x"); id "derived"]))
    (function Evaluation_error (Eval.Invalid_builtin_arguments _) -> true | _ -> false);
  check_error (program [Event_declaration ("reading", Int)]
    (app "button" [Literal (String_literal "Read"); id "reading"]))
    (function Evaluation_error (Eval.Invalid_builtin_arguments _) -> true | _ -> false)
