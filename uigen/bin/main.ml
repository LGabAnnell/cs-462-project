open Uigen

let rec string_of_value = function
  | Eval.VInt n -> string_of_int n
  | Eval.VBool b -> string_of_bool b
  | Eval.VString s -> Printf.sprintf "%S" s
  | Eval.VUnit -> "()"
  | Eval.VClosure _ -> "<function>"
  | Eval.VUI ui -> string_of_ui ui
  | Eval.VNode id -> Printf.sprintf "<node %d>" id
  | Eval.VBuiltin _ -> "<function>"
  | Eval.VList values -> "[" ^ String.concat "; " (List.map string_of_value values) ^ "]"

and string_of_ui = function
  | Eval.UIText s -> Printf.sprintf "text %S" s
  | Eval.UIButton (s, id) -> Printf.sprintf "button %S <node %d>" s id
  | Eval.UIColumn uis -> "column [" ^ String.concat "; " (List.map string_of_ui uis) ^ "]"
  | Eval.UIRow uis -> "row [" ^ String.concat "; " (List.map string_of_ui uis) ^ "]"
  | Eval.UIDyn id -> Printf.sprintf "dyn <node %d>" id

let string_of_binop = function
  | Ast.Add -> "+" | Ast.Subtract -> "-" | Ast.Multiply -> "*"
  | Ast.Equal -> "==" | Ast.Less_than -> "<" | Ast.Concat -> "^"

let string_of_error = function
  | Eval.Unbound_variable name -> "unbound variable: " ^ name
  | Eval.Invalid_operands (op, left, right) ->
      Printf.sprintf "invalid operands for %s: %s and %s"
        (string_of_binop op) (string_of_value left) (string_of_value right)
  | Eval.Non_boolean_condition value ->
      "condition is not a boolean: " ^ string_of_value value
  | Eval.Non_function_application value ->
      "cannot apply non-function value: " ^ string_of_value value
  | Eval.Invalid_builtin_arguments (builtin, args) ->
      let name = match builtin with
        | Eval.Show -> "show" | Eval.Text -> "text" | Eval.Button -> "button"
        | Eval.Column -> "column" | Eval.Row -> "row" | Eval.Dyn -> "dyn"
        | _ -> "reactive built-in" in
      "invalid arguments to " ^ name ^ ": " ^ String.concat ", " (List.map string_of_value args)
  | Eval.Invalid_callback value ->
      "reactive callback is not callable: " ^ string_of_value value
  | Eval.Invalid_parent (_, value) ->
      "invalid reactive parent: " ^ string_of_value value

let evaluate program =
  let rec declarations environment = function
    | [] -> Eval.eval environment program.Ast.render |> Result.map_error string_of_error
    | Ast.Event_declaration (name, _) :: _ ->
        Error ("event declaration " ^ name ^ " is unsupported: reactive execution is not implemented")
    | Ast.Let (name, _, expression) :: rest ->
        (match Eval.eval environment expression with
         | Error error -> Error (string_of_error error)
         | Ok value -> declarations ((name, value) :: environment) rest)
  in
  declarations [] program.Ast.declarations

type mode = Auto | Eval | Construct

let rec string_of_type = function
  | Ast.Int -> "Int" | Ast.Bool -> "Bool" | Ast.String -> "String"
  | Ast.UI -> "UI" | Ast.Unit -> "Unit"
  | Ast.List t -> "List (" ^ string_of_type t ^ ")"
  | Ast.Behaviour t -> "Behaviour (" ^ string_of_type t ^ ")"
  | Ast.Event t -> "Event (" ^ string_of_type t ^ ")"
  | Ast.Arrow (a, b) -> "(" ^ string_of_type a ^ " -> " ^ string_of_type b ^ ")"

let kind_and_parents = function
  | Construct.Source (name, typ) -> "source " ^ name ^ " : " ^ string_of_type typ, []
  | Construct.Map (_, b) -> "map", [b]
  | Construct.Map2 (_, a, b) -> "map2", [a; b]
  | Construct.MapE (_, e) -> "mapE", [e]
  | Construct.FilterE (_, e) -> "filterE", [e]
  | Construct.MergeWith (_, a, b) -> "mergeWith", [a; b]
  | Construct.Fold (_, e) -> "fold", [e]
  | Construct.Hold e -> "hold", [e]
  | Construct.Snapshot (_, e, b) -> "snapshot", [e; b]

let string_of_ids ids =
  "[" ^ String.concat "; " (List.map string_of_int ids) ^ "]"

let print_construction (result : Construct.construction) =
  print_endline "Constructed graph (reactive nodes have not been evaluated):";
  List.iter (fun (node : Construct.node) ->
    let kind, parents = kind_and_parents node.kind in
    let contents = match node.contents with
      | Construct.Uninitialized -> "uninitialized"
      | Construct.No_occurrence -> "no occurrence"
      | Construct.Value value -> string_of_value value in
    Printf.printf "node %d: %s | parents: %s | contents: %s | dependents: %s | dirty: %s\n"
      node.id kind (string_of_ids parents) contents
      (string_of_ids (Construct.Node_set.elements node.dependents))
      (if Construct.Node_set.mem node.id result.dirty then "yes" else "no"))
    (Construct.nodes result.graph);
  Printf.printf "render: %s\n" (string_of_value result.value)

let construct ?(graph = true) program =
  match Construct.run program with
  | Ok result ->
      if graph then print_construction result
      else print_endline (string_of_value result.value);
      0
  | Error (Construct.Duplicate_source name) ->
      prerr_endline ("Construction error: duplicate event source: " ^ name); 1
  | Error (Construct.Evaluation_error error) ->
      prerr_endline ("Construction error: " ^ string_of_error error); 1

let location lexbuf =
  let position = Lexing.lexeme_start_p lexbuf in
  Printf.sprintf "%s:%d:%d" position.Lexing.pos_fname position.Lexing.pos_lnum
    (position.Lexing.pos_cnum - position.Lexing.pos_bol + 1)

(* The grammar allows a let-expression as an application argument, so the
   parser can shift a following top-level let into the preceding expression.
   If parsing the whole program fails, try declaration boundaries at keyword
   tokens. Each part must independently parse as exactly one declaration, and
   the suffix must parse as a complete program. This preserves parses that the
   existing grammar already accepts. *)
exception Lexical_error_at of string * string

let parse_declarations source text =
  let lexbuf = Lexing.from_string text in
  Lexing.set_filename lexbuf source;
  let rec boundaries result =
    let token = Lexer.token lexbuf in
    let offset = (Lexing.lexeme_start_p lexbuf).Lexing.pos_cnum in
    match token with
    | Parser.EOF -> List.rev result
    | Parser.LET | Parser.EVENT | Parser.RENDER -> boundaries (offset :: result)
    | _ -> boundaries result
  in
  let positions =
    try boundaries [] with Lexer.Error message ->
      raise (Lexical_error_at (location lexbuf, message))
  in
  let parse_text text =
    try Some (Parser.program Lexer.token (Lexing.from_string text))
    with Parsing.Parse_error | Lexer.Error _ -> None
  in
  let length = String.length text in
  let cache = Hashtbl.create 16 in
  let rec parse_from start =
    match Hashtbl.find_opt cache start with
    | Some result -> result
    | None ->
      let result = parse_uncached start in
      Hashtbl.add cache start result;
      result
  and parse_uncached start =
    let remainder = String.sub text start (length - start) in
    match parse_text remainder with
    | Some program -> Some program
    | None -> try_boundaries start positions
  and try_boundaries start = function
    | [] -> None
    | boundary :: rest when boundary <= start -> try_boundaries start rest
    | boundary :: rest ->
        let part = String.sub text start (boundary - start) in
        match parse_text (part ^ "\nrender 0") with
        | Some { Ast.declarations = [declaration]; _ } ->
            (match parse_from boundary with
             | Some program ->
                 Some { program with Ast.declarations = declaration :: program.Ast.declarations }
             | None -> try_boundaries start rest)
        | _ -> try_boundaries start rest
  in
  parse_from 0

let read_all channel =
  let buffer = Buffer.create 4096 in
  let bytes = Bytes.create 4096 in
  let rec read () =
    match input channel bytes 0 (Bytes.length bytes) with
    | 0 -> Buffer.contents buffer
    | count -> Buffer.add_subbytes buffer bytes 0 count; read ()
  in
  read ()

let run mode source channel =
  let text = read_all channel in
  let lexbuf = Lexing.from_string text in
  Lexing.set_filename lexbuf source;
  try
    let program =
      try Parser.program Lexer.token lexbuf
      with Parsing.Parse_error ->
        match parse_declarations source text with
        | Some program -> program
        | None -> raise Parsing.Parse_error
    in
    (match mode with
     | Construct -> construct program
     | Eval ->
         (match evaluate program with
          | Ok value -> print_endline (string_of_value value); 0
          | Error message -> prerr_endline ("Evaluation error: " ^ message); 1)
     | Auto ->
         let graph = List.exists (function Ast.Event_declaration _ -> true | _ -> false)
             program.Ast.declarations in
         construct ~graph program)
  with
  | Lexer.Error message ->
      Printf.eprintf "%s: lexical error: %s\n" (location lexbuf) message; 1
  | Lexical_error_at (where, message) ->
      Printf.eprintf "%s: lexical error: %s\n" where message; 1
  | Parsing.Parse_error ->
      Printf.eprintf "%s: syntax error\n" (location lexbuf); 1

let main () =
  let usage () = prerr_endline "Usage: uigen [--mode auto|eval|construct] [FILE]"; 1 in
  let rec arguments mode path = function
    | [] -> Some (mode, path)
    | "--" :: rest ->
        (match path, rest with
         | _, [] -> Some (mode, path)
         | None, [file] -> Some (mode, Some file)
         | _ -> None)
    | "--mode" :: value :: rest ->
        (match mode, value with
         | None, "auto" -> arguments (Some Auto) path rest
         | None, "eval" -> arguments (Some Eval) path rest
         | None, "construct" -> arguments (Some Construct) path rest
         | _ -> None)
    | "--mode" :: [] -> None
    | arg :: _ when String.length arg > 0 && arg.[0] = '-' -> None
    | arg :: rest ->
        (match path with None -> arguments mode (Some arg) rest | Some _ -> None)
  in
  match arguments None None (List.tl (Array.to_list Sys.argv)) with
  | None -> usage ()
  | Some (mode, path) ->
    let mode = Option.value mode ~default:Auto in
    match path with
    | None -> run mode "<stdin>" stdin
    | Some path ->
      (try
         let channel = open_in path in
         Fun.protect ~finally:(fun () -> close_in_noerr channel)
           (fun () -> run mode path channel)
       with Sys_error message -> prerr_endline message; 1)

let () =
  let status = try main () with Sys_error message -> prerr_endline message; 1 in
  exit status
