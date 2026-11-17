open Uigen

let rec string_of_value = function
  | Eval.VInt n -> string_of_int n
  | Eval.VBool b -> string_of_bool b
  | Eval.VString s -> Printf.sprintf "%S" s
  | Eval.VUnit -> "()"
  | Eval.VClosure _ -> "<function>"
  | Eval.VList values -> "[" ^ String.concat "; " (List.map string_of_value values) ^ "]"

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

let run source channel =
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
    match evaluate program with
    | Ok value -> print_endline (string_of_value value); 0
    | Error message -> prerr_endline ("Evaluation error: " ^ message); 1
  with
  | Lexer.Error message ->
      Printf.eprintf "%s: lexical error: %s\n" (location lexbuf) message; 1
  | Lexical_error_at (where, message) ->
      Printf.eprintf "%s: lexical error: %s\n" where message; 1
  | Parsing.Parse_error ->
      Printf.eprintf "%s: syntax error\n" (location lexbuf); 1

let main () =
  match Array.to_list Sys.argv with
  | [_] -> run "<stdin>" stdin
  | [_; path] ->
      (try
         let channel = open_in path in
         Fun.protect ~finally:(fun () -> close_in_noerr channel)
           (fun () -> run path channel)
       with Sys_error message -> prerr_endline message; 1)
  | _ -> prerr_endline "Usage: uigen [FILE]"; 1

let () =
  let status = try main () with Sys_error message -> prerr_endline message; 1 in
  exit status
