{
open Parser

exception Error of string

let keyword = function
  | "let" -> LET | "event" -> EVENT | "render" -> RENDER | "in" -> IN
  | "if" -> IF | "then" -> THEN | "else" -> ELSE | "fn" -> FN
  | "Int" -> INT_TYPE | "Bool" -> BOOL_TYPE | "String" -> STRING_TYPE
  | "UI" -> UI_TYPE | "Unit" -> UNIT_TYPE | "List" -> LIST_TYPE
  | "Behaviour" -> BEHAVIOUR_TYPE | "Event" -> EVENT_TYPE
  | "true" -> TRUE | "false" -> FALSE
  | name -> IDENTIFIER name
}

let digit = ['0'-'9']
let identifier = ['a'-'z' 'A'-'Z' '0'-'9']+
let whitespace = [' ' '\t' '\r']+

rule token = parse
  | whitespace { token lexbuf }
  | '\n' { Lexing.new_line lexbuf; token lexbuf }
  | digit+ as n {
      try INTEGER (int_of_string n)
      with Failure _ -> raise (Error ("Integer out of range: " ^ n))
    }
  | identifier as name { keyword name }
  | '"' { string_literal (Buffer.create 32) lexbuf }
  | "->" { ARROW } | "==" { EQUAL }
  | '(' { LPAREN } | ')' { RPAREN } | '[' { LBRACKET } | ']' { RBRACKET }
  | ':' { COLON } | ';' { SEMICOLON } | '=' { ASSIGN }
  | '+' { PLUS } | '-' { MINUS } | '*' { STAR } | '<' { LESS } | '^' { CARET }
  | eof { EOF }
  | _ as c { raise (Error (Printf.sprintf "Unexpected character: %C" c)) }

and string_literal buffer = parse
  | '"' { STRING (Buffer.contents buffer) }
  | "\\\"" { Buffer.add_char buffer '"'; string_literal buffer lexbuf }
  | "\\\\" { Buffer.add_char buffer '\\'; string_literal buffer lexbuf }
  | "\\n" { Buffer.add_char buffer '\n'; string_literal buffer lexbuf }
  | "\\t" { Buffer.add_char buffer '\t'; string_literal buffer lexbuf }
  | '\\' _ as escape { raise (Error ("Invalid string escape: " ^ escape)) }
  | '\\' eof { raise (Error "Unterminated string escape") }
  | '\n' { Lexing.new_line lexbuf; Buffer.add_char buffer '\n'; string_literal buffer lexbuf }
  | eof { raise (Error "Unterminated string literal") }
  | _ as c { Buffer.add_char buffer c; string_literal buffer lexbuf }
