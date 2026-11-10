%{
open Ast
%}

%token LET EVENT RENDER IN IF THEN ELSE FN
%token INT_TYPE BOOL_TYPE STRING_TYPE UI_TYPE UNIT_TYPE LIST_TYPE BEHAVIOUR_TYPE EVENT_TYPE
%token TRUE FALSE
%token <int> INTEGER
%token <string> IDENTIFIER STRING
%token LPAREN RPAREN LBRACKET RBRACKET COLON SEMICOLON
%token ASSIGN ARROW PLUS MINUS STAR EQUAL LESS CARET EOF

%start program
%type <Ast.program> program

%%

program:
  declarations RENDER expression EOF { { declarations = $1; render = $3 } }
;

declarations:
  /* empty */ { [] }
| declaration declarations { $1 :: $2 }
;

declaration:
  LET IDENTIFIER annotation ASSIGN expression { Let ($2, $3, $5) }
| EVENT IDENTIFIER COLON typ { Event_declaration ($2, $4) }
;

annotation:
  /* empty */ { None }
| COLON typ { Some $2 }
;

typ:
  type_prefix { $1 }
| type_prefix ARROW typ { Arrow ($1, $3) }
;

type_prefix:
  INT_TYPE { Int }
| BOOL_TYPE { Bool }
| STRING_TYPE { String }
| UI_TYPE { UI }
| UNIT_TYPE { Unit }
| LIST_TYPE type_prefix { List $2 }
| BEHAVIOUR_TYPE type_prefix { Behaviour $2 }
| EVENT_TYPE type_prefix { Event $2 }
| LPAREN typ RPAREN { $2 }
;

expression:
  comparison { $1 }
;

comparison:
  concatenation { $1 }
| comparison EQUAL concatenation { Binary (Equal, $1, $3) }
| comparison LESS concatenation { Binary (Less_than, $1, $3) }
;

concatenation:
  addition { $1 }
| addition CARET concatenation { Binary (Concat, $1, $3) }
;

addition:
  multiplication { $1 }
| addition PLUS multiplication { Binary (Add, $1, $3) }
| addition MINUS multiplication { Binary (Subtract, $1, $3) }
;

multiplication:
  application { $1 }
| multiplication STAR application { Binary (Multiply, $1, $3) }
;

application:
  atom { $1 }
| application atom { Apply ($1, $2) }
;

atom:
  INTEGER { Literal (Integer $1) }
| TRUE { Literal (Boolean true) }
| FALSE { Literal (Boolean false) }
| STRING { Literal (String_literal $1) }
| LPAREN RPAREN { Literal Unit_literal }
| IDENTIFIER { Identifier $1 }
| FN LPAREN IDENTIFIER COLON typ RPAREN ARROW expression { Lambda ($3, $5, $8) }
| LET IDENTIFIER annotation ASSIGN expression IN expression { Let_in ($2, $3, $5, $7) }
| IF expression THEN expression ELSE expression { If ($2, $4, $6) }
| LBRACKET expressions RBRACKET { List_literal $2 }
;

expressions:
  expression { [$1] }
| expression SEMICOLON expressions { $1 :: $3 }
;
