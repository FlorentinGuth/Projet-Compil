{
open Utils
open Parser
open Lexing
open Printer
open Format


let error lexbuf s = raise (Lexing_error (s, (lexeme_start_p lexbuf,
                                              lexeme_end_p   lexbuf)))

let reserved = assoc_to_hashtbl
                 [("access",        ACCESS);
                  ("all",           ALL);
                  ("and",           AND);
                  ("begin",         BEGIN);
                  ("else",          ELSE);
                  ("elsif",         ELSIF);
                  ("end",           END);
                  ("false",         BOOL false);
                  ("for",           FOR);
                  ("function",      FUNC);
                  ("if",            IF);
                  ("in",            IN);
                  ("is",            IS);
                  ("loop",          LOOP);
                  ("new",           NEW);
                  ("not",           NOT);
                  ("null",          NULL);
                  ("or",            OR);
                  ("out",           OUT);
                  ("procedure",     PROC);
                  ("record",        RECORD);
                  ("rem",           REM);
                  ("return",        RETURN);
                  ("reverse",       REVERSE);
                  ("then",          THEN);
                  ("true",          BOOL true);
                  ("type",          TYPE);
                  ("use",           USE);
                  ("while",         WHILE);
                  ("with",          WITH);
                 ]

let to_token s =
  let s' = String.lowercase_ascii s in
  try
    Hashtbl.find reserved s'
  with
  | Not_found -> IDENT s'

let to_int lexbuf s =
  try
    let n = int_of_string s in
    assert (n <= 1 lsl 31);
    INT n
  with
  | Failure _ | Assert_failure _ ->
    error lexbuf (sprintf "Integer too big: %s" s)
}


let epsilon = ""
let newline = "\n" | "\r\n"
let separator = [' ' '\t']
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']

let ident = (letter (letter | digit | '_')*)
let integer = (digit+)
let quote = '\''
let char = ['\000'-'\127']
let comment = "--"


rule token = parse
  | separator               { token lexbuf }
  | newline                 { new_line lexbuf; token lexbuf }
  | comment                 { comment lexbuf }
  | ident as s              { to_token s }
  | integer as n            { to_int lexbuf n }
  | quote (char as c) quote { CHAR c }
  | "="  { EQ }
  | "/=" { NEQ }
  | "<"  { LT }
  | "<=" { LEQ }
  | ">"  { GT }
  | ">=" { GEQ }
  | "+"  { PLUS }
  | "-"  { MINUS }
  | "*"  { TIMES }
  | "/"  { DIV }
  | ","  { COMMA }
  | ";"  { SEMICOLON }
  | ":"  { COLON }
  | ":=" { AFFECT }
  | "."  { DOT }
  | ".." { DOTDOT }
  | "'"  { QUOTE }
  | "("  { LPAREN }
  | ")"  { RPAREN }
  | eof  { EOF }
  | _ as c { error lexbuf (sprintf "Unknown character %c" c) }
    
and comment = parse
  | newline      { new_line lexbuf; token lexbuf }
  | eof          { EOF }
  | _            { comment lexbuf }


{
  (** Tokens printer *)
  
  let print_token fmt = function
    | WITH        -> fprintf fmt "WITH"
    | USE         -> fprintf fmt "USE"
    | PROC        -> fprintf fmt "PROC"
    | FUNC        -> fprintf fmt "FUNC"
    | RETURN      -> fprintf fmt "RETURN"
    | IS          -> fprintf fmt "IS"
    | BEGIN       -> fprintf fmt "BEGIN"
    | END         -> fprintf fmt "END"
    | TYPE        -> fprintf fmt "TYPE"
    | ACCESS      -> fprintf fmt "ACCESS"
    | RECORD      -> fprintf fmt "RECORD"
    | ALL         -> fprintf fmt "ALL"
    | INT n       -> fprintf fmt "@[INT %d@]" n
    | CHAR c      -> fprintf fmt "@[CHAR %c@]" c
    | BOOL b      -> fprintf fmt "@[BOOL %b@]" b
    | NULL        -> fprintf fmt "NULL"
    | IDENT s     -> fprintf fmt "%s" s
    | OR          -> fprintf fmt "OR"
    | OR_ELSE     -> fprintf fmt "OR_ELSE"
    | AND         -> fprintf fmt "AND"
    | AND_THEN    -> fprintf fmt "AND_THEN"
    | NOT         -> fprintf fmt "NOT"
    | EQ          -> fprintf fmt "="
    | NEQ         -> fprintf fmt "/="
    | GT          -> fprintf fmt ">"
    | GEQ         -> fprintf fmt ">="
    | LT          -> fprintf fmt "<"
    | LEQ         -> fprintf fmt "<="
    | PLUS        -> fprintf fmt "+"
    | MINUS       -> fprintf fmt "-"
    | TIMES       -> fprintf fmt "*"
    | DIV         -> fprintf fmt "/"
    | REM         -> fprintf fmt "%%"
    | UNARY_MINUS -> fprintf fmt "-"
    | NEW         -> fprintf fmt "NEW"
    | IF          -> fprintf fmt "IF"
    | THEN        -> fprintf fmt "THEN"
    | ELSIF       -> fprintf fmt "ELSIF"
    | ELSE        -> fprintf fmt "ELSE"
    | WHILE       -> fprintf fmt "WHILE"
    | FOR         -> fprintf fmt "FOR"
    | REVERSE     -> fprintf fmt "REVERSE"
    | LOOP        -> fprintf fmt "LOOP"
    | COMMA       -> fprintf fmt ","
    | SEMICOLON   -> fprintf fmt ";"
    | COLON       -> fprintf fmt ":"
    | AFFECT      -> fprintf fmt ":="
    | DOT         -> fprintf fmt "."
    | DOTDOT      -> fprintf fmt ".."
    | QUOTE       -> fprintf fmt "'"
    | LPAREN      -> fprintf fmt "("
    | RPAREN      -> fprintf fmt ")"
    | IN          -> fprintf fmt "IN"
    | OUT         -> fprintf fmt "OUT"
    | EOF         -> fprintf fmt "EOF"
                       
  let print_token_list fmt l =
    print_list (print_sep " ") print_token fmt l;
    pp_print_flush fmt ()
}
