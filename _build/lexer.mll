{
open Parser
open Lexing  


exception Lexical_error of string
let error s = raise (Lexical_error s)

let new_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with pos_lnum = pos.pos_lnum + 1;
                                  pos_bol = pos.pos_cnum
                       }


let assoc_to_hashtbl l =
  let t = Hashtbl.create 42 in
  List.iter (fun (k, v) -> Hashtbl.add t k v) l;
  t

let reserved = assoc_to_hashtbl
                 [("access",    ACCESS);
                  ("and",       AND);
                  ("begin",     BEGIN);
                  ("else",      ELSE);
                  ("elsif",     ELSIF);
                  ("end",       END);
                  ("false",     BOOL false);
                  ("for",       FOR);
                  ("function",  FUNC);
                  ("if",        IF);
                  ("in",        IN);
                  ("is",        IS);
                  ("loop",      LOOP);
                  ("new",       NEW);
                  ("not",       NOT);
                  ("null",      NULL);
                  ("or",        OR);
                  ("out",       OUT);
                  ("procedure", PROC);
                  ("record",    RECORD);
                  ("rem",       REM);
                  ("return",    RETURN);
                  ("reverse",   REVERSE);
                  ("then",      THEN);
                  ("true",      BOOL true);
                  ("type",      TYPE);
                  ("use",       USE);
                  ("while",     WHILE);
                  ("with",      WITH);
                  ("put",       PUT);
                  ("new_line",  NEW_LINE);
                 ]

let to_token s =
  let s' = String.lowercase_ascii s in
  try
    Hashtbl.find reserved s'
  with
  | Not_found -> IDENT s'

}


let epsilon = ""
let newline = '\n'
let separator = [' ' '\t']
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']

let ident = (letter (letter | digit | '_')*)
let integer = (digit+)
let char = '\'' _ '\''
let comment = "--"


rule token = parse
  | separator    { token lexbuf }
  | newline      { new_line lexbuf; token lexbuf }
  | comment      { comment lexbuf }
  | (ident as s1) "'" (ident as s2) { if String.lowercase_ascii s1 = "character"
                                           && String.lowercase_ascii s2 = "val"
                                      then CHAR_VAL
                                      else error "\"'\" is not a valid character here" }
  | ident as s   { to_token s }
  | integer as n { try
                     let n = int_of_string n in
                     assert (n <= 1 lsl 31);
                     INT n
                   with _ -> error "Integer too big" }
  | char as c    { CHAR c.[1] }
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
  | "("  { LPAREN }
  | ")"  { RPAREN }
  | eof          { EOF }
  | _ as c       { error ("Unknown character " ^ (String.make 1 c)) }
    
and comment = parse
  | newline      { new_line lexbuf; token lexbuf }
  | eof          { EOF }
  | _            { comment lexbuf }
