{
open Utils
open Parser
open Lexing


let error lexbuf s = raise (Lexing_error (s, (lexeme_start_p lexbuf,
                                              lexeme_end_p   lexbuf)))
(*
let new_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with pos_lnum = pos.pos_lnum + 1;
                                  pos_bol = pos.pos_cnum
                       }*)

let reserved = assoc_to_hashtbl
                 [("access",        ACCESS);
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
                  ("character'val", IDENT "character'val");
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
    error lexbuf (Format.sprintf "Integer too big: %s" s)
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
  | _ as c { error lexbuf (Format.sprintf "Unknown character %c" c) }
    
and comment = parse
  | newline      { new_line lexbuf; token lexbuf }
  | eof          { EOF }
  | _            { comment lexbuf }
