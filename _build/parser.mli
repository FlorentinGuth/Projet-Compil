
(* The type of tokens. *)

type token = 
  | WITH
  | WHILE
  | USE
  | UNARY_MINUS
  | TYPE
  | TIMES
  | THEN
  | SEMICOLON
  | RPAREN
  | REVERSE
  | RETURN
  | REM
  | RECORD
  | PUT
  | PROC
  | PLUS
  | OUT
  | OR_ELSE
  | OR
  | NULL
  | NOT
  | NEW_LINE
  | NEW
  | NEQ
  | MINUS
  | LT
  | LPAREN
  | LOOP
  | LEQ
  | IS
  | INT of (int)
  | IN
  | IF
  | IDENT of (string)
  | GT
  | GEQ
  | FUNC
  | FOR
  | EQ
  | EOF
  | END
  | ELSIF
  | ELSE
  | DOTDOT
  | DOT
  | DIV
  | COMMA
  | COLON
  | CHAR_VAL
  | CHAR of (char)
  | BOOL of (bool)
  | BEGIN
  | AND_THEN
  | AND
  | AFFECT
  | ACCESS

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val file: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (unit Ast.proc)
