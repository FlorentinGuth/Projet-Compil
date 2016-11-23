%{
  open Ast

  exception Parser_error of string
  

  let developp (l, y) = 
    List.map (fun x -> (x, y)) l

  let developp_full l =
    List.flatten (List.map developp l)

  let verify x = function
    | None -> true
    | Some y -> x = y

 let to_some default = function
   | None -> default
   | Some x -> x
 let option_to_list = to_some []

 let decorate desc deco = 
   { desc; deco }
%}

(** List of tokens *)

%token WITH USE
%token PUT NEW_LINE CHAR_VAL
%token PROC FUNC RETURN IS BEGIN END
%token TYPE ACCESS RECORD

%token <int>  INT
%token <char> CHAR
%token <bool> BOOL
%token        NULL

%token <string> IDENT
%token OR OR_ELSE AND AND_THEN NOT
%token EQ NEQ GT GEQ LT LEQ
%token PLUS MINUS TIMES DIV REM UNARY_MINUS
%token NEW

%token IF THEN ELSIF ELSE
%token WHILE FOR DOTDOT REVERSE LOOP

%token COMMA SEMICOLON COLON AFFECT DOT
%token LPAREN RPAREN
%token IN OUT

%token EOF


(** Associativities and precedences *)

%left     OR OR_ELSE
%left     AND AND_THEN
%nonassoc NOT

%nonassoc EQ NEQ
%nonassoc GT GEQ LT LEQ

%left     PLUS MINUS
%left     TIMES DIV REM
%nonassoc UNARY_MINUS

%left     DOT


(** Entry point *)

%start <unit Ast.proc> file


%% (** Beginning of rules *)

file:
  WITH; ada1 = ident; DOT; text_io1 = ident; SEMICOLON;
  USE;  ada2 = ident; DOT; text_io2 = ident; SEMICOLON; 
  PROC; p = proc; EOF 
    { if not (ada1.desc = "ada" && text_io1.desc = "text_io" &&
        ada2.desc = "ada" && text_io2.desc = "text_io")
      then raise
             (Parser_error
                "Wrong header, should be \"with Ada.Text_IO; use Ada.Text_IO;\"");
      if p.params <> [] then raise
                               (Parser_error
                                  "Main prcedure can't take any parameters");
      p }


%inline decorated(X):
  | x = X { decorate x () }


ident_desc:
  | id = IDENT { id }
ident:
  | id = decorated(ident_desc) { id }

type_annot:
  | id = ident { Aident id }
  | ACCESS; id = ident { Aaccess id }

mode:
  | | IN { In } (* Default mode is In *)
  | IN; OUT { InOut }

param:
  | ids = separated_nonempty_list(COMMA, ident); COLON; m = mode; typ = type_annot
    { List.map (fun id -> (id, m, typ)) ids }

params:
  | LPAREN; p = separated_nonempty_list(SEMICOLON, param); RPAREN
    { List.flatten p }

fields:
  | ids = separated_nonempty_list(COMMA, ident); COLON; typ = type_annot; SEMICOLON
    { developp (ids, typ) }


proc_or_func:
  | name = ident; ps = option(params); rt = option(preceded(RETURN, type_annot));
    IS; decls = decls; BEGIN; stmt = stmts; END; end_id = option(ident); SEMICOLON
    { if name <> to_some name end_id
      then
        raise
          (Parser_error
             "Ident following \"end\" must have same name than the function/procedure");
      { name;
        params = option_to_list ps;
        return = rt;
        decls;
        stmt } }

proc:
  | p = proc_or_func { assert (p.return = None); { p with return = () } }

func:
  | f = proc_or_func { match f.return with
                       | Some t -> { f with return = t }
                       | None ->
                          raise
                            (Parser_error
                               "A function must have an annotated return type") }


decl:
  | TYPE; id = ident; typ = option(preceded(IS, type_annot)); SEMICOLON 
    { [Dtype (id, typ)] }
  | TYPE; id = ident; IS; RECORD; r = nonempty_list(fields); END; RECORD; SEMICOLON
    { [Dtype_record (id, List.flatten r)] }
  | lv_list = separated_nonempty_list(COMMA, ident); COLON; typ = type_annot;
    e = option(preceded(AFFECT, expr)); SEMICOLON;
    { List.map (fun lv -> Dvar_decl (lv, typ, e)) lv_list }
  | PROC; p = proc { [Dproc p] }
  | FUNC; f = func { [Dfunc f] }

decls:
  | ds = list(decl) { List.flatten ds }


const:
  | n = INT  { Cint  n }
  | c = CHAR { Cchar c }
  | b = BOOL { Cbool b }
  | NULL     { Cnull }

%inline left_val:
  | id = ident { Lident id } (* Can be a function !! *)
  | e = expr; DOT; field = ident { Lmember (e, field) }


expr_desc:
  | c = const { Econst c }
  | LPAREN; e = expr; RPAREN { e.desc }
  | lv = left_val { Eleft_val lv }
  | NOT; e = expr { Eunop (Unot, e) }
  | MINUS; e = expr { Eunop (Uminus, e) } %prec UNARY_MINUS
  | e1 = expr; EQ; e2 = expr { Ebinop (e1, Beq, e2) }
  | e1 = expr; NEQ; e2 = expr { Ebinop (e1, Bneq, e2) }
  | e1 = expr; LT; e2 = expr { Ebinop (e1, Blt, e2) }
  | e1 = expr; LEQ; e2 = expr { Ebinop (e1, Bleq, e2) }
  | e1 = expr; GT; e2 = expr { Ebinop (e1, Bgt, e2) }
  | e1 = expr; GEQ; e2 = expr { Ebinop (e1, Bgeq, e2) }
  | e1 = expr; PLUS; e2 = expr { Ebinop (e1, Bplus, e2) }
  | e1 = expr; MINUS; e2 = expr { Ebinop (e1, Bminus, e2) }
  | e1 = expr; TIMES; e2 = expr { Ebinop (e1, Btimes, e2) }
  | e1 = expr; DIV; e2 = expr { Ebinop (e1, Bdiv, e2) }
  | e1 = expr; REM; e2 = expr { Ebinop (e1, Brem, e2) }
  | e1 = expr; AND; e2 = expr { Ebinop (e1, Band, e2) }
  | e1 = expr; AND; THEN; e2 = expr { Ebinop (e1, Band_then, e2) } %prec AND_THEN
  | e1 = expr; OR; e2 = expr { Ebinop (e1, Bor, e2) }
  | e1 = expr; OR; ELSE; e2 = expr { Ebinop (e1, Bor_else, e2) } %prec OR_ELSE
  | NEW; id = ident { Enew id }
  | f = ident; LPAREN; ps = separated_nonempty_list(COMMA, expr); RPAREN
    { Eapp_func (f, ps) }
  | CHAR_VAL; LPAREN; e = expr; RPAREN { Echar_val e }
expr:
  | e = decorated(expr_desc) { e }


stmt:
  | lv = left_val; AFFECT; e = expr; SEMICOLON { decorate (Saffect (lv, e)) () }
  | p = ident; SEMICOLON { decorate (Scall_proc (p, [])) () }
  | p = ident; LPAREN; ps = separated_nonempty_list(COMMA, expr); RPAREN; SEMICOLON
    { decorate (Scall_proc (p, ps)) () }
  | PUT; LPAREN; e = expr; RPAREN; SEMICOLON { decorate (Sput e) () }
  | NEW_LINE; SEMICOLON { decorate Snew_line () }
  | RETURN; e = option(expr); SEMICOLON 
   { decorate (Sreturn (to_some (decorate (Econst Cnull) ()) e)) () }
  | BEGIN; s = stmts; END; SEMICOLON { decorate (s.desc) () }
  | IF; e = expr; THEN; s1 = stmts; 
    elsif = list(pair(preceded(ELSIF, expr), preceded(THEN, stmts)));
    else_opt = option(preceded(ELSE, stmts)); END; IF; SEMICOLON
    { let rec fold = function
        | [] -> to_some (decorate (Sblock []) ()) else_opt
        | (e, s) :: tl -> decorate (Scond (e, s, fold tl)) ()
      in
      decorate (Scond (e, s1, fold elsif)) ()}
  | WHILE; e = expr; LOOP; s = stmts; END; LOOP; SEMICOLON 
    { decorate (Swhile (e, s)) () }
  | FOR; i = ident; IN; r = option(REVERSE); el = expr; DOTDOT; eu = expr;
    LOOP; s = stmts; END; LOOP; SEMICOLON
    { let r = match r with
        | None -> false
        | Some () -> true
      in
      decorate (Sfor (i, r, el, eu, s)) () }

stmts:
  | l = nonempty_list(stmt) { decorate (Sblock l) () }
