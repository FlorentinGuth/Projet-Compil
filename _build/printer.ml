open Format
open Ast
open Parser


(** General use printers *)

let print_list print_sep print_elt =
  let rec printer fmt = function
    | [] -> ()
    | [x] -> fprintf fmt "%a" print_elt x
    | x :: xs -> fprintf fmt "%a%a%a" print_elt x print_sep () printer xs
  in printer

let print_sep s fmt () =
  fprintf fmt "%s" s

let print_to_string printer arg =
  let () = printer Format.str_formatter arg in
  Format.flush_str_formatter ()


(** Tokens printer *)

let print_token fmt = function
  | WITH        -> fprintf fmt "WITH"
  | USE         -> fprintf fmt "USE"
  | PUT         -> fprintf fmt "PUT"
  | NEW_LINE    -> fprintf fmt "NEW_LINE"
  | CHAR_VAL    -> fprintf fmt "CHAR_VAL"
  | PROC        -> fprintf fmt "PROC"
  | FUNC        -> fprintf fmt "FUNC"
  | RETURN      -> fprintf fmt "RETURN"
  | IS          -> fprintf fmt "IS"
  | BEGIN       -> fprintf fmt "BEGIN"
  | END         -> fprintf fmt "END"
  | TYPE        -> fprintf fmt "TYPE"
  | ACCESS      -> fprintf fmt "ACCESS"
  | RECORD      -> fprintf fmt "RECORD"
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
  | DOTDOT      -> fprintf fmt ".."
  | REVERSE     -> fprintf fmt "REVERSE"
  | LOOP        -> fprintf fmt "LOOP"
  | COMMA       -> fprintf fmt ","
  | SEMICOLON   -> fprintf fmt ";"
  | COLON       -> fprintf fmt ":"
  | AFFECT      -> fprintf fmt ":="
  | DOT         -> fprintf fmt "."
  | LPAREN      -> fprintf fmt "("
  | RPAREN      -> fprintf fmt ")"
  | IN          -> fprintf fmt "IN"
  | OUT         -> fprintf fmt "OUT"
  | EOF         -> fprintf fmt "EOF"

let print_token_list fmt l =
  print_list (print_sep " ") print_token fmt l;
  pp_print_flush fmt ()
                  


(** AST printer *)
          

let print_ident fmt id =
  fprintf fmt "%s" id.desc

let print_unop fmt u =
  fprintf fmt (match u with
                | Unot   -> "not@ "
                | Uminus -> "-"
              )

let print_binop fmt b =
  fprintf fmt (match b with
                | Beq       -> "="
                | Bneq      -> "/="
                | Blt       -> "<"
                | Bleq      -> "<="
                | Bgt       -> ">"
                | Bgeq      -> ">="
                | Bplus     -> "+"
                | Bminus    -> "-"
                | Btimes    -> "*"
                | Bdiv      -> "/"
                | Brem      -> "rem"
                | Band      -> "and"
                | Band_then -> "and then"
                | Bor       -> "or"
                | Bor_else  -> "or else"
              )

let print_type_annot fmt = function
  | Aident  id -> fprintf fmt "%a" print_ident id
  | Aaccess id -> fprintf fmt "@[access@ %a@]" print_ident id


let print_mode fmt = function
  | In    -> fprintf fmt "in"
  | InOut -> fprintf fmt "@[in@ out@]"

let print_param fmt ((id, m, t) : 'a param) =
  fprintf fmt "@[%a@ :@ %a@ %a@]" print_ident id print_mode m print_type_annot t
let print_param_list fmt ps = print_list (print_sep ",@ ") print_param fmt ps

let print_const fmt = function
  | Cint  n -> fprintf fmt "%d" n
  | Cchar c -> fprintf fmt "%c" c
  | Cbool b -> fprintf fmt "%b" b
  | Cnull   -> fprintf fmt "null"

let rec print_expr fmt e =
  match e.desc with
  | Econst c -> fprintf fmt "%a" print_const c
  | Eleft_val lv -> fprintf fmt "%a" print_left_val lv
  | Eunop (u, e) -> fprintf fmt "@[(%a%a)@]" print_unop u print_expr e
  | Ebinop (e1, b, e2) -> fprintf fmt "@[(%a@ %a@ %a)@]" print_expr e1
                            print_binop b print_expr e2
  | Enew id -> fprintf fmt "@[new@ %a@]" print_ident id
  | Eapp_func (id, ps) -> fprintf fmt "@[%a(%a)@]" print_ident id print_expr_list ps
  | Echar_val e -> fprintf fmt "@[character'val(%a)@]" print_expr e
and print_left_val fmt = function
  | Lident id -> fprintf fmt "%a" print_ident id
  | Lmember (e, id) -> fprintf fmt "@[%a.%a@]" print_expr e print_ident id
and print_expr_list fmt es = print_list (print_sep ",@ ") print_expr fmt es

let print_field fmt ((id : 'a ident), t) =
  fprintf fmt "@[%a@ :@ %a@]" print_ident id print_type_annot t
let print_fields fmt fs = print_list (print_sep ";\n") print_field fmt fs

let rec print_stmt fmt s =
  match s.desc with
  | Saffect (lv, e) -> fprintf fmt "@[%a@ :=@ %a;@]" print_left_val lv print_expr e
  | Scall_proc (id, ps) -> fprintf fmt "@[%a(%a);@]" print_ident id print_expr_list ps
  | Sput e -> fprintf fmt "@[put(%a);@]" print_expr e
  | Snew_line -> fprintf fmt "@[new_line;@]"
  | Sreturn e -> fprintf fmt "@[return@ %a;@]" print_expr e
  | Sblock s -> fprintf fmt "@[begin\n%a\nend;@]" print_stmt_list s
  | Scond (e, s1, s2) -> fprintf fmt "@[if@ %a@ then@ %a@ else@ %a@ end@ if;@]"
                           print_expr e print_stmt s1 print_stmt s2
  | Swhile (e, s) -> fprintf fmt "@[while@ %a@ loop@ %a@ end@ loop;@]"
                       print_expr e print_stmt s
  | Sfor (id, b, el, eu, s) -> fprintf fmt
                                 "@[for@ %a@ in@ %a%a@ ..@ %a@ loop@ %a@ end@ loop;@]"
                                 print_ident id (fun fmt b -> if b
                                                  then fprintf fmt "reverse@ "
                                                  else ()) b
                                 print_expr el print_expr eu print_stmt s
and print_stmt_list fmt ss = print_list (print_sep "\n") print_stmt fmt ss

let rec print_decl fmt = function
  | Dtype (id, None) -> fprintf fmt "@[type@ %a;@]" print_ident id
  | Dtype (id, Some t) -> fprintf fmt "@[type@ %a@ is@ %a;@]" print_ident id
                            print_type_annot t
  | Dtype_record (id, fs) -> fprintf fmt
                               "@[type@ %a@ is@ @[record@ %a@ end@ record@];@]"
                               print_ident id print_fields fs
  | Dvar_decl (id, t, None) -> fprintf fmt "@[%a@ :@ %a;@]" print_ident id
                                 print_type_annot t
  | Dvar_decl (id, t, Some e) -> fprintf fmt "@[%a@ :@ %a@ :=@ %a;@]" print_ident id
                                   print_type_annot t print_expr e
  | Dproc p -> fprintf fmt "%a" print_proc p
  | Dfunc f -> fprintf fmt "%a" print_func f
and print_proc fmt p =
  fprintf fmt "@[procedure@ %a(%a)@ is@ @[%a@]@ begin@ @[%a@]@ end;@]"
    print_ident p.name print_param_list p.params print_decl_list p.decls
    print_stmt p.stmt
and print_func fmt f =
  fprintf fmt "@[function@ %a(%a)@ return@ %a@ is@ @[%a@]@ begin@ @[%a@]@ end;@]"
    print_ident f.name print_param_list f.params print_type_annot f.return
    print_decl_list f.decls print_stmt f.stmt
and print_decl_list fmt ds = print_list (print_sep "\n") print_decl fmt ds


let print_ast fmt a =
  print_proc fmt a;
  pp_print_flush fmt ()



(** Decoration printers *)

let print_loc fmt ((s, e) : loc) =
  let open Lexing in
  fprintf fmt "@]File@ \"%s\"@, line@ %d,@ characters@ %d-%d@]"
    s.pos_fname s.pos_lnum (s.pos_cnum - s.pos_bol + 1) (e.pos_cnum - s.pos_bol + 1)


let print_record fmt r =
  match r with
  | Def r -> let print_field fmt (n, t) = fprintf fmt "@[%s:@ %s@]" n t in
    let print_fields fmt l = print_list (print_sep ";@ ") print_field fmt l in
    fprintf fmt "@[%s@ is@ record@ @[%a@]@ end@ record@]"
      r.ident print_fields r.fields
  | Decl s -> fprintf fmt "%s" s

let print_typ fmt = function
  | Tnull -> fprintf fmt "typenull"
  | Tint  -> fprintf fmt "integer"
  | Tchar -> fprintf fmt "character"
  | Tbool -> fprintf fmt "boolean"
  | Trecord r -> print_record fmt r
  | Taccess r -> fprintf fmt "@[access@ %a@]" print_record r
  | Tfunc _ | Tproc _ -> assert false


(** To string printers *)

let print_typ_to_str () = print_to_string print_typ
let print_type_annot_to_str () = print_to_string print_type_annot
