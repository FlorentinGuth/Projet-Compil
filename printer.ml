open Format


(** General use printers *)

let print_option printer fmt (prec, opt) =
  match opt with
  | None -> ()
  | Some x -> fprintf fmt "%s%a" prec printer x

let print_if fmt (s, b) =
  if b then fprintf fmt "%s" s

let print_list print_sep print_elt =
  let rec printer fmt = function
    | [] -> ()
    | [x] -> fprintf fmt "%a" print_elt x
    | x :: xs -> fprintf fmt "%a%a%a" print_elt x print_sep () printer xs
  in printer

let print_sep s fmt () =
  fprintf fmt "%s" s

let print_to_string printer arg =
  printer Format.str_formatter arg;
  Format.flush_str_formatter ()


(*
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
*)
