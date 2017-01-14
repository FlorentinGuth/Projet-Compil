type ident_desc = string (* only lowercase letters, can be "put", "new_line", 
                            "character'val"... *)

type binop =
  | Beq | Bneq
  | Blt | Bleq | Bgt | Bgeq
  | Bplus | Bminus | Btimes | Bdiv | Brem
  | Band | Band_then | Bor | Bor_else

type const =
  | Cint  of int
  | Cchar of char
  | Cbool of bool
  | Cnull

type mode = In | InOut


type 'ident typ_annot = (* Type annotated by the user *)
  {
    typ_ident: 'ident;
    is_access: bool;
  }
type 'ident annotated =
  {
    ident: 'ident;
    ta   : 'ident typ_annot;
  }

let map_ta f ta =
  { ta with typ_ident = f ta.typ_ident }
let map_a f a =
  { ident = f a.ident; ta = map_ta f a.ta }


open Format
open Printer


let print_ident_desc fmt (id : ident_desc) =
  fprintf fmt "%s" id

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

let print_const fmt = function
  | Cint  n -> fprintf fmt "%d" n
  | Cchar c -> fprintf fmt "%c" c
  | Cbool b -> fprintf fmt "%b" b
  | Cnull   -> fprintf fmt "null"

let print_mode fmt = function
  | In    -> fprintf fmt "in"
  | InOut -> fprintf fmt "in out"
