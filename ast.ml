open Format
open Printer


(** Types idenpendant of decoration outside to allow conversion *)

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


(** The type of a decorated type *)
type ('desc, 'deco) node =
  {
    desc : 'desc;
    deco : 'deco;
  }


(** The signature used to decorate an AST with *)
module type DECO =
sig
  type ident_deco
  type expr_deco
  type stmt_deco
end


(** Functor producing an AST decorated with the given module *)
module DecoratedAST (D : DECO) =
struct
  (** The types of the AST *)

  type ident = (ident_desc, D.ident_deco) node

  type type_annot = (* type annotated by the user, is only the name of the type *)
    | Aident  of ident
    | Aaccess of ident

  type param = ident * mode * type_annot (* Can come from multiple params declared 
                                            with same type, and default mode is In *)

  
  type expr_desc = 
    | Econst    of const
    | Eleft_val of left_val
    | Ebinop    of expr * binop * expr
    | Enot      of expr
    | Enew      of ident
    | Eapp_func of ident * (expr list) (* list is non-empty *)
  and expr = (expr_desc, D.expr_deco) node

  and left_val =
    | Lident  of ident
    | Lmember of expr * ident


  type decl =
    | Dtype        of ident * type_annot option
    (* Allows aliasing of types, and can not be initialised *)
    | Dtype_record of ident * (ident * type_annot) list
    (* list must be non-empty, multiple fields could have been declared 
       simultaneously *)
    | Dvar_decl    of ident * type_annot * expr option
    (* Can come from a declaration of multiple vars with the same value *)
    | Dproc_func   of proc_func

  and proc_func = {
    name   : ident;
    params : param list;
    return : type_annot option; (* being None for a procedure 
                                   and Some typ for a function *)
    decls  : decl list;
    stmt   : stmt;
  }


  and stmt_desc =
    | Saffect    of left_val * expr
    | Scall_proc of ident * (expr list) (* includes empty list *)
    | Sreturn    of expr (* includes return without expression *)
    | Sblock     of stmt list (* includes empty list *)
    | Scond      of expr * stmt * stmt
    (* includes no else and multiple statements *)
    | Swhile     of expr * stmt
    | Sfor       of ident * bool * expr * expr * stmt
    (* counter, reverse, lower_bound, upper_bound, instructions 
       Really tempting to convert in while loop, but would need a new variable, and
       the declarations aren't available during parsing... *)
  and stmt = (stmt_desc, D.stmt_deco) node


  type ast = proc_func


  (** General functions on AST types *)

  let typ_annot_id = function
    | Aident id | Aaccess id -> id
      

  (** AST Printers *)

  let print_ident fmt (id : ident) =
    fprintf fmt "%s" id.desc

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

  let print_param fmt ((id, m, t) : param) =
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
    | Enot e -> fprintf fmt "@[not (%a)@]" print_expr e
    | Ebinop (e1, b, e2) -> fprintf fmt "@[(%a@ %a@ %a)@]" print_expr e1
                              print_binop b print_expr e2
    | Enew id -> fprintf fmt "@[new@ %a@]" print_ident id
    | Eapp_func (id, ps) -> fprintf fmt "@[%a(%a)@]"
                              print_ident id print_expr_list ps
  and print_left_val fmt = function
    | Lident id -> fprintf fmt "%a" print_ident id
    | Lmember (e, id) -> fprintf fmt "@[%a.%a@]" print_expr e print_ident id
  and print_expr_list fmt es = print_list (print_sep ",@ ") print_expr fmt es

  let print_field fmt ((id : ident), t) =
    fprintf fmt "@[%a@ :@ %a@]" print_ident id print_type_annot t
  let print_fields fmt fs = print_list (print_sep ";\n") print_field fmt fs

  let rec print_stmt fmt s =
    match s.desc with
    | Saffect (lv, e) -> fprintf fmt "@[%a@ :=@ %a;@]" print_left_val lv print_expr e
    | Scall_proc (id, ps) -> fprintf fmt "@[%a(%a);@]" print_ident id
                               print_expr_list ps
    | Sreturn e -> fprintf fmt "@[return@ %a;@]" print_expr e
    | Sblock s -> fprintf fmt "@[begin\n%a\nend;@]" print_stmt_list s
    | Scond (e, s1, s2) -> fprintf fmt "@[if@ %a@ then@ %a@ else@ %a@ end@ if;@]"
                             print_expr e print_stmt s1 print_stmt s2
    | Swhile (e, s) -> fprintf fmt "@[while@ %a@ loop@ %a@ end@ loop;@]"
                         print_expr e print_stmt s
    | Sfor (id, b, el, eu, s) ->
      fprintf fmt "@[for@ %a@ in@ %a%a@ ..@ %a@ loop@ %a@ end@ loop;@]"
        print_ident id (fun fmt b -> if b then fprintf fmt "reverse@ " else ()) b
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
    | Dvar_decl (id, t, Some e) -> fprintf fmt "@[%a@ :@ %a@ :=@ %a;@]"
                                     print_ident id print_type_annot t print_expr e
    | Dproc_func pf -> fprintf fmt "%a" print_proc_func pf
  and print_proc_func fmt pf =
    let (name, return_printer) = match pf.return with
      | None ->   ("procedure", (fun _ () -> ()))
      | Some t -> ("function",  (fun fmt () -> fprintf fmt "@ return@ %a"
                                                 print_type_annot t))
    in
    fprintf fmt "@[%s@ %a(%a)%a@ is@ @[%a@]@ @[%a@]@]"
      name print_ident pf.name print_param_list pf.params return_printer ()
      print_decl_list pf.decls print_stmt pf.stmt
  and print_decl_list fmt ds = print_list (print_sep "\n") print_decl fmt ds


  let print_ast fmt a =
    print_proc_func fmt a;
    pp_print_flush fmt ()
end


(** Decoration *)

module Loc =
struct
  type pos = Lexing.position
  type loc = pos * pos (* (start, end) *)

  type ident_deco = loc
  type expr_deco  = loc
  type stmt_deco  = loc

  let print_loc fmt ((s, e) : loc) =
    let open Lexing in
    fprintf fmt "@[File@ \"%s\"@, line@ %d,@ characters %d-%d@]"
      s.pos_fname s.pos_lnum (s.pos_cnum - s.pos_bol + 1)
      (e.pos_cnum - s.pos_bol + 1)
end

module AST = DecoratedAST(Loc)


module Typ =
struct
  type record_def =
    {
      ident  : ident_desc;
      fields : (ident_desc * ident_desc) list; (* list is non-empty *)
    }
  type record =
    | Decl of ident_desc
    | Def  of record_def

  type typ =
    | Tnull
    | Tint
    | Tchar 
    | Tbool
    | Trecord of record
    | Taccess of record
    | Tproc_func of (typ * mode) list * typ (* typ = Tnull for a procedure *)

  type ident_deco = typ
  type expr_deco  = typ
  type stmt_deco  = unit


  let print_record fmt r =
    match r with
    | Def r -> let print_field fmt (n, t) = fprintf fmt "@[%s:@ %s@]" n t in
      let print_fields fmt l = print_list (print_sep "; ") print_field fmt l in
      fprintf fmt "@[%s@ is@ record@ @[%a@]@ end@ record@]"
        r.ident print_fields r.fields
    | Decl s -> fprintf fmt "%s" s
                  
  let print_typ fmt t =
    match t with
    | Tnull -> fprintf fmt "typenull"
    | Tint  -> fprintf fmt "integer"
    | Tchar -> fprintf fmt "character"
    | Tbool -> fprintf fmt "boolean"
    | Trecord r -> print_record fmt r
    | Taccess r -> fprintf fmt "@[access@ %a@]" print_record r
    | Tproc_func _ -> assert false
end

module TypedAST = DecoratedAST(Typ)
