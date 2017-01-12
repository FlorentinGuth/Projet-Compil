open Utils
open Ast_common


(** Decoration types *)

type level = int
type t_ident = (ident_desc, level) node
    

type typ_def =
  | Tnull
  | Tint
  | Tchar
  | Tbool
  | Trecord of t_ident annotated list (* list is non-empty *)
  | Taccess of t_ident
  | Tproc_func of (typ * mode) list * typ (* typ = Tnull for a procedure *)
and typ =
  {
    t_ident : t_ident;
    def     : typ_def;
  }

let null = { t_ident = decorate "typenull"  0; def = Tnull }
let int  = { t_ident = decorate "integer"   0; def = Tint  }
let char = { t_ident = decorate "character" 0; def = Tchar }
let bool = { t_ident = decorate "boolean"   0; def = Tbool }

let get_level t =
  t.t_ident.deco

let is_access t =
  match t.def with
  | Taccess _ -> true
  | _ -> false

let to_access t =
  { t_ident = decorate "" t.t_ident.deco; def = Taccess t.t_ident }

let reduce_pf t =
  match t.def with
  | Tproc_func ([], r) -> r
  | _ -> t


type ident_deco =
  {
    typ: typ; 
    level: level;
    offset: int;   (* offset according to %rbp *)
  }


(** AST *)

type ident = (ident_desc, ident_deco) node
type ident_decl = ident
(* Not same types because it makes sense to decorate with loc but not with a type *)


type nonrec typ_annot = ident_decl typ_annot
type nonrec annotated = ident_decl annotated


type expr_desc = 
  | Econst    of const
  | Eleft_val of left_val
  | Ebinop    of expr * binop * expr
  | Enot      of expr
  | Enew      of ident_decl
  | Eapp_func of ident * (expr list) (* list is non-empty *)
and expr = (expr_desc, typ) node

and left_val =
  | Lident  of ident
  | Lmember of expr * ident_decl



type param = annotated * mode (* Can come from multiple params declared 
                                 with same type, and default mode is In *)

type stmt =
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


type decl =
  | Dtype_decl  of ident_decl * typ_annot option
  (* Allows aliasing of types *)
  | Drecord_def of ident_decl * annotated list
  (* list must be non-empty, multiple fields could have been declared 
     simultaneously *)
  | Dvar_decl   of annotated * expr option
  (* Can come from a declaration of multiple vars with the same value *)
  | Dproc_func  of proc_func

and proc_func =
  {
    name   : ident_decl;
    params : param list;
    return : typ_annot; (* being null (empty typ_ident) for a procedure *)
    decls  : decl list;
    stmt   : stmt;
  }


type ast = proc_func


let map_ta f ta =
  { ta with typ_ident = f ta.typ_ident }
let map_a f a =
  { ident = f a.ident; ta = map_ta f a.ta }



open Format
open Printer

let print_t_ident fmt id =
  fprintf fmt "%s[%d]" id.desc id.deco

let print_ta fmt ta =
  fprintf fmt "@[%a%a@]" print_if ("access ", ta.is_access)
    print_t_ident ta.typ_ident 
let print_a fmt a =
  fprintf fmt "@[%a: %a@]" print_t_ident a.ident print_ta a.ta
let print_record fmt r =
  let print_fields fmt l = print_list (print_sep "; ") print_a fmt l in
  fprintf fmt "@[record@ @[%a@]@ end@ record@]" print_fields r

let rec print_typ_spec fmt t =
  match t with
  | Tnull -> fprintf fmt "typenull[0]"
  | Tint  -> fprintf fmt "integer[0]"
  | Tchar -> fprintf fmt "character[0]"
  | Tbool -> fprintf fmt "boolean[0]"
  | Trecord r -> print_record fmt r
  | Taccess s -> fprintf fmt "@[access@ %a@]" print_t_ident s
  | Tproc_func (ps, r) -> let print_param fmt (t, _) = print_typ fmt t in
    let print_params fmt ps = print_list (print_sep " -> ") print_param fmt ps in
    fprintf fmt "%a -> %a" print_params ps print_typ r
and print_typ fmt t =
  fprintf fmt "%a is %a" print_t_ident t.t_ident print_typ_spec t.def
