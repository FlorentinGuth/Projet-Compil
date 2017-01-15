open Utils
open Ast_common


(** Decoration types *)

type t_ident = (ident_desc, int) node (* level *)
    

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
    t_ident: t_ident;
    def:     typ_def;
    t_size:  int;     (* All sizes are in quads, i.e. 8 bytes *)
  }

let null = { t_ident = decorate "typenull"  0; def = Tnull; t_size = 1 }
let int  = { t_ident = decorate "integer"   0; def = Tint;  t_size = 1 }
let char = { t_ident = decorate "character" 0; def = Tchar; t_size = 1 }
let bool = { t_ident = decorate "boolean"   0; def = Tbool; t_size = 1 }

let get_level t =
  t.t_ident.deco

let is_access t =
  match t.def with
  | Taccess _ -> true
  | _ -> false

let to_access t =
  { t_ident = decorate "" t.t_ident.deco; def = Taccess t.t_ident; t_size = 1 }

let reduce_pf t =
  match t.def with
  | Tproc_func ([], r) -> r
  | _ -> t



(** AST *)

type ident =
  {
    is_reference: bool;
    size:   int;
    level:  int;     (* directly difference from current lvl to ident lvl *)
    offset: int;     (* offset according to %rbp *)
  }

type pf_sig =
  {
    name: ident_desc;
    modes: mode list;
    level:    int;
    size_ret: int;
  }

type expr_desc = 
  | Econst    of const
  | Ederef    of expr
  | Eident    of ident
  | Emember   of expr * int (* address of field *)
  | Ebinop    of expr * binop * expr
  | Enot      of expr
  | Enew      of int (* size *)
  | Eapp_func of pf_sig * (expr list) (* list is non-empty *)
and expr = (expr_desc, int) node (* decorated with size *)



type stmt =
  | Saffect    of expr (* assured to be a left_val *) * expr
  | Scall_proc of pf_sig * (expr list) (* includes empty list *)
  | Sreturn    of expr (* includes return without expression *)
  | Sblock     of stmt list (* includes empty list *)
  | Scond      of expr * stmt * stmt (* includes no else and multiple statements *)
  | Swhile     of expr * stmt

type decl =
  | Dvar_decl   of int (* offset *) * expr
  | Dproc_func  of proc_func

and proc_func =
  {
    pf_sig:  pf_sig;
    frame:   int;
    ret:     int;
    decls:   decl list;
    stmt:    stmt;
  }


type ast = proc_func


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
  fprintf fmt "%a{%d} is %a"
    print_t_ident t.t_ident t.t_size print_typ_spec t.def


let print_ident fmt id =
  fprintf fmt "%a%d(RBP)[%d]{%d}" print_if ("&", id.is_reference)
    id.offset id.level id.size
    
let print_ident_decl fmt id =
  fprintf fmt "%s" id

let print_pf_sig fmt pf_sig =
  let print_modes = print_list (print_sep ", ") print_mode in
  fprintf fmt "%a[%d] (%a) RETURNS{%d}" print_ident_decl pf_sig.name
    pf_sig.level print_modes pf_sig.modes pf_sig.size_ret


let rec print_expr_desc fmt = function
  | Econst c -> fprintf fmt "C%a" print_const c
  | Eident id -> print_ident fmt id
  | Emember (e, o) -> fprintf fmt "%a.(%d)" print_expr e o
  | Ederef e -> fprintf fmt "%a*" print_expr e
  | Ebinop (e1, b, e2) -> fprintf fmt "%a %a %a" print_expr e1 print_binop b
                            print_expr e2
  | Enot e -> fprintf fmt "NOT (%a)" print_expr e
  | Enew n -> fprintf fmt "NEW (%d)" n
  | Eapp_func (pf, el) -> fprintf fmt "%a (%a)" print_pf_sig pf print_exprs el
and print_expr fmt e =
  fprintf fmt "%a{%d}" print_expr_desc e.desc e.deco

and print_exprs fmt = print_list (print_sep ", ") print_expr fmt


let rec print_stmt fmt = function
  | Saffect (lv, e) -> fprintf fmt "%a := %a;" print_expr lv print_expr e
  | Scall_proc (pf, el) -> fprintf fmt "%a (%a);" print_pf_sig pf print_exprs el
  | Sreturn e -> fprintf fmt "RET %a;" print_expr e
  | Sblock b -> let print_stmts = print_list (print_sep "\n") print_stmt in
    fprintf fmt "BEGIN\n@[%a@]\nEND;" print_stmts b
  | Scond (e, s1, s2) -> fprintf fmt "IF %a THEN %a ELSE %a END IF;"
                           print_expr e print_stmt s1 print_stmt s2
  | Swhile (e, s) -> fprintf fmt "WHILE %a LOOP %a END LOOP;"
                       print_expr e print_stmt s

let print_return fmt r =
  if r = 0 then fprintf fmt "RAX" else fprintf fmt "%d(RBP)" r

let rec print_decl fmt = function
  | Dvar_decl (n, e) -> fprintf fmt "%d(RBP) := %a;" n print_expr e
  | Dproc_func pf -> print_proc_func fmt pf

and print_proc_func fmt pf =
  let print_decls = print_list (print_sep "\n") print_decl in
  fprintf fmt "FUNCTION %a FRAME{%d} RET_AT %a\n%a%a" print_pf_sig pf.pf_sig
    pf.frame print_return pf.ret print_decls pf.decls print_stmt pf.stmt

let print_ast = print_proc_func
