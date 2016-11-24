type ('desc, 'deco) node = {
  desc : 'desc;
  deco : 'deco;
}


(** Nodes of the AST *)

type ident_desc = string  (* only lowercase letters *)
and 'deco ident = (ident_desc, 'deco) node

type unop =
  | Unot
  | Uminus

type binop =
  | Beq | Bneq
  | Blt | Bleq | Bgt | Bgeq
  | Bplus | Bminus | Btimes | Bdiv | Brem
  | Band | Band_then | Bor | Bor_else

type 'deco type_annot = (* type annotated by the user, only the name of the type *)
  | Aident  of 'deco ident
  | Aaccess of 'deco ident

type mode = In | InOut

type 'deco param = 'deco ident * mode * 'deco type_annot
(* Can come from multiple params declared with same type, and default mode is In *)

type const =
  | Cint  of int
  | Cchar of char
  | Cbool of bool
  | Cnull

type 'deco expr_desc = 
  | Econst    of const
  | Eleft_val of 'deco left_val
  | Eunop     of unop * 'deco expr
  | Ebinop    of 'deco expr * binop * 'deco expr
  | Enew      of 'deco ident
  | Eapp_func of 'deco ident * ('deco expr list) (* list is non-empty *)
  | Echar_val of 'deco expr
and 'deco expr = ('deco expr_desc, 'deco) node
                   
and 'deco left_val =
  | Lident  of 'deco ident
  | Lmember of 'deco expr * 'deco ident


type 'deco decl =
  | Dtype        of 'deco ident * 'deco type_annot option
  (* Allows aliasing of types, and can not be initialised *)
  | Dtype_record of 'deco ident * ('deco ident * 'deco type_annot) list
  (* list must be non-empty, multiple fields could have been declared 
     simultaneously *)
  | Dvar_decl    of 'deco ident * 'deco type_annot * 'deco expr option
  (* Can come from a declaration of multiple vars with the same value *)
  | Dproc        of 'deco proc
  | Dfunc        of 'deco func
                       
and ('deco, 'return) proc_func = {
  name   : 'deco ident;
  params : 'deco param list;
  return : 'return;
  decls  : 'deco decl list;
  stmt   : 'deco stmt;
}
and 'deco proc = ('deco, unit) proc_func
and 'deco func = ('deco, 'deco type_annot) proc_func


and 'deco stmt_desc =
  | Saffect    of 'deco left_val * 'deco expr
  | Scall_proc of 'deco ident * ('deco expr list) (* includes empty list *)
  | Sput       of 'deco expr
  | Snew_line
  | Sreturn    of 'deco expr (* includes return without expression *)
  | Sblock     of 'deco stmt list (* includes empty list *)
  | Scond      of 'deco expr * 'deco stmt * 'deco stmt
  (* includes no else and multiple statements *)
  | Swhile     of 'deco expr * 'deco stmt
  | Sfor       of 'deco ident * bool * 'deco expr * 'deco expr * 'deco stmt
  (* counter, reverse, lower_bound, upper_bound, instructions 
     Really tempting to convert in while loop, but would need a new variable, and
     the declarations aren't available during parsing *)
and 'deco stmt = ('deco stmt_desc, 'deco) node


(** Decoration *)

type pos = Lexing.position
type loc = pos * pos (* (start, end) *)

type typ =
  | Tnull
  | Tint
  | Tchar 
  | Tbool
  | Trecord of record
  | Taccess of record
  | Tfunc   of typ list * typ
  | Tproc   of typ list
and record = {
  ident  : ident_desc;
  fields : (ident_desc * ident_desc) list;
}


(** Actual AST *)

type ast       = loc proc
type typed_ast = typ proc
