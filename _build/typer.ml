open Ast
open Loc
open Typ
open Utils
open Printer

module A = AST
open A
module T = TypedAST

(** General error functions *)

let error msg loc =
  raise (Typing_error (msg, loc))
    

let ensure_record typ loc =
  match typ with
  | Trecord r -> r
  | _ -> error "Only a record can have an access type" loc

let ensure_defined r loc =
  match r with
  | Decl s -> error (Format.sprintf "Record %s has not been defined yet" s) loc
  | Def r -> r

let wrong_type t_exp t_proc loc =
  let print_typ_to_str = print_to_string print_typ in
  error (Format.sprintf
           "This expression has type %s but was expected of type %s"
           (print_typ_to_str t_proc) (print_typ_to_str t_exp)) loc


let ensure_equal t1 t2 loc2 =
  if t1 <> t2 then wrong_type t1 t2 loc2

let ensure_equiv t1 t2 loc2 =
  match (t1, t2) with
  | (Tnull, Taccess _) | (Taccess _, Tnull) -> ()
  | _ -> ensure_equal t1 t2 loc2


let is_access = function
  | Taccess _ -> true
  | _ -> false

(** The environment module *)

module Env : sig
  type t

  val new_env : t

  val get_id_type : t -> ident -> typ
  val get_typ_annot_type : t -> ident -> typ

  val set_id_type : t -> ident_desc -> typ -> t
  val set_typ_annot_type : t -> ident_desc -> typ -> t
end = struct
  module Smap = Map.Make(String)

  type map = typ Smap.t (* maps an identifier with its type *)
  type t = map * map



  let add_in (ts, r) = Tproc_func (List.map (fun t -> (t, In)) ts, r)
  let reserved = [("character'val", add_in ([Tint],  Tchar));
                  ("put",           add_in ([Tchar], Tnull));
                  ("new_line",      add_in ([],      Tnull));
                 ]

  let unops    = [("not",           add_in ([Tbool], Tbool))] 
  
  let type_binop = function
    | Beq | Bneq ->                            assert false
    | Blt | Bleq | Bgt | Bgeq ->               ([Tint;  Tint],  Tbool)
    | Bplus | Bminus | Btimes | Bdiv | Brem -> ([Tint;  Tint],  Tint)
    | Band | Band_then | Bor | Bor_else ->     ([Tbool; Tbool], Tbool)
  let binops = List.map (fun b -> (print_to_string print_binop b,
                                   add_in (type_binop b)))
                 [Blt; Bleq; Bgt; Bgeq; Bplus; Bminus; Btimes; Bdiv; Brem;
                  Band; Band_then; Bor; Bor_else]
                 
  let new_env = (Smap.of_list (reserved @ unops @ binops),
                 Smap.of_list [("integer",       Tint);
                               ("character",     Tchar);
                               ("boolean",       Tbool);
                              ])


  let search_map map key name =
    try
      Smap.find key.desc map
    with
    | Not_found -> error (Format.sprintf "Unknown %s: %s" name key.desc) key.deco

  let get_id_type (id_env, _) id =
    search_map id_env id "identifier"

  let get_typ_annot_type (_, typ_env) typ =
    search_map typ_env typ "type"


  let set_id_type (id_env, typ_env) id typ =
    (Smap.add id typ id_env, typ_env)

  let set_typ_annot_type (id_env, typ_env) typ_annot typ =
    (id_env, Smap.add typ_annot typ typ_env)
end


(** Typing of expressions *)

let type_ident env id =
  (replace_deco id (Env.get_id_type env id) : T.ident)


let type_typ_annot env (ta : A.typ_annot) =
  (({ typ_ident = replace_deco ta.typ_ident ();
      is_access = ta.is_access } : T.typ_annot),
   Env.get_typ_annot_type env ta.typ_ident)

let type_annotated env (a : A.annotated) =
  let (ta', t) = type_typ_annot env a.ta in
  (({ ident = replace_deco a.ident ();
      ta    = ta' } : T.annotated), t)

let type_const = function
  | Cint  _ -> Tint
  | Cchar _ -> Tchar
  | Cbool _ -> Tbool
  | Cnull   -> Tnull

let type_field env r (f : ident_decl) =
  try
    let a = List.find (fun a -> f.desc = a.ident) r.fields in
    Env.get_typ_annot_type env (decorate a.ta.typ_ident f.deco)
  with
  | Not_found -> error
                   (Format.sprintf "Record %s has no field %s" r.r_ident f.desc)
                   f.deco


let rec type_expr env expr =
  match expr.desc with
  | Econst c -> decorate (T.Econst c) (type_const c)
  | Eleft_val lv -> let (lv', t) = type_left_val env lv in
    decorate (T.Eleft_val lv') t
  | Enot e ->
    let ([e'], t) = type_proc_func env
                      (decorate "not" expr.deco) [e] in
    decorate (T.Enot e') t
  | Ebinop (e1, b, e2) ->
    begin
      match b with
      | Beq | Bneq -> let e1' = type_expr env e1 in
        let e2' = type_expr env e2 in
        ensure_equiv e1'.deco e2'.deco e2.deco;
        decorate (T.Ebinop (e1', b, e2')) Tbool
      | _ ->  let ([e1'; e2'], t) =
                type_proc_func env
                  (decorate (print_to_string print_binop b) expr.deco)
                  [e1; e2] in
        decorate (T.Ebinop (e1', b, e2')) t
    end
  | Enew id -> let t = Env.get_typ_annot_type env id in
    let t' = to_access (ensure_record t id.deco) in
    decorate (T.Enew (replace_deco id ())) t'
  | Eapp_func (f, params) -> 
    let (params', t) = type_proc_func env f params in
    decorate (T.Eapp_func (type_ident env f, params')) t
      

and type_left_val ?(ensure_lv = false) env lv =
  let not_left_val loc = error "This expression should be a left value" loc in
  match lv with
  | Lident id -> let id' = type_ident env id in
    let t = match id'.deco with
      | Tproc_func ([], t) -> if ensure_lv then not_left_val id.deco else t
      | t -> t
    in
    (T.Lident (replace_deco id' t), t)
  | Lmember (e, f) ->
    begin
      let e' = type_expr env e in
      let r = match e'.deco with
        | Trecord r -> r
        | Taccess r -> ensure_record
                         (Env.get_typ_annot_type env (decorate r e.deco)) e.deco
        | _ -> error
                 (Format.sprintf "This expression should be a record or an access")
                 e.deco
      in
      let t = type_field env (ensure_defined r e.deco) f in
      if ensure_lv && not (is_access e'.deco)
      then ignore(ensure_left_val env e);
      (T.Lmember (e', replace_deco f t), t)
    end

and ensure_left_val env e =
  match e.desc with
  | Eleft_val lv -> let (lv', t) = type_left_val ~ensure_lv:true env lv in
    decorate (T.Eleft_val lv') t
  | _ -> error "This expression should be a left value" e.deco
           

and type_proc_func ?(expr = true) env name params =
  let typ = Env.get_id_type env name in
  match (typ, expr) with
  | (Tproc_func (_, Tnull), true) ->
    error "A procedure cannot be called in an expression" name.deco
  | (Tproc_func (_, t), false) when t <> Tnull ->
    error "A function cannot be applied in a statement" name.deco
  | (Tproc_func (l, r), _) ->
    begin
      try
        let params' = List.map2
                        (fun (t, m) e -> let type_func env e = match m with
                                            | In -> type_expr env e
                                            | InOut -> ensure_left_val env e
                           in
                           let e' = type_func env e in
                           ensure_equiv t e'.deco e.deco;
                           e') l params in
        (params', r)
      with
      | Invalid_argument _ (* "List.map2" *) ->
        error (Format.sprintf "%s expects %d arguments but is given %d here"
                 name.desc (List.length l) (List.length params)) name.deco
    end
  | _ -> let (fp, v) = if expr then ("function", "applied")
           else ("procedure", "called") in
    error (Format.sprintf "%s is not a %s, it cannot be %s" name.desc fp v) name.deco


(** Typing of statements *)

let rec desc_type_stmt env return_type const stmt =
  match stmt.desc with
  | Saffect (lv, e) -> let (lv', t) = type_left_val env lv in
    let e' = type_expr env e in
    ensure_equiv t e'.deco e.deco;
    T.Saffect (lv', e')
  | Scall_proc (p, params) ->
    let (params', _) = type_proc_func ~expr:false env p params in
    T.Scall_proc (type_ident env p, params')
  | Sreturn e -> let e' = type_expr env e in
    let ensure = if return_type = Tnull then ensure_equal else ensure_equiv in
    ensure return_type e'.deco e.deco;
    T.Sreturn e'
  | Sblock l -> T.Sblock (List.map (type_stmt env return_type const) l)
  | Scond (e, s1, s2) -> let e' = type_expr env e in
    ensure_equal Tbool e'.deco e.deco;
    let s1' = type_stmt env return_type const s1 in
    let s2' = type_stmt env return_type const s2 in
    T.Scond (e', s1', s2')
  | Swhile (e, s) -> let e' = type_expr env e in
    ensure_equal Tbool e'.deco e.deco;
    let s' = type_stmt env return_type const s in
    T.Swhile (e', s')
  | Sfor (id, rev, lb, ub, s) -> let id' = type_ident env id in
    let lb' = type_expr env lb in
    let ub' = type_expr env ub in
    ensure_equal Tint lb'.deco lb.deco;
    ensure_equal Tint ub'.deco ub.deco;
    let s' = type_stmt (Env.set_id_type env id.desc id'.deco) return_type const s in
    T.Sfor (id', rev, lb', ub', s')
    
and type_stmt env return_type const stmt =
  decorate (desc_type_stmt env return_type const stmt) ()


(** Typing of declarations *)

let ensure_well_formed env ta =
  if ta.is_access
  then to_access (ensure_record (Env.get_typ_annot_type env ta.typ_ident)
                    ta.typ_ident.deco)
  else match Env.get_typ_annot_type env ta.typ_ident with
    | Trecord r -> Trecord (Def (ensure_defined r ta.typ_ident.deco))
    | t -> t


let rec type_decl env = function
  | Dtype_decl (id, None) -> let env' = Env.set_typ_annot_type env id.desc
                                          (Trecord (Decl id.desc)) in
    (T.Dtype_decl (replace_deco id (), None), env')
  | Dtype_decl (id, Some ta) -> (* ta.is_access = true because no aliasing *)
    let t = Env.get_typ_annot_type env ta.typ_ident in (* ensures r is declared *)
    let t' = ensure_record t ta.typ_ident.deco in
    let env' = Env.set_typ_annot_type env id.desc (to_access t') in
    (T.Dtype_decl (replace_deco id (),
                   Some { typ_ident = replace_deco ta.typ_ident ();
                          is_access = true }), env')
  | Drecord_def (id, fields) ->
    List.iter (fun a ->
                ignore (ensure_well_formed
                          (Env.set_typ_annot_type env id.desc
                             (Trecord (Decl id.desc))) a.ta);)
      fields;
    let fields' = List.map (fun a -> { ident = replace_deco id ();
                                       ta = fst (type_typ_annot env a.ta) })
                    fields in
    let env' = Env.set_typ_annot_type env id.desc
                 (Trecord (Def { r_ident = id.desc;
                                 fields = List.map strip_deco_a fields' })) in
    (T.Drecord_def (replace_deco id (), fields'), env')
  | Dvar_decl (a, None) -> let typ = ensure_well_formed env a.ta in
    let env' = Env.set_id_type env a.ident.desc typ in
    (T.Dvar_decl (fst (type_annotated env' a), None), env')
  | Dvar_decl (a, Some e) -> let typ = ensure_well_formed env a.ta in
    let e' = type_expr env e in
    ensure_equiv typ e'.deco e.deco;
    let env' = Env.set_id_type env a.ident.desc typ in
    (T.Dvar_decl (fst (type_annotated env' a), Some e'), env')
  | Dproc_func pf ->
    let (return', ret) = match pf.return with
      | None -> (Tnull, None)
      | Some ta -> (ensure_well_formed env ta, Some (fst (type_typ_annot env ta))) in
    let params' = List.map (fun (a, m) ->
                             let typ = ensure_well_formed env a.ta in (* needed? *)
                             (type_annotated env a, m))
                    pf.params in
    let env' = Env.set_id_type env pf.name.desc
                 (Tproc_func (List.map
                                (fun ((a, t), m) -> (t, m)) params', return')) in
    let env'' = List.fold_left (fun env ((a, t), m) ->
                                 Env.set_id_type env a.ident.desc t) env' params' in
    let (decls', env'') = type_decls env'' pf.decls in
    let stmt' = type_stmt env'' return' [] pf.stmt in
    let pf' = T.{ name = replace_deco pf.name ();
                  params = List.map (fun ((a, _), m) -> (a, m)) params';
                  return = ret;
                  decls = decls'; (* embed environment? *)
                  stmt = stmt' } in
    (T.Dproc_func pf', env')

and type_decls env = function
  | [] -> ([], env)
  | d :: ds -> let (d', env') = type_decl env d in
    let (ds', env'') = type_decls env' ds in
    (d' :: ds', env'')


(** Typing of a program *)

let type_ast (a : A.ast) =
  match fst (type_decls Env.new_env [Dproc_func a]) with
  | [T.Dproc_func a'] -> (a' : T.ast)
  | _ -> assert false
