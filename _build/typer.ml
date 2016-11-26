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
  | Trecord r -> Taccess r
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


let type_type_annot env ta =
  let type_typ typ =
    (replace_deco typ (Env.get_typ_annot_type env typ) : T.ident)
  in
  match ta with
  | Aident id -> let id' = type_typ id in
    (T.Aident id', id'.deco)
  | Aaccess id -> let id' = type_typ id in
    (T.Aaccess id', ensure_record id'.deco id.deco)


let type_const = function
  | Cint  _ -> Tint
  | Cchar _ -> Tchar
  | Cbool _ -> Tbool
  | Cnull   -> Tnull

let type_field env r (f : ident) =
  try
    let t_name = List.assoc f.desc r.fields in
    Env.get_typ_annot_type env (decorate t_name f.deco)
  with
  | Not_found -> error
                   (Format.sprintf "Record %s has no field %s" r.ident f.desc) f.deco


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
  | Enew id -> let (id', t) = type_type_annot env (Aident id) in
    let t' = ensure_record t id.deco in
    decorate (T.Enew (T.typ_annot_id id')) t'
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
      match e'.deco with
      | Trecord r | Taccess r ->
        let t = type_field env (ensure_defined r e.deco) f in
        if ensure_lv && not (is_access e'.deco)
        then ignore(ensure_left_val env e);
        (T.Lmember (e', replace_deco f t), t)
      | _ -> error (Format.sprintf "This expression should be a record or an access")
               e.deco
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

let ensure_well_formed env = function
  | Aident id ->
    begin
      match Env.get_typ_annot_type env id with
      | Trecord r -> Trecord (Def (ensure_defined r id.deco))
      | t -> t
    end
  | Aaccess id -> ensure_record (Env.get_typ_annot_type env id) id.deco


let rec type_decl env = function
  | Dtype (id, None) -> Env.set_typ_annot_type env id.desc
                          (Trecord (Decl id.desc))
  | Dtype (_, Some (Aident _)) -> assert false
  | Dtype (id, Some (Aaccess r)) ->
    let t = Env.get_typ_annot_type env r in (* ensures r is declared *)
    let t' = ensure_record t r.deco in
    Env.set_typ_annot_type env id.desc t'
  | Dtype_record (id, fields) ->
    List.iter (fun (_, t) ->
                ignore (ensure_well_formed
                          (Env.set_typ_annot_type env id.desc
                             (Trecord (Decl id.desc))) t);)
      fields;
    let fields' = List.map (fun (id, t) -> (id.desc, (typ_annot_id t).desc))
                    fields in
    Env.set_typ_annot_type env id.desc
      (Trecord (Def { ident = id.desc; fields = fields' }))
  | Dvar_decl (id, t, None) -> let typ = ensure_well_formed env t in
    Env.set_id_type env id.desc typ
  | Dvar_decl (id, t, Some e) -> let typ = ensure_well_formed env t in
    let e' = type_expr env e in
    ensure_equiv typ e'.deco e.deco;
    Env.set_id_type env id.desc typ
  | Dproc_func pf ->
    let return' = match pf.return with
      | None -> Tnull
      | Some r -> ensure_well_formed env r in
    let params' = List.map (fun (id, m, t) ->
                             let typ = ensure_well_formed env t in
                             (id, m, typ))
                    pf.params in
    let env' = Env.set_id_type env pf.name.desc
                 (Tproc_func (List.map
                                (fun (_, m, t) -> (t, m)) params', return')) in
    let env'' = List.fold_left (fun env (id, _, t) ->
                                 Env.set_id_type env id.desc t) env' params' in
    let env'' = type_decls env'' pf.decls in
    let stmt' = type_stmt env'' return' [] pf.stmt in
    env'

and type_decls env = function
  | [] -> env
  | d :: ds -> type_decls (type_decl env d) ds


(** Typing of a program *)

let type_ast a =
  ignore (type_decls Env.new_env [Dproc_func a])
