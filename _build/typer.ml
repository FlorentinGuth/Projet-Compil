open Ast
open Utils
open Printer


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
  error (Format.sprintf
           "This expression has type %a but was expected of type %a"
           print_typ_to_str t_proc print_typ_to_str t_exp) loc


let ensure_equal t1 t2 loc2 =
  if t1 <> t2 then wrong_type t1 t2 loc2

let ensure_equiv t1 t2 loc2 =
  match (t1, t2) with
  | (Tnull, Taccess _) | (Taccess _, Tnull) -> ()
  | _ -> ensure_equal t1 t2 loc2


let typ_annot_id = function
  | Aident id | Aaccess id -> id

(** The environment module *)

module Env : sig
  type t

  val new_env : t

  val get_id_type : t -> loc ident -> typ
  val get_typ_annot_type : t -> loc ident -> typ

  val set_id_type : t -> ident_desc -> typ -> t
  val set_typ_annot_type : t -> ident_desc -> typ -> t
end = struct
  module Smap = Map.Make(String)

  type map = typ Smap.t (* maps an identifier with its type *)
  type t = map * map
  let new_env = (Smap.empty,
                 Smap.of_list [("integer",   Tint);
                               ("character", Tchar);
                               ("boolean",   Tbool);])


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
  (replace_deco id (Env.get_id_type env id) : typ ident)


let type_unop = function
  | Unot  -> Tfunc ([(Tbool, In)], Tbool)
  | Uminus-> Tfunc ([(Tint, In)],  Tint)

let type_binop = function
  | Beq | Bneq -> assert false
  | Blt | Bleq | Bgt | Bgeq -> Tfunc ([(Tint, In); (Tint, In)], Tbool)
  | Bplus | Bminus | Btimes | Bdiv | Brem ->
    Tfunc ([(Tint, In); (Tint, In)], Tint)
  | Band | Band_then | Bor | Bor_else ->
    Tfunc ([(Tbool, In); (Tbool, In)], Tbool)


let type_type_annot env ta =
  let type_typ typ =
    (replace_deco typ (Env.get_typ_annot_type env typ) : typ ident)
  in
  match ta with
  | Aident id -> let id' = type_typ id in
    (Aident id', id'.deco)
  | Aaccess id -> let id' = type_typ id in
    (Aaccess id', ensure_record id'.deco id.deco)


let type_const = function
  | Cint  _ -> Tint
  | Cchar _ -> Tchar
  | Cbool _ -> Tbool
  | Cnull   -> Tnull

let type_field env r (f : loc ident) =
  try
    let t_name = List.assoc f.desc r.fields in
    Env.get_typ_annot_type env (decorate t_name f.deco)
  with
  | Not_found -> error
                   (Format.sprintf "Record %s has no field %s" r.ident f.desc) f.deco


let rec type_expr env expr =
  match expr.desc with
  | Econst c -> decorate (Econst c) (type_const c)
  | Eleft_val lv -> let (lv', t) = type_leftval env lv in
    decorate (Eleft_val lv') t
  | Eunop (u, e) ->
    begin
      match type_app_func env (print_to_string print_unop u) (type_unop u)
              expr.deco [e] with
      | ([e'], t) -> decorate (Eunop (u, e')) t
      | _ -> assert false
    end
  | Ebinop (e1, o, e2) ->
    begin
      match o with
      | Beq | Bneq -> let e1' = type_expr env e1 in
        let e2' = type_expr env e2 in
        ensure_equiv e1'.deco e2'.deco e2.deco;
        decorate (Ebinop (e1', o, e2')) Tbool
      | _ ->
        begin
          match type_app_func env (print_to_string print_binop o) (type_binop o)
                  expr.deco [e1; e2] with
          | ([e1'; e2'], t) -> decorate (Ebinop (e1', o, e2')) t
          | _ -> assert false
        end
    end
  | Enew id -> let (id', t) = type_type_annot env (Aident id) in
    let t' = ensure_record t id.deco in
    decorate (Enew (typ_annot_id id')) t'
  | Eapp_func (f, params) -> let f' = type_ident env f in
    let (params', t) = type_app_func env f'.desc f'.deco f.deco params in
    decorate (Eapp_func (f', params')) t
  | Echar_val e ->
    begin
      match type_app_func env "character'val" (Tfunc ([(Tchar, In)], Tint))
              expr.deco [e] with
      | ([e'], t) -> decorate (Echar_val e') t
      | _ -> assert false
    end
      

and type_leftval env = function
  | Lident id -> let id' = type_ident env id in
    let t = match id'.deco with
      | Tfunc ([], t) | t -> t (* cannot be a function here *)
    in
    (Lident (replace_deco id' t), t)
  | Lmember (e, f) ->
    begin
      let e' = type_expr env e in
      match e'.deco with
      | Trecord r | Taccess r ->
        let t = type_field env (ensure_defined r e.deco) f in
        (Lmember (e', replace_deco f t), t)
      | _ -> error (Format.sprintf "This expression should be a record or an access")
               e.deco
    end

and type_app_func env f_name f_type f_loc params =
  match f_type with
    | Tfunc (l, r) ->
      begin
        try
          let params' = List.map2 (fun (t, _) e -> let e' = type_expr env e in
                                    ensure_equiv t e'.deco e.deco;
                                    e') l params in
          (params', r)
        with
        | Invalid_argument "List.map2" ->
          error (Format.sprintf "%s expects %d arguments but is given %d here" f_name
                   (List.length l) (List.length params)) f_loc
      end
    | Tproc _ -> error "A procedure cannot be called in an expression" f_loc
    | _ -> error (Format.sprintf "%s is not a function, it cannot be applied" f_name)
             f_loc


(** Typing of statements *)

let type_call_proc env p_name p_type p_loc params =
  match p_type with
    | Tproc l ->
      begin
        try
          List.map2 (fun (t, _) e -> let e' = type_expr env e in
                      ensure_equiv t e'.deco e.deco;
                      e') l params
        with
        | Invalid_argument "List.map2" ->
          error (Format.sprintf "%s expects %d arguments but is given %d here" p_name
                   (List.length l) (List.length params)) p_loc
      end
    | Tfunc _ -> error "A function cannot be called in a statement" p_loc
    | _ -> error (Format.sprintf "%s is not a procedure, it cannot be called" p_name)
             p_loc

let rec desc_type_stmt env return_type stmt =
  match stmt.desc with
  | Saffect (lv, e) -> let (lv', t) = type_leftval env lv in
    let e' = type_expr env e in
    ensure_equiv t e'.deco e.deco;
    Saffect (lv', e')
  | Scall_proc (p, params) -> let p' = type_ident env p in
    let params' = type_call_proc env p.desc p'.deco p.deco params in
    Scall_proc (p', params')
  | Sput e ->
    begin
      match type_call_proc env "put" (Tproc [(Tchar, In)]) stmt.deco [e] with
      | [e'] -> Sput e'
      | _ -> assert false
    end
  | Snew_line -> Snew_line
  | Sreturn e -> let e' = type_expr env e in
    let ensure = if return_type = Tnull then ensure_equal else ensure_equiv in
    ensure return_type e'.deco e.deco;
    Sreturn e'
  | Sblock l -> Sblock (List.map (type_stmt env return_type) l)
  | Scond (e, s1, s2) -> let e' = type_expr env e in
    ensure_equal Tbool e'.deco e.deco;
    let s1' = type_stmt env return_type s1 in
    let s2' = type_stmt env return_type s2 in
    Scond (e', s1', s2')
  | Swhile (e, s) -> let e' = type_expr env e in
    ensure_equal Tbool e'.deco e.deco;
    let s' = type_stmt env return_type s in
    Swhile (e', s')
  | Sfor (id, rev, lb, ub, s) -> let id' = type_ident env id in
    let lb' = type_expr env lb in
    let ub' = type_expr env ub in
    ensure_equal Tint lb'.deco lb.deco;
    ensure_equal Tint ub'.deco ub.deco;
    let s' = type_stmt (Env.set_id_type env id.desc id'.deco) return_type s in
    Sfor (id', rev, lb', ub', s')
    
and type_stmt env return_type stmt =
  decorate_dummy_typ (desc_type_stmt env return_type stmt)


(** Typing of declarations *)

module Wf_typs : sig
  type t

  val empty : t

  val is_wf : t -> 'a type_annot -> bool
  val set_wf : t -> 'a type_annot -> t
  val set_wf_all : t -> 'a ident -> t
end = struct  
  module OrderedTyp = struct
    type t = unit type_annot   
    let compare = Pervasives.compare
  end
  module Tset = Set.Make(OrderedTyp)
      
  type t = Tset.t


  let build s =
    Aident (decorate s ())
  
  let empty = Tset.of_list (List.map build ["integer"; "character"; "boolean"])


  let strip = function
    | Aident id -> Aident (replace_deco id ())
    | Aaccess id -> Aaccess (replace_deco id ())
                
  let is_wf set x =
    Tset.mem (strip x) set

  let set_wf set x =
    Tset.add (strip x) set

  let set_wf_all set x =
    set_wf (set_wf set (Aident x)) (Aaccess x)
end


let ensure_wf wf t =
   if not (Wf_typs.is_wf wf t)
   then error (Format.sprintf "%a is not well-formed"
                 print_type_annot_to_str t) (typ_annot_id t).deco


let rec type_decls env wf = function
  (* env contains all declared records, 
     they do not need to have already been defined *)
  | [] -> env
  | d :: ds ->
    begin
      match d with
      | Dtype (id, None) ->
        type_decls (Env.set_typ_annot_type env id.desc
                      (Trecord (Decl id.desc)))
          (Wf_typs.set_wf wf (Aaccess id)) ds
      | Dtype (_, Some (Aident _)) -> assert false
      | Dtype (id, Some (Aaccess r)) ->
        let _ = Env.get_typ_annot_type env r in (* ensures r is declared *)
        type_decls (Env.set_typ_annot_type env id.desc
                      (Taccess (Decl r.desc)))
          (Wf_typs.set_wf wf (Aaccess id)) ds
      | Dtype_record (id, fields) ->
        let wf' = Wf_typs.set_wf wf (Aaccess id) in
        List.iter (fun (_, t) -> ensure_wf wf' t) fields;
        type_decls (Env.set_typ_annot_type env id.desc
                      (Trecord
                         (Def { ident = id.desc;
                                fields =
                                  List.map
                                    (fun (id, t) ->
                                       (id.desc, (typ_annot_id t).desc))
                                    fields
                              })))
          (Wf_typs.set_wf_all wf id) ds
      | Dvar_decl (id, t, None) -> ensure_wf wf t;
        let (_, typ) = type_type_annot env t in
        type_decls (Env.set_id_type env id.desc typ) wf ds
      | Dvar_decl (id, t, Some e) -> ensure_wf wf t;
        let (_, typ) = type_type_annot env t in
        let e' = type_expr env e in
        ensure_equiv typ e'.deco e.deco;
        type_decls (Env.set_id_type env id.desc typ) wf ds
      | Dproc p ->
        let params' = List.map (fun (id, m, t) -> ensure_wf wf t;
                                 (id, m, snd (type_type_annot env t)))
                        p.params in
        let env' = Env.set_id_type env p.name.desc
                     (Tproc (List.map (fun (_, m, t) -> (t, m)) params')) in
        let env'' = List.fold_left (fun env (id, _, t) ->
                                    Env.set_id_type env id.desc t) env params' in
        let env'' = type_decls env'' wf p.decls in
        let stmt' = type_stmt env'' Tnull p.stmt in
        type_decls env' wf ds
      | Dfunc f ->
        ensure_wf wf f.return;
        let return' = snd (type_type_annot env f.return) in
        let params' = List.map (fun (id, m, t) -> ensure_wf wf t;
                                 (id, m, snd (type_type_annot env t)))
                        f.params in
        let env' = Env.set_id_type env f.name.desc
                     (Tfunc (List.map (fun (_, m, t) -> (t, m)) params', return')) in
        let env'' = List.fold_left (fun env (id, _, t) ->
                                    Env.set_id_type env id.desc t) env params' in
        let env'' = type_decls env'' wf f.decls in
        let stmt' = type_stmt env'' return' f.stmt in
        type_decls env' wf ds
    end


(** Typing of a program *)

let type_ast a =
  ignore (type_decls Env.new_env Wf_typs.empty [Dproc a])
