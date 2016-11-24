open Ast
open Utils

let error m s e =
  raise (Typing_error (m, (s, e)))

let equiv t1 t2 =
  match (t1, t2) with
  | (Tnull, Taccess _) | (Taccess _, Tnull) -> true
  | _ -> t1 = t2



module Env : sig
  type t

  val new_env : t

  val get_id_type :  t -> ident_desc -> typ
  val get_typ_type : t -> ident_desc -> typ 
end = struct
  module Smap = Map.Make(String)
      
  type map = typ Smap.t (* maps an identifier with its type *)
  type t = map * map
  let new_env = (Smap.empty,
                 Smap.of_list [("integer",   Tint);
                               ("character", Tchar);
                               ("boolean",   Tbool);])

  let get_id_type (id_env, _) id =
    Smap.find id id_env

  let get_typ_type (_, typ_env) typ =
    Smap.find typ typ_env
end




let type_ident env id =
  replace_deco id (Env.get_id_type env id.desc)

let type_unop = function
  | Unot  -> Tfunc ([Tbool], Tbool)
  | Uminus-> Tfunc ([Tint],  Tint)

let type_binop = function
  | Beq | Bneq -> assert false
  | Blt | Bleq | Bgt | Bgeq -> Tfunc ([Tint; Tint], Tbool)
  | Bplus | Bminus | Btimes | Bdiv | Brem ->
    Tfunc ([Tint; Tint], Tint)
  | Band | Band_then | Bor | Bor_else ->
    Tfunc ([Tbool; Tbool], Tbool)

let type_type_annot env ta =
  let type_typ typ =
    replace_deco typ (Env.get_typ_type env typ.desc)
  in
  match ta with
  | Aident  id -> Aident  (type_typ id)
  | Aaccess id -> Aaccess (type_typ id)



let type_const = function
  | Cint  _ -> Tint
  | Cchar _ -> Tchar
  | Cbool _ -> Tbool
  | Cnull   -> Tnull

let type_leftval env = function
  | Lident id -> type_id id
  


  
let rec type_expr (env : env) (expr : loc expr) =
  let verify e t = assert (e.deco = t) in
  let type_annot = function
    | Aident id -> type_id env id
    | Aaccess id ->
      begin
        match type_id env id with
        | Trecord r -> Taccess r
        | _ -> failwith "Record expected"
      end
  in
  match expr.desc with
  | Econst c -> replace_deco expr (type_const c)
  | Eleft_val (Lident id) -> let t = type_id env id in
    decorate (Eleft_val (Lident (replace_deco id t))) t
  | Eleft_val (Lmember (e2, f)) ->
    begin
      let e2' = type_expr env e2 in
      match e2'.deco with
      | Trecord r | Taccess r ->
        let t_r = 
        let t = type_field r f.desc in
        encaps (Eleft_val (Lmember (e2', encaps f.desc t))) t
      | _ -> failwith "Record expected"
    end
  | Ebinop (e1, op, e2) ->
    begin
      let e1' = type_expr e1 in
      let e2' = type_expr e2 in
      let e' = Ebinop (e1', op, e2') in
      match op with
      | Oeq | Oneq -> if equiv e1'.deco e2'.deco
        then encaps e' Tbool
        else failwith "Must have same type"
      | Olt | Ole | Ogt | Oge -> verify e1' Tint; verify e2' Tint;
        encaps e' Tbool
      | Oplus | Ominus | Otimes | Odiv | Orem -> verify e1' Tint; verify e2' Tint;
        encaps e' Tint
      | Oand | Oand_then | Oor | Oor_else -> verify e1' Tbool; verify e2' Tbool;
        encaps e' Tbool
    end
  | Enot e -> let e' = type_expr e in
    verify e' Tbool;
    encaps (Enot e') Tbool
  | Eminus e -> let e' = type_expr e in
    verify e' Tint;
    encaps (Eminus e') Tint
  | Enew id -> 
    begin
      match type_id id with
      | (Trecord r) as t -> encaps (Enew (encaps id.desc t)) (Taccess r)
      | _ -> failwith "Record expected"
    end
  | Eapp_func (f, params) -> let f = Smap.find f.desc funcs in
    let types = List.map (fun (_, m, t) -> assert (m = In); type_annot t) f.params in
    let params' = List.map2 (fun e t -> let e' = type_expr e in
                              if equiv e'.deco t then e' else failwith "Wrong type")
                    params types in
    encaps (Eapp_func (encaps f.name.desc Tnull, params')) (type_annot f.return)
  | Echar_val e -> let e' = type_expr e in
    verify e' Tint;
    encaps (Echar_val e') Tchar
