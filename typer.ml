open Ast


module Smap = Map.Make(String)
type env = typ Smap.t
type funcs = loc func Smap.t

let equiv t1 t2 =
  match (t1, t2) with
  | (Tnull, Taccess _) | (Taccess _, Tnull) -> true
  | _ -> t1 = t2

let type_field (r : record) f =
  snd (List.find (fun (s, _) -> s = f) r)


let rec type_expr (env : env) (funcs : funcs) (expr : loc expr) =
  let type_expr = type_expr env funcs in
  let verify e t = assert (e.deco = t) in
  let encaps e t = { desc = e; deco = t } in
  let type_id id = Smap.find id.desc env in
  let type_annot = function
    | Aident id -> type_id id
    | Aaccess id ->
      begin
        match type_id id with
        | Trecord r -> Taccess r
        | _ -> failwith "Record expected"
      end
  in
  match expr.desc with
  | Eint _ as ed -> encaps ed Tint
  | Echar _ as ed -> encaps ed Tchar
  | Ebool _ as ed -> encaps ed Tbool
  | Enull as ed -> encaps ed Tnull
  | Eleft_val l ->
    begin
      match l with
      | Lident id -> let t = type_id id in
        encaps (Eleft_val (Lident (encaps id.desc t))) t
      | Lmember (e2, f) ->
        begin
          let e2' = type_expr e2 in
          match e2'.deco with
          | Trecord r | Taccess r -> let t = type_field r f.desc in
            encaps (Eleft_val (Lmember (e2', encaps f.desc t))) t
          | _ -> failwith "Record expected"
        end
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
