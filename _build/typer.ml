open Ast_common
open Utils
open Printer

module A = Ast
module T = Ast_typed


let get_unique_id =
  let id = ref (-1) in
  (fun () -> incr id; Format.sprintf "_id_%d" !id)

(** General error functions *)

let error msg loc =
  raise (Typing_error (msg, loc))
    

let ensure_record typ loc =
  match typ with
  | T.Trecord r -> r
  | _ -> error "A record was expected" loc

let wrong_type t_exp t_proc loc =
  let print_typ_to_str = print_to_string T.print_typ in
  error (Format.sprintf
           "This expression has type %s but was expected of type %s"
           (print_typ_to_str t_proc) (print_typ_to_str t_exp)) loc


(** The environment module *)

module Env : sig
  type t

  val new_env : t
  (** Returns a new environment with builtins *)

  val get_id_type : t -> ?lvl:int -> A.ident -> T.ident * T.typ
  (** Types an identifier (variable or procedure/function) at a given level 
      (default is current one) *)
  val get_typ_ident_type : t -> ?lvl:int -> A.ident_decl -> T.typ
  (** Types a typ_ident, i.e. a name of a type *)
  val get_typ_annot_type : t -> ?lvl:int -> A.typ_annot -> T.typ
  (** Takes into account whether typ_annot is an access or not *)

  val set_id_type : t -> ?lvl:int -> ?force:bool -> A.ident -> T.typ -> t * int
  (** Sets the type of an identifier (variable or procedure/function), at the given
      level, and force (defaults to false) prevents the env to check for multiple 
      definitions of the same variable (useful for for-loop-counter-variables).
      It also returns the offset of the newly declared variable *)
  val set_typ_ident_type : t -> ?force:bool -> A.ident -> T.typ -> t
  val set_param : t -> ?force:bool -> A.param -> t * int
  val set_undefined : t -> ?force:bool -> A.ident -> t
  val set_subtypes : t -> T.typ (* big *) -> T.typ (* little *) -> t
  val set_not_const : t -> ident_desc -> t

  val ensure_all_def : t -> loc -> unit
  val ensure_not_const : t -> A.ident -> unit
  val ensure_subtype : t -> T.typ -> T.typ -> loc -> unit

  val level : t -> int
  val incr_level : t -> t
  val deco_level : t -> ident_desc -> T.t_ident
  (** Decorates the given identifier with the current level *)
  val get_frame_size : t -> int
end = struct
  type constr =
    | Ta (* type_annot *)
    | Id (* variable or procedure/fonction *)
  type obj = constr * (T.typ * int)
  
  module Tset = Set.Make(struct type t = T.typ * T.typ
      let compare = Pervasives.compare end)

  type map = (obj * int) list Smap.t
  type t = 
    {
      objs    : map;    (* Maps an identifier to its type *)
      consts  : Sset.t; (* Set of variables that shouldn't be modified *)
      notdef  : Sset.t; (* Set of types that haven't been defined yet *)
      byref   : Sset.t; (* Set of variables that have been passed by reference *)
      subtypes: Tset.t; (* Contains (t1, t2) pairs where t2 is a subtype of t1 *)
      current_level : int;
      current_offset: int;
    }

  (* Functions to perform checks on objects, when they are expected to be an ident
     and they are in fact a type for instance *)
  let to_id t = ((Id, t) : obj)
  let to_ta t = ((Ta, t) : obj)
  let of_id (obj : obj) id =
    match obj with
    | (Id, t) -> t
    | (Ta, _) -> error (Format.sprintf "%s is a type but an identifier is expected"
                          id.desc) id.deco
  let of_ta (obj : obj) id =
    match obj with
    | (Id, _) -> error (Format.sprintf "%s is an identifier but a type is expected"
                          id.desc) id.deco
    | (Ta, t) -> t


  let add_in (ts, r) = T.Tproc_func (List.map (fun t -> (t, In)) ts, r)
  let reserved = [("character'val", add_in ([T.int],  T.char));
                  ("put",           add_in ([T.char], T.null));
                  ("new_line",      add_in ([],       T.null));
                 ]

  let unops    = [("not",           add_in ([T.bool], T.bool))] 
  
  let type_binop = function
    | Beq | Bneq ->                            assert false
    | Blt | Bleq | Bgt | Bgeq ->               ([T.int;  T.int],  T.bool)
    | Bplus | Bminus | Btimes | Bdiv | Brem -> ([T.int;  T.int],  T.int)
    | Band | Band_then | Bor | Bor_else ->     ([T.bool; T.bool], T.bool)
  let binops = List.map (fun b -> (print_to_string print_binop b,
                                   add_in (type_binop b)))
                 [Blt; Bleq; Bgt; Bgeq; Bplus; Bminus; Btimes; Bdiv; Brem;
                  Band; Band_then; Bor; Bor_else]

  let formate = List.map (fun (k, t) -> (k, [(to_id (T.{ t_ident = decorate k 0;
                                                         def = t;
                                                         t_size = -1; }, 0), 0)]))
  let builtins_ids = formate (reserved @ unops @ binops)

  let primitives = List.map (fun t -> (t.T.t_ident.desc, [(to_ta (t, 0), 0)]))
                     [T.null; T.int; T.char; T.bool]
                 
  let new_env = { objs = Smap.of_list (builtins_ids @ primitives);
                  consts = Sset.empty;
                  notdef = Sset.empty;
                  byref  = Sset.empty;
                  subtypes = Tset.empty;
                  current_level = 0; (* TODO change? *)
                  current_offset = 0;
                }


  (* Helper function to avoid copypasting get_id_type into get_typ_ident_type *)
  let get_type env lvl id id_or_ta of_id_or_ta =
    let l = try Smap.find id.desc env.objs
      with Not_found -> error (Format.sprintf "Unknown %s: %s" id_or_ta id.desc)
                          id.deco in
    let (o, lvl_o) = List.find (fun o -> snd o <= lvl) l in
    (of_id_or_ta o id, lvl_o)
      
  let get_id_type env ?(lvl = env.current_level) id =
    if Sset.mem id.desc env.notdef
    then error "A variable cannot depend on its own initialization" id.deco
    else let ((typ, offset), level) = get_type env lvl id "identifier" of_id in
      let byref = Sset.mem id.desc env.byref in
      (T.{ is_reference = byref;
           size = typ.t_size;
           level = lvl - level;
           offset }, typ)
    
  let get_typ_ident_type env ?(lvl = env.current_level) ti =
    let ((typ, _offset), _level) = get_type env lvl ti "defined type" of_ta in
    typ

  let get_typ_annot_type env ?(lvl = env.current_level) ta =
    if ta.is_access then
      if Sset.mem ta.typ_ident.desc env.notdef then
        { T.t_ident = decorate "" lvl;
          T.def = T.Taccess (replace_deco ta.typ_ident lvl);
          t_size = 1; }
      else T.to_access (get_typ_ident_type env ~lvl ta.typ_ident)
    else get_typ_ident_type env ~lvl ta.typ_ident


  let set_id_type env ?(lvl = env.current_level) ?(force = false) id typ =
    let l = try Smap.find id.desc env.objs with Not_found -> [] in
    let lvl' = try snd (List.find (fun o -> snd o <= lvl) l)
      with Not_found -> -1 in
    if lvl' = lvl && not force
    then error (Format.sprintf "%s is already defined at this level" id.desc) id.deco;
    let offs = env.current_offset - 1 in
    let l' = (to_id (typ, offs), lvl) :: l in
    let l'' = List.stable_sort (fun o o' -> compare (snd o') (snd o)) l' in
    ({ env with objs = Smap.add id.desc l'' env.objs;
                notdef = Sset.remove id.desc env.notdef;
                current_offset = offs }, offs)

  let set_typ_ident_type env ?(force = false) ta typ =
    let l = try Smap.find ta.desc env.objs with Not_found -> [] in
    let () = try
        if snd (List.hd l) = env.current_level && not force
        then error (Format.sprintf "%s is already defined at this level"
                      ta.desc) ta.deco
      with Failure _ (*"hd"*) -> () in
    let l' = (to_ta (typ, 0), env.current_level) :: l in
    let l'' = List.stable_sort (fun o o' -> compare (snd o') (snd o)) l' in
    { env with objs = Smap.add ta.desc l'' env.objs;
               notdef = Sset.remove ta.desc env.notdef }

  let set_param env ?force (a, m) =
    let t = get_typ_annot_type env a.ta in
    let (env', ofs) = set_id_type env ?force a.ident t in
    let (consts', byref') = match m with
      | InOut -> (env.consts, Sset.add a.ident.desc env.byref)
      | In    -> if T.is_access t then (env.consts, env.byref)
        else (Sset.add a.ident.desc env.consts, env.byref)
    in
    ({ env' with consts = consts'; byref = byref' }, ofs)

  let set_not_const env id =
    { env with consts = Sset.remove id env.consts }

  let set_subtypes env t1 t2 =
    let sub = Tset.fold (fun (t3, t4) tset -> if t2 = t3 then Tset.add (t1, t4) tset
                          else if t1 = t4 then Tset.add (t3, t2) tset else tset)
                env.subtypes (Tset.add (t1, t2) env.subtypes) in
    { env with subtypes = sub }

  let set_undefined env ?(force = false) id =
    if Sset.mem id.desc env.notdef && not force then
      error (Format.sprintf "%s is already declared at this level" id.desc) id.deco;
    try
      if snd (List.hd (Smap.find id.desc env.objs)) = env.current_level && not force
      then error (Format.sprintf "%s is already defined at this level" id.desc)
             id.deco
      else raise Not_found
    with Not_found -> { env with notdef = Sset.add id.desc env.notdef }


  let ensure_all_def env loc =
    try
      let id = Sset.choose env.notdef in
      error (Format.sprintf "%s has not been defined before the \
                             next level of declarations" id) loc
    with Not_found -> ()

  let ensure_not_const env id =
    if Sset.mem id.desc env.consts
    then error (Format.sprintf "%s cannot be modified, it has been \
                                declared In or is a counter variable" id.desc)
           id.deco

  let ensure_subtype env t1 t2 loc2 =
    (* t2 is a subtype of t1? *)
    match (t1.T.def, t2.T.def) with
    | (T.Taccess _, T.Tnull) -> ()
    | _ -> if not (Tset.mem (t1, t2) env.subtypes) && t1 <> t2
      then wrong_type t1 t2 loc2


  let level env =
    env.current_level

  let incr_level env =
    { env with current_level = env.current_level + 1 }

  let deco_level env s =
    decorate s env.current_level

  let get_frame_size env =
    -env.current_offset
end


(** Typing of expressions *)

let type_const = function
  | Cint  _ -> T.int
  | Cchar _ -> T.char
  | Cbool _ -> T.bool
  | Cnull   -> T.null

(** Checks if fields has a f field, id being the name of the record, and in this
    case return (typ_of_field, offset) *)
let type_field env id fields (f : A.ident_decl) =
  let type_field_aux a =
    Env.get_typ_annot_type env ~lvl:a.ident.deco
      (map_ta (fun id -> replace_deco id f.deco) a.ta)
  in
  if f.desc = "all" then
    let t_size = List.fold_left (fun s a -> s + (type_field_aux a).T.t_size)
                   0 fields in
    (T.{ t_ident = id;
         def = Trecord fields;
         t_size }, 0)
  else
    let rec search fields offs =
      match fields with
      | [] -> error (Format.sprintf "Record %s has no field %s" id.desc f.desc)
                f.deco
      | a :: tl -> let t = type_field_aux a in
        if f.desc = a.ident.desc then (t, offs)
        else search tl (offs + t.T.t_size)
    in
    search fields 0


let deco_expr e t =
  ((decorate e t.T.t_size : T.expr), t)

(** Mutually recursive functions getting to the leftmost ident to verify he shouldn't
    be modified *)
let rec lv_ensure_not_modified env lv =
  (* Could probably be factorized with type_left_val, but would be less readable *)
  match lv with
  | A.Lident id -> Env.ensure_not_const env id;
  | A.Lmember (e, _) -> if not (T.is_access (snd (type_expr env e))) (* ineffecicient *)
    then expr_ensure_not_modified env e
and expr_ensure_not_modified env e =
  match e.desc with
  | A.Eleft_val lv -> lv_ensure_not_modified env lv
  | _ -> ()


and type_expr ?(ensure_lv = false) ?(ensure_not_new = false) env expr =
  match expr.desc with
  | A.Eleft_val lv ->
    let (lv', t) = type_left_val ~ensure_lv env lv in
    deco_expr (T.Eleft_val lv') t
      
  | e ->
    if ensure_lv then error "This expression should be a left value" expr.deco
    else begin
      match e with
      | A.Econst c ->
        let t = type_const c in deco_expr (T.Econst c) t
                                  
      | A.Enot e ->
        begin
          let (e', t) = match type_proc_func env (decorate "not" expr.deco) [e] with
            | ([e'], t, _, _) -> (e', t)
            | _ -> assert false
          in deco_expr (T.Enot e') t
        end
        
      | A.Ebinop (e1, b, e2) ->
        begin
          match b with
          | Beq | Bneq ->
            let (e1', t1) = type_expr env e1 in
            let (e2', t2) = type_expr env e2 in
            let () = try
                Env.ensure_subtype env t1 t2 e2.deco
              with Typing_error _ -> Env.ensure_subtype env t2 t1 e1.deco
            in deco_expr (T.Ebinop (e1', b, e2')) T.bool
                 
          | _ ->
            begin
              let (e1', e2', t) =
                match type_proc_func env (decorate (print_to_string print_binop b)
                                            expr.deco) [e1; e2] with
              | ([e1'; e2'], t, _, _) -> (e1', e2', t)
              | _ -> assert false
              in
              deco_expr (T.Ebinop (e1', b, e2')) t
            end
        end
        
      | A.Enew id ->
        if ensure_not_new then error "This form isn't allowed" expr.deco;
        let t = Env.get_typ_ident_type env id in
        deco_expr (T.Enew t.T.t_size) (T.to_access t)
          
      | A.Eapp_func (f, params) -> 
        let (params', t, modes, lvl) = type_proc_func env f params in
        let pf_sig = T.{ name = (Format.sprintf "_f_%s_%d" f.desc lvl);
                         modes;
                         level = Env.level env - lvl } in
        deco_expr (T.Eapp_func (pf_sig, params')) t
          
      | A.Eleft_val _ ->
        assert false
    end
      

and type_left_val ?(ensure_lv = false) env lv =
  let not_left_val loc = error "This expression should be a left value" loc in
  match lv with
  | A.Lident id ->
    let (id', t) = Env.get_id_type env id in
    let t = match t.T.def with
      | T.Tproc_func ([], r) -> if ensure_lv then not_left_val id.deco else r
      | _ -> t in
    if T.is_access t then (T.Laccess id', t) else (T.Lident id', t)
    
  | A.Lmember (e, f) ->
    begin
      let (e', t) = type_expr env ~ensure_not_new:true e in
      let (r, id_r) = match t.T.def with
        | T.Trecord r -> (r, t.T.t_ident)
        | T.Taccess r -> let typ = Env.get_typ_ident_type env ~lvl:r.deco
                                     (replace_deco r e.deco) in
          (ensure_record typ.T.def e.deco, typ.T.t_ident)
        | _ -> error (Format.sprintf "This expression should be a record or an \
                                      access on a record") e.deco
      in
      let (t_field, offs) = type_field env id_r r f in
      if ensure_lv && not (T.is_access t)
      then ignore (type_expr ~ensure_lv:true env e); (* ineffective, but simpler *)
      (T.Lmember (e', offs, t_field.T.t_size), t_field)
    end
           

and type_proc_func ?(expr = true) env name params =
  let (_, typ) = Env.get_id_type env name in
  match (typ.T.def, expr) with
  | (T.Tproc_func (_, t), true) when t = T.null ->
    error "A procedure cannot be called in an expression" name.deco
  | (T.Tproc_func (_, t), false) when t <> T.null ->
    error "A function cannot be applied in a statement" name.deco
  | (T.Tproc_func (l, r), _) -> begin
      try
        let params' = List.map2
                        (fun (t, m) e ->
                           let (e', te) = match m with
                             | In -> type_expr env e
                             | InOut ->
                               let (e', te) = type_expr ~ensure_lv:true env e in
                               expr_ensure_not_modified env e; (e', te)
                           in
                           Env.ensure_subtype env t te e.deco;
                           e') l params in
        (params', r, List.map snd l, typ.T.t_ident.deco)
      with
      | Invalid_argument _ (* "List.map2" *) ->
        error (Format.sprintf "%s expects %d arguments but is given %d here"
                 name.desc (List.length l) (List.length params)) name.deco
    end
  | _ -> let (fp, v) = if expr then ("function", "applied")
           else ("procedure", "called") in
    error (Format.sprintf "%s is not a %s, it cannot be %s" name.desc fp v) name.deco


(** Typing of statements *)

let rec type_stmt env return_type stmt =
  match stmt.desc with
  | A.Saffect (lv, e) ->
    let (lv', t) = type_left_val ~ensure_lv:true env lv in
    lv_ensure_not_modified env lv;
    let (e', te) = type_expr env e in
    Env.ensure_subtype env t te e.deco;
    T.Saffect (lv', e')
      
  | A.Scall_proc (p, params) ->
    let (params', _t, modes, lvl) = type_proc_func ~expr:false env p params in
    let pf_sig = T.{ name = Format.sprintf "_f_%s_%d" p.desc lvl;
                     modes;
                     level = Env.level env - lvl } in
    T.Scall_proc (pf_sig, params')
      
  | A.Sreturn e ->
    let (e', t) = type_expr env e in
    Env.ensure_subtype env return_type t e.deco;
    T.Sreturn e'
      
  | A.Sblock l ->
    T.Sblock (List.map (type_stmt env return_type) l)
                    
  | A.Scond (e, s1, s2) ->
    let (e', t) = type_expr env e in
    Env.ensure_subtype env T.bool t e.deco;
    let s1' = type_stmt env return_type s1 in
    let s2' = type_stmt env return_type s2 in
    T.Scond (e', s1', s2')
      
  | A.Swhile (e, s) ->
    let (e', t) = type_expr env e in
    Env.ensure_subtype env T.bool t e.deco;
    let s' = type_stmt env return_type s in
    T.Swhile (e', s')
      
  | A.Sfor (id, rev, lb, ub, s) -> 
    let env = Env.set_undefined env ~force:true id in
    let dummy_ident ofs =
      T.{ is_reference = false; size = 1; level = 0; offset = ofs } in
    let process env b id =
      let (b', t) = type_expr env b in
      Env.ensure_subtype env T.int t b.deco;
      let (env', ofs) = Env.set_id_type env (decorate_dummy_loc id) T.int in
      (b', dummy_ident ofs, env') in
    let (lb', id_lb, env) = process env lb (get_unique_id ()) in
    let (ub', id_ub, env) = process env ub (get_unique_id ()) in
    let (env', ofs_i) = Env.set_param env ~force:true
                          ({ ident = id;
                             ta = { typ_ident = decorate "integer" id.deco;
                                    is_access  = false } }, In) in
    let id_i = dummy_ident ofs_i in
    let s' = type_stmt env' return_type s in
    let (start_expr, test_op, test_id, incr_op) =
      if rev then (ub', Bgeq, id_lb, Bminus)
      else (lb', Bleq, id_ub, Bplus) in
    let open T in
    let lid_i = Lident id_i in
    let expr_i = decorate (Eleft_val lid_i) 1 in
    let expr_test_id = decorate (Eleft_val (Lident test_id)) 1 in
    let expr_1 = decorate (Econst (Cint 1)) 1 in
    let expr_incr = decorate (Ebinop (expr_i, incr_op, expr_1)) 1 in
    let start = Saffect (lid_i, start_expr) in
    let test = Ebinop (expr_i, test_op, expr_test_id) in
    let incr = Saffect (lid_i, expr_incr) in
    T.Sblock T.[start; Swhile (decorate test 1, Sblock [s'; incr])]


(** Typing of declarations *)

let rec ensure_return name loc stmts =
  let try_ret l b2 = try List.iter (fun (loc, b) -> ensure_return name loc b) l
    with Typing_error (_, loc') -> ensure_return name loc' b2
  in
  match stmts with
  | [] -> error (Format.sprintf "Function %s does not always end with a return"
                   name) loc
  | s :: tl -> begin
      match s.desc with
      | A.Saffect _ | A.Scall_proc _ | A.Swhile _ | A.Sfor _ -> ensure_return name s.deco tl
      | A.Sreturn _ -> begin match tl with
          | [] -> ()
          | s :: _ -> report ~warning:true "" "Dead code" s.deco
        end
      | A.Sblock b -> try_ret [(loc, b)] tl
      | A.Scond (_, s1, s2) -> try_ret [(s1.deco, [s1]); (s2.deco, [s2])] tl
    end


let rec type_decl env d =
  let to_t_id id = Env.deco_level env id.desc in
  match d with
  | A.Dtype_decl (id, None) ->
    let env' = Env.set_undefined env id in
    (None, env')
    
  | A.Dtype_decl (id, Some ta) ->
    if id.desc = ta.typ_ident.desc
    then error "A type cannot be an access on itself" id.deco;
    let t = Env.get_typ_annot_type env ta in
    let t' = { t with T.t_ident = to_t_id id } in
    let env' = Env.set_typ_ident_type env id t' in
    let env'' = Env.set_subtypes env' t' t in
    (None, env'')
    
  | A.Drecord_def (id, fields) ->
    let env_with_r = try
        Env.set_undefined env id
      with Typing_error _ -> env (* r already declared *) in
    let idents = List.map (fun a -> a.ident) fields in
    let () = match List.find_duplicates
                     (fun i i' -> String.compare i.desc i'.desc) idents with
    | None -> ()
    | Some (_, i') -> error (Format.sprintf "Field %s has already been declared"
                               i'.desc) i'.deco
    in
    let size = ref 0 in (* ugly... *)
    let fields_t = List.map (fun a ->
                              let t = Env.get_typ_annot_type env_with_r a.ta in
                              size := !size + t.T.t_ident.deco;
                              map_a (fun id -> replace_deco id (T.get_level t)) a)
                     fields in
    let env' = Env.set_typ_ident_type env id
                 { T.t_ident = to_t_id id;
                   T.def = T.Trecord fields_t;
                   t_size = !size } in
    (None, env')
    
  | A.Dvar_decl (a, e) ->
    let env = Env.set_undefined env a.ident in
    let typ = Env.get_typ_annot_type env a.ta in
    if a.ident.desc = a.ta.typ_ident.desc
    then error (Format.sprintf "Object %s cannot be used before the end of its \\
                  declaration" a.ident.desc) a.ta.typ_ident.deco;
    begin match e with
      | None ->
        let (env', _offs) = Env.set_id_type env a.ident typ in
        (None,
         Env.set_not_const env' a.ident.desc)
        
      | Some e ->
        let (e', t) = type_expr env e in
        Env.ensure_subtype env typ t e.deco;
        let (env', offs) = Env.set_id_type env a.ident typ in
        (Some (T.Dvar_decl (offs, e')),
         Env.set_not_const env' a.ident.desc)
    end
    
  | A.Dproc_func pf ->
    Env.ensure_all_def env pf.A.name.deco;
    let return' = if pf.A.return.typ_ident.desc = ""
      then T.null else begin
        ensure_return pf.A.name.desc pf.A.name.deco [pf.A.stmt];
        Env.get_typ_annot_type env pf.A.return
      end in
    let params' = List.map (fun (a, m) -> (Env.get_typ_annot_type env a.ta, m))
                    (* ensures wf *) pf.A.params in
    let add_pf env2 = fst (Env.set_id_type env2 ~lvl:(Env.level env) pf.A.name
                             T.{ t_ident = to_t_id pf.A.name;
                                 def = Tproc_func (params', return');
                                 t_size = -1 }) in
    let env' = List.fold_left (fun env p -> fst (Env.set_param env p))
                 (Env.incr_level env) pf.A.params in
    (* Adding pf after params and return, thanks to the absurd shadowing rules *)
    let (decls', env'') = type_decls (add_pf env') pf.A.stmt.deco pf.A.decls in
    let stmt' = type_stmt env'' return' pf.A.stmt in
    let stmt'' = T.(Sblock [stmt'; Sreturn (decorate (Econst Cnull) 1)]) in
    let pf_sig = T.{ name = Format.sprintf "_f_%s_%d" pf.A.name.desc (Env.level env);
                     modes = List.map snd params';
                     level = Env.level env } in
    let pf' = T.{ pf_sig;
                  frame = Env.get_frame_size env'';
                  decls = decls';
                  stmt = stmt'' } in
    (Some (T.Dproc_func pf'), add_pf env)

and type_decls env loc_after decls =
  match decls with
  | [] -> Env.ensure_all_def env loc_after; ([], env)
  | d :: ds -> let (d', env_d) = type_decl env d in
    let (ds', env_ds) = type_decls env_d loc_after ds in
    match d' with
    | None -> (ds', env_ds)
    | Some d' ->  (d' :: ds', env_ds)


(** Typing of a program *)

let type_ast (a : A.ast) =
  match fst (type_decl Env.new_env (A.Dproc_func a)) with
  | Some (T.Dproc_func a') -> (a' : T.ast)
  | _ -> assert false
