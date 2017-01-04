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
  | _ -> error "A record was expected" loc

let wrong_type t_exp t_proc loc =
  let print_typ_to_str = print_to_string print_typ in
  error (Format.sprintf
           "This expression has type %s but was expected of type %s"
           (print_typ_to_str t_proc) (print_typ_to_str t_exp)) loc


(** The environment module *)

module Env : sig
  type t

  val new_env : t
  (** Returns a new environment with builtins *)

  val get_id_type : t -> ?lvl:level -> ident -> typ
  (** Types an identifier (variable or procedure/function) at a given level 
      (default is current one) *)
  val get_typ_ident_type : t -> ?lvl:level -> ident -> typ
  (** Types a typ_ident, i.e. a name of a type *)
  val get_typ_annot_type : t -> ?lvl:level -> typ_annot -> typ
  (** Takes into account whether typ_annot is an access or not *)

  val set_id_type : t -> ?lvl:level -> ?force:bool -> ident -> typ -> t
  (** Sets the type of an identifier (variable or procedure/function), at the given
      level, and force (defaults to false) prevents the env to check for multiple 
      definitions of the same variable (useful for for-loop-counter-variables) *)
  val set_typ_ident_type : t -> ?force:bool -> ident -> typ -> t
  val set_param : t -> ?force:bool -> param -> t
  val set_undefined : t -> ?force:bool -> ident -> t
  val set_subtypes : t -> typ -> typ -> t
  val set_not_const : t -> ident_desc -> t

  val ensure_all_def : t -> loc -> unit
  val ensure_not_const : t -> ident -> unit
  val ensure_subtype : t -> typ -> typ -> loc -> unit

  val level : t -> level
  val incr_level : t -> t
  val deco_level : t -> ident_desc -> t_ident
  (** Decorates the given identifier with the current level *)
end = struct
  type constr =
    | Ta (* type_annot *)
    | Id (* variable or procedure/fonction *)
  type obj = constr * typ
  
  module Tset = Set.Make(struct type t = typ * typ
      let compare = Pervasives.compare end)

  type map = (obj * level) list Smap.t
  type t = 
    {
      objs     : map;    (* Maps an identifier to its type *)
      consts   : Sset.t; (* Set of variables that shouldn't be modified *)
      notdef   : Sset.t; (* Set of types that haven't been defined yet *)
      subtypes : Tset.t; (* Contains (t1, t2) pairs where t2 is a subtype of t1 *)
      current_level : level;
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


  let add_in (ts, r) = Tproc_func (List.map (fun t -> (t, In)) ts, r)
  let reserved = [("character'val", add_in ([int],  char));
                  ("put",           add_in ([char], null));
                  ("new_line",      add_in ([],     null));
                 ]

  let unops    = [("not",           add_in ([bool], bool))] 
  
  let type_binop = function
    | Beq | Bneq ->                            assert false
    | Blt | Bleq | Bgt | Bgeq ->               ([int;  int],  bool)
    | Bplus | Bminus | Btimes | Bdiv | Brem -> ([int;  int],  int)
    | Band | Band_then | Bor | Bor_else ->     ([bool; bool], bool)
  let binops = List.map (fun b -> (print_to_string print_binop b,
                                   add_in (type_binop b)))
                 [Blt; Bleq; Bgt; Bgeq; Bplus; Bminus; Btimes; Bdiv; Brem;
                  Band; Band_then; Bor; Bor_else]

  let formate = List.map (fun (k, t) -> (k, [(to_id { t_ident = decorate k 0;
                                                      def = t }, 0)]))
  let builtins_ids = formate (reserved @ unops @ binops)

  let primitives = List.map (fun t -> (t.t_ident.desc, [(to_ta t, 0)]))
                     [null; int; char; bool]
                 
  let new_env = { objs = Smap.of_list (builtins_ids @ primitives);
                  consts = Sset.empty;
                  notdef = Sset.empty;
                  subtypes = Tset.empty;
                  current_level = 1;
                }


  (* Helper function to avoid copypasting get_id_type into get_typ_ident_type *)
  let get_type env lvl id id_or_ta of_id_or_ta =
    let l = try Smap.find id.desc env.objs
      with Not_found -> error (Format.sprintf "Unknown %s: %s" id_or_ta id.desc)
                          id.deco in
    let o = fst (List.find (fun o -> snd o <= lvl) l) in
    of_id_or_ta o id
      
  let get_id_type env ?(lvl = env.current_level) id =
    if Sset.mem id.desc env.notdef
    then error "A variable cannot depend on its own initialization" id.deco
    else get_type env lvl id "identifier" of_id
    
  let get_typ_ident_type env ?(lvl = env.current_level) ti =
    get_type env lvl ti "defined type" of_ta

  let get_typ_annot_type env ?(lvl = env.current_level) ta =
    if ta.is_access then
      if Sset.mem ta.typ_ident.desc env.notdef
      then { t_ident = decorate "" lvl;
             def = Taccess (replace_deco ta.typ_ident lvl) }
      else to_access (get_typ_ident_type env ~lvl ta.typ_ident)
    else get_typ_ident_type env ~lvl ta.typ_ident


  let set_id_type env ?(lvl = env.current_level) ?(force = false) id typ =
    let l = try Smap.find id.desc env.objs with Not_found -> [] in
    let lvl' = try snd (List.find (fun o -> snd o <= lvl) l)
      with Not_found -> -1 in
    if lvl' = lvl && not force
    then error (Format.sprintf "%s is already defined at this level" id.desc) id.deco;
    let l' = (to_id typ, lvl) :: l in
    let l'' = List.stable_sort (fun o o' -> compare (snd o') (snd o)) l' in
    { env with objs = Smap.add id.desc l'' env.objs;
               notdef = Sset.remove id.desc env.notdef }

  let set_typ_ident_type env ?(force = false) ta typ =
    let l = try Smap.find ta.desc env.objs with Not_found -> [] in
    let () = try
        if snd (List.hd l) = env.current_level && not force
        then error (Format.sprintf "%s is already defined at this level"
                      ta.desc) ta.deco
      with Failure _ (*"hd"*) -> () in
    let l' = (to_ta typ, env.current_level) :: l in
    let l'' = List.stable_sort (fun o o' -> compare (snd o') (snd o)) l' in
    { env with objs = Smap.add ta.desc l'' env.objs;
               notdef = Sset.remove ta.desc env.notdef }

  let set_param env ?force (a, m) =
    let t = get_typ_annot_type env a.ta in
    let env' = set_id_type env ?force a.ident t in
    { env' with consts = match m with
                    | InOut -> env.consts
                    | In    -> if is_access t then env.consts
                      else Sset.add a.ident.desc env.consts }

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
    match (t1.def, t2.def) with
    | (Taccess _, Tnull) -> ()
    | _ -> if not (Tset.mem (t1, t2) env.subtypes) && t1 <> t2
      then wrong_type t1 t2 loc2


  let level env =
    env.current_level

  let incr_level env =
    { env with current_level = env.current_level + 1 }

  let deco_level env s =
    decorate s env.current_level
end


(** Typing of expressions *)

(** Decorates an A.ident with its type (returns a T.ident) *)
let type_ident env id =
  (replace_deco id (Env.get_id_type env id) : T.ident)


let type_typ_annot env (ta : A.typ_annot) =
  (({ typ_ident = replace_deco ta.typ_ident ();
      is_access = ta.is_access } : T.typ_annot),
   Env.get_typ_annot_type env ta)

let type_annotated env (a : A.annotated) =
  let (ta', t) = type_typ_annot env a.ta in
  (({ ident = replace_deco a.ident ();
      ta    = ta' } : T.annotated), t)

let type_const = function
  | Cint  _ -> int
  | Cchar _ -> char
  | Cbool _ -> bool
  | Cnull   -> null

(** Checks if fields has a f field, id being the name of the record *)
let type_field env id fields (f : A.ident_decl) =
  try
    if f.desc = "all" then { t_ident = id; def = Trecord fields }
    else let a = List.find (fun a -> f.desc = a.ident.desc) fields in
      Env.get_typ_annot_type env ~lvl:a.ident.deco
        (map_ta (fun id -> replace_deco id f.deco) a.ta)
  with
  | Not_found -> error (Format.sprintf "Record %s has no field %s" id.desc f.desc)
                   f.deco


(** Mutually recursive functions getting to the leftmost ident to verify he shouldn't
    be modified *)
let rec lv_ensure_not_modified env lv =
  (* Could probably be factorized with type_left_val, but would be less readable *)
  match lv with
  | Lident id -> Env.ensure_not_const env id;
  | Lmember (e, _) -> if not (is_access (type_expr env e).deco) (* ineffecicient *)
    then expr_ensure_not_modified env e
and expr_ensure_not_modified env e =
  match e.desc with
  | Eleft_val lv -> lv_ensure_not_modified env lv
  | _ -> ()


and type_expr ?(ensure_lv = false) ?(ensure_not_new = false) env expr =
  match expr.desc with
  | Eleft_val lv -> let (lv', t) = type_left_val ~ensure_lv env lv in
    decorate (T.Eleft_val lv') t
  | e -> if ensure_lv then error "This expression should be a left value" expr.deco
    else begin
      match e with
      | Econst c -> decorate (T.Econst c) (type_const c)
      | Enot e -> begin
          let (e', t) = match type_proc_func env (decorate "not" expr.deco) [e] with
            | ([e'], t) -> (e', t)
            | _ -> assert false
          in
          decorate (T.Enot e') t
        end
      | Ebinop (e1, b, e2) -> begin
          match b with
          | Beq | Bneq -> let e1' = type_expr env e1 in
            let e2' = type_expr env e2 in
            let () = try
                Env.ensure_subtype env e1'.deco e2'.deco e2.deco
              with Typing_error _ -> Env.ensure_subtype env e2'.deco e1'.deco e1.deco
            in
            decorate (T.Ebinop (e1', b, e2')) bool
          | _ -> begin
              let (e1', e2', t) =
                match type_proc_func env (decorate (print_to_string print_binop b)
                                            expr.deco) [e1; e2] with
              | ([e1'; e2'], t) -> (e1', e2', t)
              | _ -> assert false
              in
              decorate (T.Ebinop (e1', b, e2')) t
            end
        end
      | Enew id -> if ensure_not_new then error "This form isn't allowed" expr.deco;
        let t = Env.get_typ_ident_type env id in
        decorate (T.Enew (replace_deco id ())) (to_access t)
      | Eapp_func (f, params) -> 
        let (params', t) = type_proc_func env f params in
        decorate (T.Eapp_func (type_ident env f, params')) t
      | Eleft_val _ -> assert false
    end
      

and type_left_val ?(ensure_lv = false) env lv =
  let not_left_val loc = error "This expression should be a left value" loc in
  match lv with
  | Lident id -> let id' = type_ident env id in
    let t = match id'.deco.def with
      | Tproc_func ([], r) -> if ensure_lv then not_left_val id.deco else r
      | _ -> id'.deco in
    (T.Lident (replace_deco id' t), t)
  | Lmember (e, f) ->
    begin
      let e' = type_expr env ~ensure_not_new:true e in
      let (r, id) = match e'.deco.def with
        | Trecord r -> (r, e'.deco.t_ident)
        | Taccess r -> let t = Env.get_typ_ident_type env ~lvl:r.deco
                                 (replace_deco r e.deco) in
          (ensure_record t.def e.deco, t.t_ident)
        | _ -> error (Format.sprintf "This expression should be a record or an \
                                      access on a record") e.deco
      in
      let t = type_field env id r f in
      if ensure_lv && not (is_access e'.deco)
      then ignore (type_expr ~ensure_lv:true env e); (* ineffective, but simpler *)
      (T.Lmember (e', replace_deco f t), t)
    end
           

and type_proc_func ?(expr = true) env name params =
  let typ = Env.get_id_type env name in
  match (typ.def, expr) with
  | (Tproc_func (_, t), true) when t = null ->
    error "A procedure cannot be called in an expression" name.deco
  | (Tproc_func (_, t), false) when t <> null ->
    error "A function cannot be applied in a statement" name.deco
  | (Tproc_func (l, r), _) -> begin
      try
        let params' = List.map2
                        (fun (t, m) e ->
                           let e' = match m with
                             | In -> type_expr env e
                             | InOut -> let e' = type_expr ~ensure_lv:true env e in
                               expr_ensure_not_modified env e; e'
                           in
                           Env.ensure_subtype env t e'.deco e.deco;
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

let rec desc_type_stmt env return_type stmt =
  match stmt.desc with
  | Saffect (lv, e) -> let (lv', t) = type_left_val ~ensure_lv:true env lv in
    lv_ensure_not_modified env lv;
    let e' = type_expr env e in
    Env.ensure_subtype env t e'.deco e.deco;
    T.Saffect (lv', e')
  | Scall_proc (p, params) ->
    let (params', _) = type_proc_func ~expr:false env p params in
    T.Scall_proc (type_ident env p, params')
  | Sreturn e -> let e' = type_expr env e in
    Env.ensure_subtype env return_type e'.deco e.deco;
    T.Sreturn e'
  | Sblock l -> T.Sblock (List.map (type_stmt env return_type) l)
  | Scond (e, s1, s2) -> let e' = type_expr env e in
    Env.ensure_subtype env bool e'.deco e.deco;
    let s1' = type_stmt env return_type s1 in
    let s2' = type_stmt env return_type s2 in
    T.Scond (e', s1', s2')
  | Swhile (e, s) -> let e' = type_expr env e in
    Env.ensure_subtype env bool e'.deco e.deco;
    let s' = type_stmt env return_type s in
    T.Swhile (e', s')
  | Sfor (id, rev, lb, ub, s) -> let id' = replace_deco id int in
    let env = Env.set_undefined env ~force:true id in
    let process b =
      let b' = type_expr env b in
      Env.ensure_subtype env int b'.deco b.deco;
      b' in
    let (lb', ub') = (process lb, process ub) in
    let env' = Env.set_param env ~force:true
                 ({ ident = id;
                    ta = { typ_ident = decorate "integer" id.deco;
                           is_access  = false } }, In) in
    let s' = type_stmt env' return_type s in
    T.Sfor (id', rev, lb', ub', s')
    
and type_stmt env return_type stmt =
  decorate (desc_type_stmt env return_type stmt) ()


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
      | Saffect _ | Scall_proc _ | Swhile _ | Sfor _ -> ensure_return name s.deco tl
      | Sreturn _ -> begin match tl with
          | [] -> ()
          | s :: _ -> report ~warning:true "" "Dead code" s.deco
        end
      | Sblock b -> try_ret [(loc, b)] tl
      | Scond (_, s1, s2) -> try_ret [(s1.deco, [s1]); (s2.deco, [s2])] tl
    end


let rec type_decl env d =
  let to_t_id id = Env.deco_level env id.desc in
  match d with
  | Dtype_decl (id, None) -> let env' = Env.set_undefined env id in
    (T.Dtype_decl (replace_deco id (), None), env')
  | Dtype_decl (id, Some ta) -> if id.desc = ta.typ_ident.desc
    then error "A type cannot be an access on itself" id.deco;
    let (ta', t) = type_typ_annot env ta in
    let t' = { t with t_ident = to_t_id id } in
    let env' = Env.set_typ_ident_type env id t' in
    let env'' = Env.set_subtypes env' t' t in
    (T.Dtype_decl (replace_deco id (), Some ta'), env'')
  | Drecord_def (id, fields) ->
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
    let fields_t = List.map (fun a ->
                              let t = Env.get_typ_annot_type env_with_r a.ta in
                              map_a (fun id -> replace_deco id (get_level t)) a)
                     fields in
    let fields' = List.map (fun a -> { ident = replace_deco a.ident ();
                                       ta = fst (type_typ_annot env_with_r a.ta) })
                    fields in
    let env' = Env.set_typ_ident_type env id
                 { t_ident = to_t_id id;
                   def = Trecord fields_t } in
    (T.Drecord_def (replace_deco id (), fields'), env')
  | Dvar_decl (a, e) -> let typ = Env.get_typ_annot_type env a.ta in
    let env = Env.set_undefined env a.ident in
    let e' = match e with
      | None -> None
      | Some e -> let e' = type_expr env e in
        Env.ensure_subtype env typ e'.deco e.deco;
        Some e' in
    let env' = Env.set_id_type env a.ident typ in
    (T.Dvar_decl (fst (type_annotated env' a), e'),
     Env.set_not_const env' a.ident.desc)
  | Dproc_func pf -> Env.ensure_all_def env pf.name.deco;
    let (return', ret) = if pf.return.typ_ident.desc = ""
      then (null, { typ_ident = decorate "" (); is_access = false })
      else begin
        ensure_return pf.name.desc pf.name.deco [pf.stmt];
        (Env.get_typ_annot_type env pf.return, fst (type_typ_annot env pf.return))
      end in
    let params' = List.map (fun (a, m) -> (type_annotated env a, m)) (* ensures wf *)
                    pf.params in
    let add_pf env2 = Env.set_id_type env2 ~lvl:(Env.level env) pf.name
                        { t_ident = to_t_id pf.name;
                          def = Tproc_func
                                  (List.map (fun ((_, t), m) -> (t, m)) params',
                                   return') } in
    let env' = List.fold_left (fun env p -> Env.set_param env p)
                 (Env.incr_level env) pf.params in
    (* Adding pf after params and return, thanks to the absurd shadowing rules *)
    let (decls', env'') = type_decls (add_pf env') pf.stmt.deco pf.decls in
    let stmt' = type_stmt env'' return' pf.stmt in
    let pf' = { T.name = replace_deco pf.name ();
                T.params = List.map (fun ((a, _), m) -> (a, m)) params';
                T.return = ret;
                T.decls = decls'; (* embed environment? *)
                T.stmt = stmt' } in
    (T.Dproc_func pf', add_pf env)

and type_decls env loc_after decls =
  match decls with
  | [] -> Env.ensure_all_def env loc_after; ([], env)
  | d :: ds -> let (d', env') = type_decl env d in
    let (ds', env'') = type_decls env' loc_after ds in
    (d' :: ds', env'')


(** Typing of a program *)

let type_ast (a : A.ast) =
  match fst (type_decl Env.new_env (Dproc_func a)) with
  | T.Dproc_func a' -> (a' : T.ast)
  | _ -> assert false
