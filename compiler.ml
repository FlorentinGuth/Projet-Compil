open Utils
open Ast_common
open Ast_typed
open X86_64


let get_unique_label =
  let id = ref 0 in
  (fun () -> let l = Format.sprintf "_L_%n" !id in
    incr id;
    l)


let pushn n = if n = 0 then nop else subq (imm (8*n)) (reg rsp)
let popn  n = if n = 0 then nop else addq (imm (8*n)) (reg rsp)

let rec iter n code =
  if n = 0 then nop else code ++ (iter (n - 1) code)

let copy from_addr to_addr size =
  let rec aux ofs acc =
    if ofs = -1 then acc else
      let code = movq (ind ~ofs:(8*ofs) from_addr) (reg r15) ++
        movq (reg r15) (ind ~ofs:(8*ofs) to_addr) in
      aux (ofs - 1) (acc ++ code)
  in
  aux (size - 1) nop
                       

let compile_const c =
  let n = match c with
    | Cint n -> n
    | Cchar c -> Char.code c
    | Cbool b -> if b then 1 else 0
    | Cnull -> 0
  in
  pushq (imm n)

let rec compile_expr e =
  match e.desc with
  (* Result pushed on the top of the stack *)
  | Econst c ->
    compile_const c
      
  | Eleft_val lv ->
    let (code, size) = compile_left_val lv in
    code ++ pushn size ++ copy rsi rsp size

  | Ebinop (e1, o, e2) ->
    let c1 = compile_expr e1 in
    let c2 = compile_expr e2 in
    let ce = c1 ++ c2 ++ popq rbx ++ popq rax in
    
    let comparator_code jmp_op =
      (* flags are those of e1 - e2 *)
      let l = get_unique_label () in
      ce ++
      movq (imm 1) (reg rcx) ++ cmpq (reg rbx) (reg rax) ++ jmp_op l ++
      movq (imm 0) (reg rcx) ++ label l ++ pushq (reg rcx)
    in
    let equality_code jmp_if_not =
      let l = get_unique_label () in
      let size = e1.deco in
      let rec compare i acc =
        if i = 0 then acc
        else let code = movq (ind ~ofs:(8*i) rsp) (reg rax) ++
                        movq (ind ~ofs:(8*(i + size)) rsp) (reg rbx) ++
                        cmpq (reg rbx) (reg rax) ++ jmp_if_not l in
          compare (i - 1) (code ++ acc)
      in
      c1 ++ c2 ++ movq (imm 0) (reg rcx) ++ compare size nop ++
      movq (imm 1) (reg rcx) ++ label l ++ pushq (reg rcx)
    in
    
    let lazy_code lazy_op default jmp_op =
      let l = get_unique_label () in
      c1 ++ popq rax ++ movq (imm default) (reg rcx) ++ testq (reg rax) (reg rax) ++
      jmp_op l ++ c2 ++ popq rcx ++ label l ++ pushq (reg rcx)
    in
    
    let arith_code arith_op =
      (* performs e1 op e2 *)
      ce ++ arith_op (reg rbx) (reg rax) ++ pushq (reg rax)
    in
    let div_code want_rem =
      let r = if want_rem then rdx else rax in
      ce ++ cqto ++ idivq (reg rbx) ++ pushq (reg r)
    in
    
    begin match o with
      | Beq  -> equality_code jne (* beware, it is inversed *)
      | Bneq -> equality_code je
      | Blt  -> comparator_code jl
      | Bleq -> comparator_code jle
      | Bgt  -> comparator_code jg
      | Bgeq -> comparator_code jge
                  
      | Bplus  -> arith_code addq
      | Bminus -> arith_code subq
      | Btimes -> arith_code imulq
      | Bdiv   -> div_code false
      | Brem   -> div_code true
                    
      | Band -> arith_code andq
      | Bor  -> arith_code orq
                  
      | Band_then -> lazy_code andq 0 je
      | Bor_else  -> lazy_code orq  1 jne
    end
    
  | Enot e -> (* not x = 1 - x since x is 0 or 1 *)
    compile_expr e ++ popq rax ++ negq (reg rax) ++ incq (reg rax)
                                                          
  | Enew size ->
    movq (imm (8 * size)) (reg rdi) ++ call "malloc" ++ pushq (reg rax)
                                                          
  | Eapp_func (f, args) -> (* Parameters mode to do *)
    let args_code = List.fold_left (fun c e -> c ++ compile_expr e) nop args in
    args_code ++ pushq (reg r15) (* %rbp père normalement *) ++
    call f.name ++ popn (1 + List.length args) ++ pushq (reg rax)
                                                          
and compile_left_val lv =
  (** put address in rsi and returns size of object pointed by the given address *)
  let follow_rbp n =
    movq (reg rbp) (reg rsi) ++
    iter n (movq (ind ~ofs:16 rsi) (reg rsi))
  in
  match lv with
  | Lident id ->
    let code = follow_rbp id.level ++ addq (imm (8*id.offset)) (reg rsi) ++
              if id.is_reference then movq (ind rsi) (reg rsi) else nop in
    (code, id.size)
      
  | Laccess id ->
    let code = follow_rbp id.level ++ movq (ind ~ofs:(8*id.offset) rsi) (reg rsi) ++
               if id.is_reference then movq (ind rsi) (reg rsi) else nop in
    (code, id.size)
      
  | Lmember (e, offs, size) ->
    let code = compile_expr e ++ movq (ind ~ofs:(8*offs) rsp) (reg rsi) in
    (* Memory leak TODO *)
    (code, size)
      

let rec compile_stmt s =
  match s with
  | Saffect (lv, e) ->    
    let (code, size) = compile_left_val lv in
    code ++ pushq (reg rsi) ++ compile_expr e ++
    movq (ind ~ofs:(8*e.deco) rsp) (reg rsi) ++ copy rsp rsi size ++
    popn e.deco
                                     
  | Scall_proc (p, args) -> (* Parameters mode to do *)
    let args_code = List.fold_left (fun c e -> c ++ compile_expr e) nop args in
    args_code ++ pushq (reg r15) (* %rbp père normalement *) ++
    call p.name ++ popn (1 + List.length args)

  | Sreturn e ->
    compile_expr e ++ popq rax ++ movq (reg rbp) (reg rsp) ++ popq rbp ++ ret

  | Sblock b ->
    List.fold_left (fun c s -> c ++ compile_stmt s) nop b

  | Scond (cond, s1, s2) ->
    let l_else = get_unique_label () in
    let l_end = get_unique_label () in
    compile_expr cond ++ popq rax ++ testq (reg rax) (reg rax) ++ je l_else ++
    compile_stmt s1 ++ jmp l_end ++
    label l_else ++ compile_stmt s2 ++ label l_end

  | Swhile (loop, s) ->
    let l_cond = get_unique_label () in
    let l_loop = get_unique_label () in
    jmp l_cond ++ label l_loop ++ compile_stmt s ++
    label l_cond ++ compile_expr loop ++ popq rax ++
    testq (reg rax) (reg rax) ++ jne l_loop
(*
  | Sfor (i, rev, lb, ub, s) -> (* lb | ub | i <- rsp *)
    let l_cond = get_unique_label () in
    let l_loop = get_unique_label () in
    let lb_adr = ind ~ofs:16 rsp in
    let ub_adr = ind ~ofs:8  rsp in
    let i_adr  = ind         rsp in
    let (test, update) = if rev
      then (movq i_adr (reg rdi) ++ cmpq ub_adr (reg rdi), decq)
      else (movq i_adr (reg rdi) ++ cmpq (reg rdi) ub_adr, incq)
    in
    let (_, last) = Smap.min_binding in
    let env' = Smap.add i.desc (last - 24) in (* because of lb and ub *)
    compile_expr lb ++ compile_expr ub ++ pushq (ind ~ofs:8 rsp) ++
    jmp l_cond ++ label l_loop ++ compile_stmt env' s ++ update i_adr ++
    label l_cond ++ test ++ jge l_loop ++ popn 24*)


let rec compile_decl (codefun, codemain) = function
  (* | Dtype_decl (t, ta_opt) ->
     failwith "Not implemented"

     | Drecord_def (r, fs) ->
     failwith "Not implemented"
  *)
  | Dvar_decl (offset, e) ->
    let lv = Lident { is_reference = false; size = e.deco; level = 0; offset } in
    (codefun, codemain ++ compile_stmt (Saffect (lv, e))) (* TODO access? *)

  | Dproc_func pf ->
    let (cf, cm) = compile_decls pf.decls in
    let code = label pf.pf_sig.name ++ pushq (reg rbp) ++ movq (reg rsp) (reg rbp) ++
               pushn pf.frame ++ cm ++ compile_stmt pf.stmt ++ popn pf.frame in
    (codefun ++ code ++ cf, codemain)

and compile_decls ds =
  List.fold_left compile_decl (nop, nop) ds


let compile_program p file =
  let (codefun, codemain) = compile_decls [Dproc_func p] in
  let asm = { text =
                glabel "main" ++
                jmp p.pf_sig.name ++

                codemain ++ codefun ++

                label "_f_put_0" ++
                movslq (ind ~ofs:16 rsp) rsi ++
                movq (ilab ".Sput") (reg rdi) ++
                xorq (reg rax) (reg rax) ++
                call "printf" ++
                ret ++

                label "_f_new_line_0" ++
                movq (ilab ".Snew_line") (reg rdi) ++
                xorq (reg rax) (reg rax) ++
                call "printf" ++
                ret ++

                label "_f_character'val_0" ++
                movslq (ind ~ofs:16 rsp) rax ++
                ret;
              data =
                label ".Sput" ++
                string "%c" ++
                label ".Snew_line" ++
                string "\n"
            }
  in
  let f = open_out file in
  let fmt = Format.formatter_of_out_channel f in
  print_program fmt asm;
  Format.fprintf fmt "@?";
  close_out f
