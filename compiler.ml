open Utils
open Ast
open TypedAST
open X86_64


type local_env = int Smap.t

let get_unique_label =
  let id = ref 0 in
  (fun () -> let l = Format.sprintf "_L%n" !id in
    incr id;
    l)


let pushn n = subq (imm n) (reg rsp)
let popn  n = addq (imm n) (reg rsp)


let compile_const c =
  let n = match c with
    | Cint n -> n
    | Cchar c -> Char.code c
    | Cbool b -> if b then 1 else 0
    | Cnull -> 0
  in
  pushq (imm n)
                                 
let rec compile_expr env e =
  match e.desc with
  (* Result pushed on the top of the stack *)
  | Econst c ->
    compile_const c
      
  | Eleft_val lv ->
    let ofs = compile_left_val env lv in
    movq (ind ~ofs rbp) (reg rax)
      
  | Ebinop (e1, o, e2) ->
    let c1 = compile_expr env e1 in
    let c2 = compile_expr env e2 in
    let ce = c1 ++ c2 ++ popq rbx ++ popq rax in
    
    let comparator_code jmp_op =
      (* flags are those of e1 - e2 *)
      let l = get_unique_label () in
      ce ++
      movq (imm 1) (reg rcx) ++ cmpq (reg rbx) (reg rax) ++ jmp_op l ++
      movq (imm 0) (reg rcx) ++ label l ++ pushq (reg rcx)
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
      | Beq  -> comparator_code je                               
      | Bneq -> comparator_code jne
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
    compile_expr env e ++ popq rax ++ negq (reg rax) ++ incq (reg rax)

  | Enew r ->
    failwith "Not implemented"

  | Eapp_func (f, args) -> (* Parameters mode to do *)
    let args_code = List.fold_left (fun c e -> c ++ compile_expr env e) nop args in
    args_code ++ pushq (reg rbp) (* %rbp père normalement *) ++
    call f.desc ++ popn (8 + 8 * (List.length args)) ++ pushq (reg rax)

and compile_left_val env = function
  (* returns the offset (from rbp) of the left value *)
  | Lident id ->
    Smap.find id.desc env
      
  | Lmember (e, f) ->
    failwith "Not implemented"


let rec compile_stmt env s =
  match s.desc with
  | Saffect (lv, e) ->
    let ofs = compile_left_val env lv in
    compile_expr env e ++ popq rax ++ movq (reg rax) (ind ~ofs rbp)

  | Scall_proc (p, args) -> (* Parameters mode to do *)
    let args_code = List.fold_left (fun c e -> c ++ compile_expr env e) nop args in
    args_code ++ pushq (reg rbp) (* %rbp père normalement *) ++
    call p.desc ++ popn (8 + 8 * (List.length args))

  | Sreturn e ->
    failwith "Not implemented"

  | Sblock b ->
    List.fold_left (fun c s -> c ++ compile_stmt env s) nop b

  | Scond (cond, s1, s2) ->
    let l_else = get_unique_label () in
    let l_end = get_unique_label () in
    compile_expr env cond ++ popq rax ++ testq (reg rax) (reg rax) ++ je l_else ++
    compile_stmt env s1 ++ jmp l_end ++
    label l_else ++ compile_stmt env s2 ++ label l_end
    
  | Swhile (loop, s) ->
    let l_cond = get_unique_label () in
    let l_loop = get_unique_label () in
    jmp l_cond ++ label l_loop ++ compile_stmt env s ++
    label l_cond ++ compile_expr env loop ++ popq rax ++
    testq (reg rax) (reg rax) ++ je l_loop

  | Sfor (i, rev, lb, ub, s) ->(*
    let l_cond = get_unique_label () in
    let l_loop = get_unique_label () in
    let (init, test, update) = if rev
      then (ub, cmpq (imm lb) truc, decq)
      else (lb, cmpq truc (imm ub), incq)
    in
    let env' = in
    jmp l_cond ++ label l_loop ++ compile_stmt env' s ++
    label l_cond ++ test ++ *)
    failwith "Not implemented"


let compile_decl env = function
  | Dtype_decl (t, ta_opt) ->
    failwith "Not implemented"

  | Drecord_def (r, fs) ->
    failwith "Not implemented"

  | Dvar_decl (annot, e_opt) ->
    failwith "Not implemented"

  | Dproc_func pf ->
    failwith "Not implemented"



let compile_program p file =
  let asm = { text =
                glabel "main" ++
                label p.name.desc ++
                movq (reg rsp) (reg rbp) ++
                compile_stmt Smap.empty p.stmt ++
                xorq (reg rax) (reg rax) ++ (* exit 0 *)
                ret ++
                
                label "put" ++
                movslq (ind ~ofs:16 rsp) rsi ++
                movq (ilab ".Sput") (reg rdi) ++
                xorq (reg rax) (reg rax) ++
                call "printf" ++
                ret ++

                label "new_line" ++
                movq (ilab ".Snew_line") (reg rdi) ++
                xorq (reg rax) (reg rax) ++
                call "printf" ++
                ret ++

                label "character'val" ++
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
