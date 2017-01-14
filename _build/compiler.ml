open Utils
open Ast_common
open Ast_typed
open X86_64


let () = allow_comments false

let get_unique_label =
  let id = ref 0 in
  (fun () -> let l = Format.sprintf "L_%n" !id in
    incr id;
    l)

let imm_8   n = imm (8*n)
let ind_8 ?(ofs) reg = ind ?ofs:(match ofs with
                                | None -> None
                                | Some ofs -> Some (8*ofs)) reg
let pushn n = if n = 0 then nop else subq (imm_8 n) (reg rsp)
let popn  n = if n = 0 then nop else addq (imm_8 n) (reg rsp)

let rec iter n code =
  if n = 0 then nop else code ++ (iter (n - 1) code)

let copy from_addr to_addr size =
  let rec aux ofs acc =
    if ofs = -1 then acc else
      let code = movq (ind_8 ~ofs from_addr) (reg r15) ++
        movq (reg r15) (ind_8 ~ofs to_addr) in
      aux (ofs - 1) (acc ++ code)
  in
  aux (size - 1) nop

let follow_rbp n =
  movq (reg rbp) (reg rsi) ++ iter n (movq (ind_8 ~ofs:2 rsi) (reg rsi))
                       

let compile_const c =
  let n = match c with
    | Cint n -> n
    | Cchar c -> Char.code c
    | Cbool b -> if b then 1 else 0
    | Cnull -> 0
  in
  pushq (imm n)

let rec compile_expr_no_comment ~push e =
  match e.desc with
  (* Result pushed on the top of the stack
     if push = false, the address of the left_value is stored in %rsi *)
  | Econst c ->
    assert push; compile_const c
      
  | Eident id ->
    follow_rbp id.level ++ addq (imm_8 id.offset) (reg rsi) ++
    (if id.is_reference then movq (ind_8 rsi) (reg rsi) else nop) ++
    (if push then pushn id.size ++ copy rsi rsp id.size else nop)
    
  | Ederef er ->
    compile_expr ~push:false er ++ movq (ind_8 rsi) (reg rsi) ++
    (if push then pushn e.deco ++ copy rsi rsp e.deco else nop)

  | Emember (ef, offs) ->
    if not push then
      compile_expr ~push:false ef ++ addq (imm_8 offs) (reg rsi)
    else compile_expr ~push:true ef ++
         popn offs ++ movq (reg rsp) (reg rsi) ++
         addq (imm_8 (ef.deco - offs - e.deco)) (reg rsi) ++ copy rsp rsi e.deco ++
         popn (ef.deco - offs - e.deco)
    
  | Ebinop (e1, o, e2) ->
    assert push;
    let c1 = compile_expr ~push:true e1 in
    let c2 = compile_expr ~push:true e2 in
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
        if i = -1 then acc
        else let code = movq (ind_8 ~ofs:i rsp) (reg rax) ++
                        movq (ind_8 ~ofs:(i + size) rsp) (reg rbx) ++
                        cmpq (reg rbx) (reg rax) ++ jmp_if_not l in
          compare (i - 1) (code ++ acc)
      in
      c1 ++ c2 ++ movq (imm 0) (reg rcx) ++ compare (size - 1) nop ++
      movq (imm 1) (reg rcx) ++ label l ++ popn (2*size) ++ pushq (reg rcx)
    in
    
    let lazy_code default jmp_op =
      let l = get_unique_label () in
      c1 ++ popq rax ++ movq (imm default) (reg rcx) ++
      testq (reg rax) (reg rax) ++
      jmp_op l ++ c2 ++ popq rcx ++ label l ++ pushq (reg rcx)
    in
    
    let arith_code arith_op =
      (* performs e1 op e2 *)
      ce ++ arith_op (reg ebx) (reg eax) ++ movslq (reg eax) rax ++
      pushq (reg rax)
    in
    let div_code want_rem =
      let (r, rl) = if want_rem then (rdx, edx) else (rax, eax) in
      ce ++ cltd ++ idivl (reg ebx) ++ movslq (reg rl) r ++ pushq (reg r)
    in
    
    begin match o with
      | Beq  -> equality_code jne (* beware, it is inversed *)
      | Bneq -> equality_code je
      | Blt  -> comparator_code jl
      | Bleq -> comparator_code jle
      | Bgt  -> comparator_code jg
      | Bgeq -> comparator_code jge
                  
      | Bplus  -> arith_code addl
      | Bminus -> arith_code subl
      | Btimes -> arith_code imull
      | Bdiv   -> div_code false
      | Brem   -> div_code true
                    
      | Band -> arith_code andl
      | Bor  -> arith_code orl
                  
      | Band_then -> lazy_code 0 je
      | Bor_else  -> lazy_code 1 jne
    end
    
  | Enot e -> (* not x = 1 - x since x is 0 or 1 *)
    assert push;
    compile_expr ~push:true e ++ popq rax ++ negq (reg rax) ++ incq (reg rax)
                                                          
  | Enew size ->
    assert push;
    movq (imm_8 size) (reg rdi) ++ call "malloc" ++ pushq (reg rax)
                                                          
  | Eapp_func (f, args) ->
    compile_call f args ++ (if push then nop else (* TODO memory leak *)
                              movq (reg rsp) (reg rsi) ++ popn 1)

and compile_expr ~push e =
  let s = (Printer.print_to_string print_expr e) ^
          (if push then " (PUSH)" else " (RDI)") in
  comment s ++ compile_expr_no_comment ~push e ++ comment ("end " ^ s)


and compile_call p args =
  let (args_code, size) =
    List.fold_left2 (fun (c, s) m e -> match m with
                      | In -> (compile_expr ~push:true e ++ c, s + e.deco)
                      | InOut -> (compile_expr ~push:false e ++
                                  pushq (reg rsi) ++ c, s + 1))
      (nop, 0) p.modes args in
  pushn p.size_ret ++ args_code ++ follow_rbp p.level ++ pushq (reg rsi) ++
  call p.name ++ popn (1 + size)


type return =
  | Ret_space of int
  | Rax
      

let rec compile_stmt_no_comment ret_type s =
  match s with
  | Saffect (lv, e) ->    
    let code = compile_expr ~push:false lv in
    code ++ pushq (reg rsi) ++ compile_expr ~push:true e ++
    movq (ind_8 ~ofs:e.deco rsp) (reg rsi) ++ copy rsp rsi e.deco ++
    popn (1 + e.deco)
                                     
  | Scall_proc (p, args) ->
    compile_call p args

  | Sreturn e ->
    compile_expr ~push:true e ++
    (match ret_type with
     | Rax ->
       popq rax
         
     | Ret_space ret_ofs ->
       movq (reg rbp) (reg rsi) ++ addq (imm_8 ret_ofs) (reg rsi) ++
       copy rsp rsi e.deco) ++
    movq (reg rbp) (reg rsp) ++ popq rbp ++ ret

  | Sblock b ->
    assert false

  | Scond (cond, s1, s2) ->
    let l_else = get_unique_label () in
    let l_end = get_unique_label () in
    compile_expr ~push:true cond ++ popq rax ++ testq (reg rax) (reg rax) ++
    je l_else ++ compile_stmt ret_type s1 ++ jmp l_end ++
    label l_else ++ compile_stmt ret_type s2 ++ label l_end

  | Swhile (loop, s) ->
    let l_cond = get_unique_label () in
    let l_loop = get_unique_label () in
    jmp l_cond ++ label l_loop ++ compile_stmt ret_type s ++
    label l_cond ++ compile_expr ~push:true loop ++ popq rax ++
    testq (reg rax) (reg rax) ++ jne l_loop

and compile_stmt ret_type = function
  | Sblock b ->
    List.fold_left (fun c s -> c ++ compile_stmt ret_type s) nop b

  | s ->
    let c = Printer.print_to_string print_stmt s in
    comment c ++ compile_stmt_no_comment ret_type s ++ comment ("end " ^ c)


let rec compile_decl (codefun, codemain) = function
  (* | Dtype_decl (t, ta_opt) ->
     failwith "Not implemented"

     | Drecord_def (r, fs) ->
     failwith "Not implemented"
  *)
  | Dvar_decl (offset, e) ->
    let lv = Eident { is_reference = false; size = e.deco; level = 0; offset } in
    (codefun, codemain ++
              compile_stmt Rax (Saffect (decorate lv e.deco, e)))

  | Dproc_func pf ->
    let (cf, cm) = compile_decls pf.decls in
    let ret_type = if pf.ret = 0 then Rax else Ret_space pf.ret in
    let code = label pf.pf_sig.name ++ pushq (reg rbp) ++ movq (reg rsp) (reg rbp) ++
               pushn pf.frame ++ comment "decls" ++ cm ++
               comment "stmts" ++ compile_stmt ret_type pf.stmt in
    (codefun ++ code ++ cf, codemain)

and compile_decls ds =
  List.fold_left compile_decl (nop, nop) ds


let compile_program p file =
  let (codefun, codemain) = compile_decls [Dproc_func p] in
  let asm = { text =
                glabel "main" ++
                jmp p.pf_sig.name ++

                codemain ++ codefun ++

                label "put" ++
                movslq (ind_8 ~ofs:2 rsp) rsi ++
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
                movslq (ind_8 ~ofs:2 rsp) rax ++ movq (reg rax) (ind_8 ~ofs:3 rsp) ++
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
