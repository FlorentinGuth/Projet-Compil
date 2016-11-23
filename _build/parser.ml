
module Basics = struct
  
  exception Error
  
  type token = 
    | WITH
    | WHILE
    | USE
    | UNARY_MINUS
    | TYPE
    | TIMES
    | THEN
    | SEMICOLON
    | RPAREN
    | REVERSE
    | RETURN
    | REM
    | RECORD
    | PUT
    | PROC
    | PLUS
    | OUT
    | OR_ELSE
    | OR
    | NULL
    | NOT
    | NEW_LINE
    | NEW
    | NEQ
    | MINUS
    | LT
    | LPAREN
    | LOOP
    | LEQ
    | IS
    | INT of (int)
    | IN
    | IF
    | IDENT of (string)
    | GT
    | GEQ
    | FUNC
    | FOR
    | EQ
    | EOF
    | END
    | ELSIF
    | ELSE
    | DOTDOT
    | DOT
    | DIV
    | COMMA
    | COLON
    | CHAR_VAL
    | CHAR of (char)
    | BOOL of (bool)
    | BEGIN
    | AND_THEN
    | AND
    | AFFECT
    | ACCESS
  
end

include Basics

let _eRR =
  Basics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState206
  | MenhirState202
  | MenhirState191
  | MenhirState188
  | MenhirState187
  | MenhirState185
  | MenhirState184
  | MenhirState176
  | MenhirState174
  | MenhirState170
  | MenhirState166
  | MenhirState162
  | MenhirState158
  | MenhirState157
  | MenhirState155
  | MenhirState153
  | MenhirState149
  | MenhirState148
  | MenhirState146
  | MenhirState140
  | MenhirState135
  | MenhirState134
  | MenhirState132
  | MenhirState131
  | MenhirState118
  | MenhirState114
  | MenhirState113
  | MenhirState111
  | MenhirState109
  | MenhirState107
  | MenhirState105
  | MenhirState103
  | MenhirState101
  | MenhirState99
  | MenhirState97
  | MenhirState96
  | MenhirState94
  | MenhirState92
  | MenhirState90
  | MenhirState87
  | MenhirState85
  | MenhirState80
  | MenhirState76
  | MenhirState73
  | MenhirState72
  | MenhirState70
  | MenhirState69
  | MenhirState67
  | MenhirState65
  | MenhirState61
  | MenhirState59
  | MenhirState54
  | MenhirState47
  | MenhirState45
  | MenhirState44
  | MenhirState42
  | MenhirState41
  | MenhirState38
  | MenhirState34
  | MenhirState31
  | MenhirState26
  | MenhirState25
  | MenhirState18
  | MenhirState13
  | MenhirState10
  | MenhirState8
  | MenhirState5
  | MenhirState1
  
  open Ast

  exception Parser_error of string
  

  let developp (l, y) = 
    List.map (fun x -> (x, y)) l

  let developp_full l =
    List.flatten (List.map developp l)

  let verify x = function
    | None -> true
    | Some y -> x = y

 let to_some default = function
   | None -> default
   | Some x -> x
 let option_to_list = to_some []

 let decorate desc deco = 
   { desc; deco }

let rec _menhir_goto_list_pair_preceded_ELSIF_expr__preceded_THEN_stmts___ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((unit Ast.expr * unit Ast.stmt) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState188 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _, (x00 : (unit Ast.expr))), _, (x10 : (unit Ast.stmt))), _, (xs : ((unit Ast.expr * unit Ast.stmt) list))) = _menhir_stack in
        let _110 = () in
        let _100 = () in
        let _v : ((unit Ast.expr * unit Ast.stmt) list) = let x =
          let x1 = x10 in
          let _11 = _110 in
          let x0 = x00 in
          let _10 = _100 in
          let y =
            let x = x1 in
            let _1 = _11 in
                ( x )
          in
          let x =
            let x = x0 in
            let _1 = _10 in
                ( x )
          in
              ( (x, y) )
        in
            ( x :: xs ) in
        _menhir_goto_list_pair_preceded_ELSIF_expr__preceded_THEN_stmts___ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState184 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState191
            | BOOL _v ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _v
            | CHAR _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _v
            | CHAR_VAL ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState191
            | FOR ->
                _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState191
            | IDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _v
            | IF ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState191
            | INT _v ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _v
            | LPAREN ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState191
            | MINUS ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState191
            | NEW ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState191
            | NEW_LINE ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState191
            | NOT ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState191
            | NULL ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState191
            | PUT ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState191
            | RETURN ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState191
            | WHILE ->
                _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState191
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState191)
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _v : (unit Ast.stmt option) =     ( None ) in
            _menhir_goto_option_preceded_ELSE_stmts__ _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_decl_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit Ast.decl list list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (ds : (unit Ast.decl list list)) = _v in
        let _v : (unit Ast.decl list) =                     ( List.flatten ds ) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState131
            | BOOL _v ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _v
            | CHAR _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _v
            | CHAR_VAL ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState131
            | FOR ->
                _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState131
            | IDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _v
            | IF ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState131
            | INT _v ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _v
            | LPAREN ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState131
            | MINUS ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState131
            | NEW ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState131
            | NEW_LINE ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState131
            | NOT ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState131
            | NULL ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState131
            | PUT ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState131
            | RETURN ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState131
            | WHILE ->
                _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState131
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState131)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState206 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (unit Ast.decl list list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (unit Ast.decl list))) = _menhir_stack in
        let _v : (unit Ast.decl list list) =     ( x :: xs ) in
        _menhir_goto_list_decl_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_preceded_ELSE_stmts__ : _menhir_env -> 'ttv_tail -> (unit Ast.stmt option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | END ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMICOLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((((_menhir_stack, _menhir_s), _, (e : (unit Ast.expr))), _, (s1 : (unit Ast.stmt))), _, (elsif : ((unit Ast.expr * unit Ast.stmt) list))), (else_opt : (unit Ast.stmt option))) = _menhir_stack in
                let _9 = () in
                let _8 = () in
                let _7 = () in
                let _3 = () in
                let _1 = () in
                let _v : (unit Ast.stmt) =     ( let rec fold = function
        | [] -> to_some (decorate (Sblock []) ()) else_opt
        | (e, s) :: tl -> decorate (Scond (e, s, fold tl)) ()
      in
      decorate (Scond (e, s1, fold elsif)) ()) in
                _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce43 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((unit Ast.expr * unit Ast.stmt) list) =     ( [] ) in
    _menhir_goto_list_pair_preceded_ELSIF_expr__preceded_THEN_stmts___ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run185 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _v
    | CHAR _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _v
    | CHAR_VAL ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState185
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _v
    | INT _v ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _v
    | LPAREN ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState185
    | MINUS ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState185
    | NEW ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState185
    | NOT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState185
    | NULL ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState185
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState185

and _menhir_reduce34 : _menhir_env -> ((('ttv_tail * _menhir_state * (unit Ast.ident))) * _menhir_state * (unit Ast.expr list)) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, (f : (unit Ast.ident))), _, (ps : (unit Ast.expr list))) = _menhir_stack in
    let _4 = () in
    let _2 = () in
    let _v : (unit Ast.expr_desc) =     ( Eapp_func (f, ps) ) in
    _menhir_goto_expr_desc _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce41 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit Ast.decl list list) =     ( [] ) in
    _menhir_goto_list_decl_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run42 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42

and _menhir_run59 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59

and _menhir_run61 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61

and _menhir_goto_nonempty_list_stmt_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit Ast.stmt list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState162 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (unit Ast.stmt list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (unit Ast.stmt))) = _menhir_stack in
        let _v : (unit Ast.stmt list) =     ( x :: xs ) in
        _menhir_goto_nonempty_list_stmt_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState131 | MenhirState134 | MenhirState148 | MenhirState191 | MenhirState187 | MenhirState157 | MenhirState158 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (l : (unit Ast.stmt list)) = _v in
        let _v : (unit Ast.stmt) =                             ( decorate (Sblock l) () ) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        (match _menhir_s with
        | MenhirState158 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | END ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | SEMICOLON ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s), _, (s : (unit Ast.stmt))) = _menhir_stack in
                    let _4 = () in
                    let _3 = () in
                    let _1 = () in
                    let _v : (unit Ast.stmt) =                                      ( decorate (s.desc) () ) in
                    _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState157 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | END ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | LOOP ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | SEMICOLON ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let ((((((_menhir_stack, _menhir_s), _, (i : (unit Ast.ident))), (r : (unit option))), _, (el : (unit Ast.expr))), _, (eu : (unit Ast.expr))), _, (s : (unit Ast.stmt))) = _menhir_stack in
                        let _12 = () in
                        let _11 = () in
                        let _10 = () in
                        let _8 = () in
                        let _6 = () in
                        let _3 = () in
                        let _1 = () in
                        let _v : (unit Ast.stmt) =     ( let r = match r with
        | None -> false
        | Some () -> true
      in
      decorate (Sfor (i, r, el, eu, s)) () ) in
                        _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState148 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ELSIF ->
                _menhir_run185 _menhir_env (Obj.magic _menhir_stack) MenhirState184
            | ELSE | END ->
                _menhir_reduce43 _menhir_env (Obj.magic _menhir_stack) MenhirState184
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState184)
        | MenhirState187 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ELSIF ->
                _menhir_run185 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | ELSE | END ->
                _menhir_reduce43 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState188)
        | MenhirState191 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, (x0 : (unit Ast.stmt))) = _menhir_stack in
            let _10 = () in
            let _v : (unit Ast.stmt option) = let x =
              let x = x0 in
              let _1 = _10 in
                  ( x )
            in
                ( Some x ) in
            _menhir_goto_option_preceded_ELSE_stmts__ _menhir_env _menhir_stack _v
        | MenhirState134 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | END ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | LOOP ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | SEMICOLON ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (((_menhir_stack, _menhir_s), _, (e : (unit Ast.expr))), _, (s : (unit Ast.stmt))) = _menhir_stack in
                        let _7 = () in
                        let _6 = () in
                        let _5 = () in
                        let _3 = () in
                        let _1 = () in
                        let _v : (unit Ast.stmt) =     ( decorate (Swhile (e, s)) () ) in
                        _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState131 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | END ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | IDENT _v ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState202 _v
                | SEMICOLON ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_s = MenhirState202 in
                    let _v : (unit Ast.ident option) =     ( None ) in
                    _menhir_goto_option_ident_ _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState202)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            _menhir_fail ())
    | _ ->
        _menhir_fail ()

and _menhir_goto_const : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.const) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (c : (Ast.const)) = _v in
    let _v : (unit Ast.expr_desc) =               ( Econst c ) in
    _menhir_goto_expr_desc _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_option_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit Ast.expr option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMICOLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (e : (unit Ast.expr option))) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : (unit Ast.stmt) =    ( decorate (Sreturn (to_some (decorate (Econst Cnull) ()) e)) () ) in
        _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run132 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
    | CHAR _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
    | CHAR_VAL ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
    | INT _v ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
    | LPAREN ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | MINUS ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | NEW ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | NOT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | NULL ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState132
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState132

and _menhir_run135 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v
    | CHAR _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v
    | CHAR_VAL ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState135
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v
    | INT _v ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v
    | LPAREN ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState135
    | MINUS ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState135
    | NEW ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState135
    | NOT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState135
    | NULL ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState135
    | SEMICOLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState135 in
        let _v : (unit Ast.expr option) =     ( None ) in
        _menhir_goto_option_expr_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState135

and _menhir_run139 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL _v ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
        | CHAR _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
        | CHAR_VAL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | IDENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
        | INT _v ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
        | LPAREN ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | MINUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | NEW ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | NOT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | NULL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState140)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run144 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMICOLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : (unit Ast.stmt) =                         ( decorate Snew_line () ) in
        _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run146 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
    | CHAR _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
    | CHAR_VAL ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState146
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
    | INT _v ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
    | LPAREN ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState146
    | MINUS ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState146
    | NEW ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState146
    | NOT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState146
    | NULL ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState146
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState146

and _menhir_run149 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState149

and _menhir_run158 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | BOOL _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _v
    | CHAR _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _v
    | CHAR_VAL ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | FOR ->
        _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _v
    | IF ->
        _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | INT _v ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _v
    | LPAREN ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | MINUS ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | NEW ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | NEW_LINE ->
        _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | NOT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | NULL ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | PUT ->
        _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | RETURN ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | WHILE ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState158

and _menhir_goto_separated_nonempty_list_COMMA_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (unit Ast.expr))), _, (xs : (unit Ast.expr list))) = _menhir_stack in
        let _2 = () in
        let _v : (unit Ast.expr list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState166 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMICOLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, (p : (unit Ast.ident))), _, (ps : (unit Ast.expr list))) = _menhir_stack in
                let _5 = () in
                let _4 = () in
                let _2 = () in
                let _v : (unit Ast.stmt) =     ( decorate (Scall_proc (p, ps)) () ) in
                _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
            | AND | DIV | DOT | EQ | GEQ | GT | LEQ | LT | MINUS | NEQ | OR | PLUS | REM | TIMES ->
                _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run85 : _menhir_env -> 'ttv_tail * _menhir_state * (unit Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | CHAR _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | CHAR_VAL ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | INT _v ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | LPAREN ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | MINUS ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | NEW ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | NOT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | NULL ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85

and _menhir_run90 : _menhir_env -> 'ttv_tail * _menhir_state * (unit Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | CHAR _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | CHAR_VAL ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | INT _v ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | LPAREN ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | MINUS ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | NEW ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | NOT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | NULL ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90

and _menhir_run92 : _menhir_env -> 'ttv_tail * _menhir_state * (unit Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | CHAR _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | CHAR_VAL ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | INT _v ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | LPAREN ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | MINUS ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | NEW ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | NOT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | NULL ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92

and _menhir_run96 : _menhir_env -> 'ttv_tail * _menhir_state * (unit Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | CHAR _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | CHAR_VAL ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | ELSE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState96 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL _v ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
        | CHAR _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
        | CHAR_VAL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | IDENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
        | INT _v ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
        | LPAREN ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | MINUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | NEW ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | NOT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | NULL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97)
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | INT _v ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | LPAREN ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | MINUS ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | NEW ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | NOT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | NULL ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96

and _menhir_run99 : _menhir_env -> 'ttv_tail * _menhir_state * (unit Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
    | CHAR _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
    | CHAR_VAL ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
    | INT _v ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
    | LPAREN ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | MINUS ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | NEW ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | NOT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | NULL ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99

and _menhir_run101 : _menhir_env -> 'ttv_tail * _menhir_state * (unit Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
    | CHAR _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
    | CHAR_VAL ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
    | INT _v ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
    | LPAREN ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | MINUS ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | NEW ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | NOT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | NULL ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101

and _menhir_run103 : _menhir_env -> 'ttv_tail * _menhir_state * (unit Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | CHAR _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | CHAR_VAL ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | INT _v ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | LPAREN ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | MINUS ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | NEW ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | NOT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | NULL ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103

and _menhir_run105 : _menhir_env -> 'ttv_tail * _menhir_state * (unit Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
    | CHAR _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
    | CHAR_VAL ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
    | INT _v ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
    | LPAREN ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | MINUS ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | NEW ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | NOT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | NULL ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105

and _menhir_run107 : _menhir_env -> 'ttv_tail * _menhir_state * (unit Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
    | CHAR _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
    | CHAR_VAL ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
    | INT _v ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
    | LPAREN ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | MINUS ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | NEW ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | NOT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | NULL ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107

and _menhir_run109 : _menhir_env -> 'ttv_tail * _menhir_state * (unit Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
    | CHAR _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
    | CHAR_VAL ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
    | INT _v ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
    | LPAREN ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | MINUS ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | NEW ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | NOT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | NULL ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109

and _menhir_run111 : _menhir_env -> 'ttv_tail * _menhir_state * (unit Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
    | CHAR _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
    | CHAR_VAL ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
    | INT _v ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
    | LPAREN ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | MINUS ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | NEW ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | NOT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | NULL ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111

and _menhir_run87 : _menhir_env -> 'ttv_tail * _menhir_state * (unit Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87

and _menhir_run94 : _menhir_env -> 'ttv_tail * _menhir_state * (unit Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | CHAR _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | CHAR_VAL ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | INT _v ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | LPAREN ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | MINUS ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | NEW ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | NOT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | NULL ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94

and _menhir_run113 : _menhir_env -> 'ttv_tail * _menhir_state * (unit Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
    | CHAR _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
    | CHAR_VAL ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
    | INT _v ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
    | LPAREN ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | MINUS ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | NEW ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | NOT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | NULL ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | THEN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState113 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL _v ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
        | CHAR _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
        | CHAR_VAL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | IDENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
        | INT _v ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
        | LPAREN ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | MINUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | NEW ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | NOT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | NULL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113

and _menhir_goto_decl : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit Ast.decl list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FUNC ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState206
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState206 _v
    | PROC ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState206
    | TYPE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState206
    | BEGIN ->
        _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState206
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState206

and _menhir_goto_mode : _menhir_env -> 'ttv_tail -> (Ast.mode) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ACCESS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25

and _menhir_goto_option_preceded_AFFECT_expr__ : _menhir_env -> 'ttv_tail -> (unit Ast.expr option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMICOLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (lv_list : (unit Ast.ident list))), _, (typ : (unit Ast.type_annot))), (e : (unit Ast.expr option))) = _menhir_stack in
        let _5 = () in
        let _2 = () in
        let _v : (unit Ast.decl list) =     ( List.map (fun lv -> Dvar_decl (lv, typ, e)) lv_list ) in
        _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_nonempty_list_fields_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((unit Ast.ident * unit Ast.type_annot) list list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RECORD ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | SEMICOLON ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((((_menhir_stack, _menhir_s), _, (id : (unit Ast.ident))), _), _, (r : ((unit Ast.ident * unit Ast.type_annot) list list))) = _menhir_stack in
                    let _8 = () in
                    let _7 = () in
                    let _6 = () in
                    let _4 = () in
                    let _3 = () in
                    let _1 = () in
                    let _v : (unit Ast.decl list) =     ( [Dtype_record (id, List.flatten r)] ) in
                    _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : ((unit Ast.ident * unit Ast.type_annot) list))), _, (xs : ((unit Ast.ident * unit Ast.type_annot) list list))) = _menhir_stack in
        let _v : ((unit Ast.ident * unit Ast.type_annot) list list) =     ( x :: xs ) in
        _menhir_goto_nonempty_list_fields_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_SEMICOLON_param_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit Ast.param list list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, (p : (unit Ast.param list list))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (unit Ast.param list) =     ( List.flatten p ) in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (x : (unit Ast.param list)) = _v in
            let _v : (unit Ast.param list option) =     ( Some x ) in
            _menhir_goto_option_params_ _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (unit Ast.param list))), _, (xs : (unit Ast.param list list))) = _menhir_stack in
        let _2 = () in
        let _v : (unit Ast.param list list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_SEMICOLON_param_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_preceded_RETURN_type_annot__ : _menhir_env -> 'ttv_tail -> (unit Ast.type_annot option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IS ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FUNC ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | IDENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
        | PROC ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | TYPE ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | BEGIN ->
            _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_option_ident_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit Ast.ident option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMICOLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((((_menhir_stack, _menhir_s, (name : (unit Ast.ident))), (ps : (unit Ast.param list option))), (rt : (unit Ast.type_annot option))), _, (decls : (unit Ast.decl list))), _, (stmt : (unit Ast.stmt))), _, (end_id : (unit Ast.ident option))) = _menhir_stack in
        let _10 = () in
        let _8 = () in
        let _6 = () in
        let _4 = () in
        let _v : ((unit, unit Ast.type_annot option) Ast.proc_func) =     ( if name <> to_some name end_id
      then
        raise
          (Parser_error
             "Ident following \"end\" must have same name than the function/procedure");
      { name;
        params = option_to_list ps;
        return = rt;
        decls;
        stmt } ) in
        (match _menhir_s with
        | MenhirState59 | MenhirState13 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (p : ((unit, unit Ast.type_annot option) Ast.proc_func)) = _v in
            let _v : (unit Ast.proc) =                      ( assert (p.return = None); { p with return = () } ) in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            (match _menhir_s with
            | MenhirState13 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | EOF ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (((((_menhir_stack, _, (ada1 : (unit Ast.ident))), _, (text_io1 : (unit Ast.ident))), _, (ada2 : (unit Ast.ident))), _, (text_io2 : (unit Ast.ident))), _, (p : (unit Ast.proc))) = _menhir_stack in
                    let _13 = () in
                    let _11 = () in
                    let _10 = () in
                    let _8 = () in
                    let _6 = () in
                    let _5 = () in
                    let _3 = () in
                    let _1 = () in
                    let _v : (unit Ast.proc) =     ( if not (ada1.desc = "ada" && text_io1.desc = "text_io" &&
        ada2.desc = "ada" && text_io2.desc = "text_io")
      then raise
             (Parser_error
                "Wrong header, should be \"with Ada.Text_IO; use Ada.Text_IO;\"");
      if p.params <> [] then raise
                               (Parser_error
                                  "Main prcedure can't take any parameters");
      p ) in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_1 : (unit Ast.proc)) = _v in
                    Obj.magic _1
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | MenhirState59 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _, (p : (unit Ast.proc))) = _menhir_stack in
                let _1 = () in
                let _v : (unit Ast.decl list) =                    ( [Dproc p] ) in
                _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                _menhir_fail ())
        | MenhirState61 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (f : ((unit, unit Ast.type_annot option) Ast.proc_func)) = _v in
            let _v : (unit Ast.func) =                      ( match f.return with
                       | Some t -> { f with return = t }
                       | None ->
                          raise
                            (Parser_error
                               "A function must have an annotated return type") ) in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (f : (unit Ast.func)) = _v in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            let _1 = () in
            let _v : (unit Ast.decl list) =                    ( [Dfunc f] ) in
            _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            _menhir_fail ())
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_stmt : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit Ast.stmt) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | BOOL _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _v
    | CHAR _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _v
    | CHAR_VAL ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | FOR ->
        _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _v
    | IF ->
        _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | INT _v ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _v
    | LPAREN ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | MINUS ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | NEW ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | NEW_LINE ->
        _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | NOT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | NULL ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | PUT ->
        _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | RETURN ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | WHILE ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | ELSE | ELSIF | END ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (x : (unit Ast.stmt))) = _menhir_stack in
        let _v : (unit Ast.stmt list) =     ( [ x ] ) in
        _menhir_goto_nonempty_list_stmt_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState162

and _menhir_goto_option_REVERSE_ : _menhir_env -> 'ttv_tail -> (unit option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
    | CHAR _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
    | CHAR_VAL ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState153
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
    | INT _v ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
    | LPAREN ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState153
    | MINUS ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState153
    | NEW ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState153
    | NOT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState153
    | NULL ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState153
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState153

and _menhir_reduce15 : _menhir_env -> (('ttv_tail * _menhir_state * (unit Ast.expr))) * _menhir_state * (unit Ast.ident) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, (e0 : (unit Ast.expr))), _, (field0 : (unit Ast.ident))) = _menhir_stack in
    let _20 = () in
    let _v : (unit Ast.expr_desc) = let lv =
      let field = field0 in
      let _2 = _20 in
      let e = e0 in
                                       ( Lmember (e, field) )
    in
                      ( Eleft_val lv ) in
    _menhir_goto_expr_desc _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce14 : _menhir_env -> 'ttv_tail * _menhir_state * (unit Ast.ident) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (id0 : (unit Ast.ident))) = _menhir_stack in
    let _v : (unit Ast.expr_desc) = let lv =
      let id = id0 in
                     ( Lident id )
    in
                      ( Eleft_val lv ) in
    _menhir_goto_expr_desc _menhir_env _menhir_stack _menhir_s _v

and _menhir_run68 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.const) =              ( Cnull ) in
    _menhir_goto_const _menhir_env _menhir_stack _menhir_s _v

and _menhir_run69 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | CHAR _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | CHAR_VAL ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | INT _v ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | LPAREN ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | MINUS ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | NEW ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | NOT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | NULL ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69

and _menhir_run70 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70

and _menhir_run72 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | CHAR _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | CHAR_VAL ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | INT _v ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | LPAREN ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | MINUS ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | NEW ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | NOT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | NULL ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72

and _menhir_run73 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | CHAR _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | CHAR_VAL ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | INT _v ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | LPAREN ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | MINUS ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | NEW ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | NOT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | NULL ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73

and _menhir_run74 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (n : (int)) = _v in
    let _v : (Ast.const) =              ( Cint  n ) in
    _menhir_goto_const _menhir_env _menhir_stack _menhir_s _v

and _menhir_run75 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL _v ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
        | CHAR _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
        | CHAR_VAL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | IDENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
        | INT _v ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
        | LPAREN ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | MINUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | NEW ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | NOT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | NULL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run77 : _menhir_env -> 'ttv_tail -> _menhir_state -> (char) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (c : (char)) = _v in
    let _v : (Ast.const) =              ( Cchar c ) in
    _menhir_goto_const _menhir_env _menhir_stack _menhir_s _v

and _menhir_run78 : _menhir_env -> 'ttv_tail -> _menhir_state -> (bool) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (b : (bool)) = _v in
    let _v : (Ast.const) =              ( Cbool b ) in
    _menhir_goto_const _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_expr_desc : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit Ast.expr_desc) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (x0 : (unit Ast.expr_desc)) = _v in
    let _v : (unit Ast.expr) = let e =
      let x = x0 in
                ( decorate x () )
    in
                                 ( e ) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState166 | MenhirState118 | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | CHAR _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | CHAR_VAL ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | IDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | INT _v ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | LPAREN ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | MINUS ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | NEW ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | NOT ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | NULL ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118)
        | DIV ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (unit Ast.expr))) = _menhir_stack in
            let _v : (unit Ast.expr list) =     ( [ x ] ) in
            _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DIV | DOTDOT | EQ | GEQ | GT | LEQ | LOOP | LT | MINUS | NEQ | OR | PLUS | REM | RPAREN | SEMICOLON | THEN | TIMES ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (unit Ast.expr))), _, (e2 : (unit Ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (unit Ast.expr_desc) =                                 ( Ebinop (e1, Btimes, e2) ) in
            _menhir_goto_expr_desc _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DIV | DOTDOT | EQ | GEQ | GT | LEQ | LOOP | LT | MINUS | NEQ | OR | PLUS | REM | RPAREN | SEMICOLON | THEN | TIMES ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (unit Ast.expr))), _, (e2 : (unit Ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (unit Ast.expr_desc) =                               ( Ebinop (e1, Brem, e2) ) in
            _menhir_goto_expr_desc _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DOTDOT | EQ | GEQ | GT | LEQ | LOOP | LT | MINUS | NEQ | OR | PLUS | RPAREN | SEMICOLON | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (unit Ast.expr))), _, (e2 : (unit Ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (unit Ast.expr_desc) =                                ( Ebinop (e1, Bplus, e2) ) in
            _menhir_goto_expr_desc _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DIV | DOTDOT | EQ | GEQ | GT | LEQ | LOOP | LT | MINUS | NEQ | OR | PLUS | REM | RPAREN | SEMICOLON | THEN | TIMES ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (unit Ast.expr))), _, (e2 : (unit Ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (unit Ast.expr_desc) =                               ( Ebinop (e1, Bdiv, e2) ) in
            _menhir_goto_expr_desc _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | DOTDOT | LOOP | OR | RPAREN | SEMICOLON | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (unit Ast.expr))), _), _, (e2 : (unit Ast.expr))) = _menhir_stack in
            let _3 = () in
            let _2 = () in
            let _v : (unit Ast.expr_desc) =                                    ( Ebinop (e1, Bor_else, e2) ) in
            _menhir_goto_expr_desc _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DOTDOT | LOOP | OR | RPAREN | SEMICOLON | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (unit Ast.expr))), _, (e2 : (unit Ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (unit Ast.expr_desc) =                               ( Ebinop (e1, Bneq, e2) ) in
            _menhir_goto_expr_desc _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DOTDOT | EQ | GEQ | GT | LEQ | LOOP | LT | MINUS | NEQ | OR | PLUS | RPAREN | SEMICOLON | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (unit Ast.expr))), _, (e2 : (unit Ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (unit Ast.expr_desc) =                                 ( Ebinop (e1, Bminus, e2) ) in
            _menhir_goto_expr_desc _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DOTDOT | EQ | LOOP | NEQ | OR | RPAREN | SEMICOLON | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (unit Ast.expr))), _, (e2 : (unit Ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (unit Ast.expr_desc) =                              ( Ebinop (e1, Blt, e2) ) in
            _menhir_goto_expr_desc _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DOTDOT | EQ | LOOP | NEQ | OR | RPAREN | SEMICOLON | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (unit Ast.expr))), _, (e2 : (unit Ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (unit Ast.expr_desc) =                               ( Ebinop (e1, Bleq, e2) ) in
            _menhir_goto_expr_desc _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DOTDOT | EQ | LOOP | NEQ | OR | RPAREN | SEMICOLON | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (unit Ast.expr))), _, (e2 : (unit Ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (unit Ast.expr_desc) =                              ( Ebinop (e1, Bgt, e2) ) in
            _menhir_goto_expr_desc _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DOTDOT | EQ | LOOP | NEQ | OR | RPAREN | SEMICOLON | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (unit Ast.expr))), _, (e2 : (unit Ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (unit Ast.expr_desc) =                               ( Ebinop (e1, Bgeq, e2) ) in
            _menhir_goto_expr_desc _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DOTDOT | LOOP | OR | RPAREN | SEMICOLON | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (unit Ast.expr))), _, (e2 : (unit Ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (unit Ast.expr_desc) =                              ( Ebinop (e1, Beq, e2) ) in
            _menhir_goto_expr_desc _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DOTDOT | LOOP | OR | RPAREN | SEMICOLON | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (unit Ast.expr))), _), _, (e2 : (unit Ast.expr))) = _menhir_stack in
            let _3 = () in
            let _2 = () in
            let _v : (unit Ast.expr_desc) =                                     ( Ebinop (e1, Band_then, e2) ) in
            _menhir_goto_expr_desc _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DOTDOT | LOOP | OR | RPAREN | SEMICOLON | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (unit Ast.expr))), _, (e2 : (unit Ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (unit Ast.expr_desc) =                               ( Ebinop (e1, Band, e2) ) in
            _menhir_goto_expr_desc _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | DOTDOT | LOOP | OR | RPAREN | SEMICOLON | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (unit Ast.expr))), _, (e2 : (unit Ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (unit Ast.expr_desc) =                              ( Ebinop (e1, Bor, e2) ) in
            _menhir_goto_expr_desc _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (unit Ast.expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (unit Ast.expr_desc) =                                        ( Echar_val e ) in
            _menhir_goto_expr_desc _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (unit Ast.expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (unit Ast.expr_desc) =                              ( e.desc ) in
            _menhir_goto_expr_desc _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DIV | DOTDOT | EQ | GEQ | GT | LEQ | LOOP | LT | MINUS | NEQ | OR | PLUS | REM | RPAREN | SEMICOLON | THEN | TIMES ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (unit Ast.expr))) = _menhir_stack in
            let _1 = () in
            let _v : (unit Ast.expr_desc) =                     ( Eunop (Uminus, e) ) in
            _menhir_goto_expr_desc _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DOTDOT | LOOP | OR | RPAREN | SEMICOLON | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (unit Ast.expr))) = _menhir_stack in
            let _1 = () in
            let _v : (unit Ast.expr_desc) =                   ( Eunop (Unot, e) ) in
            _menhir_goto_expr_desc _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, (x0 : (unit Ast.expr))) = _menhir_stack in
            let _10 = () in
            let _v : (unit Ast.expr option) = let x =
              let x = x0 in
              let _1 = _10 in
                  ( x )
            in
                ( Some x ) in
            _menhir_goto_option_preceded_AFFECT_expr__ _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState132 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LOOP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | BOOL _v ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
            | CHAR _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
            | CHAR_VAL ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | FOR ->
                _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | IDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
            | IF ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | INT _v ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
            | LPAREN ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | MINUS ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | NEW ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | NEW_LINE ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | NOT ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | NULL ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | PUT ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | RETURN ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | WHILE ->
                _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState134)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState135 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (unit Ast.expr))) = _menhir_stack in
            let _v : (unit Ast.expr option) =     ( Some x ) in
            _menhir_goto_option_expr_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState140 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMICOLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _, (e : (unit Ast.expr))) = _menhir_stack in
                let _5 = () in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : (unit Ast.stmt) =                                              ( decorate (Sput e) () ) in
                _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | TIMES ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState146 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | BOOL _v ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
            | CHAR _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
            | CHAR_VAL ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | FOR ->
                _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | IDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
            | IF ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | INT _v ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
            | LPAREN ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | MINUS ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | NEW ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | NEW_LINE ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | NOT ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | NULL ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | PUT ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | RETURN ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | WHILE ->
                _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState148)
        | TIMES ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState153 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | DOTDOT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
            | CHAR _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
            | CHAR_VAL ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | IDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
            | INT _v ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
            | LPAREN ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | MINUS ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | NEW ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | NOT ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | NULL ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState155)
        | EQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState155 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LOOP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | BOOL _v ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _v
            | CHAR _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _v
            | CHAR_VAL ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | FOR ->
                _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | IDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _v
            | IF ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | INT _v ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _v
            | LPAREN ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | MINUS ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | NEW ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | NEW_LINE ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | NOT ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | NULL ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | PUT ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | RETURN ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | WHILE ->
                _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState157)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState170 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (id0 : (unit Ast.ident))), _, (e : (unit Ast.expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (unit Ast.stmt) = let lv =
              let id = id0 in
                             ( Lident id )
            in
                                                           ( decorate (Saffect (lv, e)) () ) in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState131 | MenhirState134 | MenhirState148 | MenhirState191 | MenhirState187 | MenhirState157 | MenhirState158 | MenhirState162 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState174)
        | EQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState176 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e0 : (unit Ast.expr))), _, (field0 : (unit Ast.ident))), _, (e : (unit Ast.expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _20 = () in
            let _v : (unit Ast.stmt) = let lv =
              let field = field0 in
              let _2 = _20 in
              let e = e0 in
                                               ( Lmember (e, field) )
            in
                                                           ( decorate (Saffect (lv, e)) () ) in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState185 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | BOOL _v ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _v
            | CHAR _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _v
            | CHAR_VAL ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | FOR ->
                _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | IDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _v
            | IF ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | INT _v ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _v
            | LPAREN ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | MINUS ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | NEW ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | NEW_LINE ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | NOT ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | NULL ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | PUT ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | RETURN ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | WHILE ->
                _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState187
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState187)
        | TIMES ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_preceded_IS_type_annot__ : _menhir_env -> 'ttv_tail -> (unit Ast.type_annot option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMICOLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _, (id : (unit Ast.ident))), (typ : (unit Ast.type_annot option))) = _menhir_stack in
        let _4 = () in
        let _1 = () in
        let _v : (unit Ast.decl list) =     ( [Dtype (id, typ)] ) in
        _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run26 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26

and _menhir_goto_separated_nonempty_list_COMMA_ident_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit Ast.ident list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState31 | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | OUT ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _2 = () in
                    let _1 = () in
                    let _v : (Ast.mode) =             ( InOut ) in
                    _menhir_goto_mode _menhir_env _menhir_stack _v
                | ACCESS | IDENT _ ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _1 = () in
                    let _v : (Ast.mode) =          ( In ) in
                    _menhir_goto_mode _menhir_env _menhir_stack _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    raise _eRR)
            | ACCESS | IDENT _ ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _v : (Ast.mode) =          ( In ) in
                _menhir_goto_mode _menhir_env _menhir_stack _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (unit Ast.ident))), _, (xs : (unit Ast.ident list))) = _menhir_stack in
        let _2 = () in
        let _v : (unit Ast.ident list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_ident_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState54 | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ACCESS ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState47
            | IDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState206 | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ACCESS ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | IDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_type_annot : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit Ast.type_annot) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (ids : (unit Ast.ident list))), (m : (Ast.mode))), _, (typ : (unit Ast.type_annot))) = _menhir_stack in
        let _2 = () in
        let _v : (unit Ast.param list) =     ( List.map (fun id -> (id, m, typ)) ids ) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (unit Ast.param list))) = _menhir_stack in
            let _v : (unit Ast.param list list) =     ( [ x ] ) in
            _menhir_goto_separated_nonempty_list_SEMICOLON_param_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, (x0 : (unit Ast.type_annot))) = _menhir_stack in
        let _10 = () in
        let _v : (unit Ast.type_annot option) = let x =
          let x = x0 in
          let _1 = _10 in
              ( x )
        in
            ( Some x ) in
        _menhir_goto_option_preceded_RETURN_type_annot__ _menhir_env _menhir_stack _v
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (ids : (unit Ast.ident list))), _, (typ : (unit Ast.type_annot))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : ((unit Ast.ident * unit Ast.type_annot) list) =     ( developp (ids, typ) ) in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
            | END ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, (x : ((unit Ast.ident * unit Ast.type_annot) list))) = _menhir_stack in
                let _v : ((unit Ast.ident * unit Ast.type_annot) list list) =     ( [ x ] ) in
                _menhir_goto_nonempty_list_fields_ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, (x0 : (unit Ast.type_annot))) = _menhir_stack in
        let _10 = () in
        let _v : (unit Ast.type_annot option) = let x =
          let x = x0 in
          let _1 = _10 in
              ( x )
        in
            ( Some x ) in
        _menhir_goto_option_preceded_IS_type_annot__ _menhir_env _menhir_stack _v
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AFFECT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
            | CHAR _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
            | CHAR_VAL ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | IDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
            | INT _v ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
            | LPAREN ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | MINUS ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | NEW ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | NOT ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | NULL ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67)
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _v : (unit Ast.expr option) =     ( None ) in
            _menhir_goto_option_preceded_AFFECT_expr__ _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_params_ : _menhir_env -> 'ttv_tail -> (unit Ast.param list option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RETURN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ACCESS ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | IDENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38)
    | IS ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _v : (unit Ast.type_annot option) =     ( None ) in
        _menhir_goto_option_preceded_RETURN_type_annot__ _menhir_env _menhir_stack _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState206 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState202 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState191 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState188 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState187 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState185 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState184 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState176 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState174 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState170 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState166 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState162 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState158 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState157 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState155 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState153 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState149 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState148 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState146 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState140 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState135 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState134 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState132 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState131 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (id : (string)) = _v in
    let _v : (Ast.ident_desc) =                ( id ) in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (x0 : (Ast.ident_desc)) = _v in
    let _v : (unit Ast.ident) = let id =
      let x = x0 in
                ( decorate x () )
    in
                                   ( id ) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | USE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | IDENT _v ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | PROC ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | IDENT _v ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState61 | MenhirState59 | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18)
        | IS | RETURN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _v : (unit Ast.param list option) =     ( None ) in
            _menhir_goto_option_params_ _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (id : (unit Ast.ident))) = _menhir_stack in
        let _1 = () in
        let _v : (unit Ast.type_annot) =                        ( Aaccess id ) in
        _menhir_goto_type_annot _menhir_env _menhir_stack _menhir_s _v
    | MenhirState65 | MenhirState44 | MenhirState47 | MenhirState38 | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (id : (unit Ast.ident))) = _menhir_stack in
        let _v : (unit Ast.type_annot) =                ( Aident id ) in
        _menhir_goto_type_annot _menhir_env _menhir_stack _menhir_s _v
    | MenhirState206 | MenhirState41 | MenhirState54 | MenhirState45 | MenhirState18 | MenhirState34 | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34)
        | COLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (unit Ast.ident))) = _menhir_stack in
            let _v : (unit Ast.ident list) =     ( [ x ] ) in
            _menhir_goto_separated_nonempty_list_COMMA_ident_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ACCESS ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState44
            | IDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
            | RECORD ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState44 in
                let _menhir_stack = (_menhir_stack, _menhir_s) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | IDENT _v ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44)
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _v : (unit Ast.type_annot option) =     ( None ) in
            _menhir_goto_option_preceded_IS_type_annot__ _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (id : (unit Ast.ident))) = _menhir_stack in
        let _1 = () in
        let _v : (unit Ast.expr_desc) =                     ( Enew id ) in
        _menhir_goto_expr_desc _menhir_env _menhir_stack _menhir_s _v
    | MenhirState185 | MenhirState176 | MenhirState170 | MenhirState166 | MenhirState155 | MenhirState153 | MenhirState146 | MenhirState140 | MenhirState135 | MenhirState132 | MenhirState67 | MenhirState69 | MenhirState72 | MenhirState73 | MenhirState118 | MenhirState96 | MenhirState113 | MenhirState114 | MenhirState111 | MenhirState109 | MenhirState107 | MenhirState105 | MenhirState103 | MenhirState101 | MenhirState99 | MenhirState97 | MenhirState94 | MenhirState92 | MenhirState90 | MenhirState85 | MenhirState80 | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
            | CHAR _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
            | CHAR_VAL ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | IDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
            | INT _v ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
            | LPAREN ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | MINUS ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | NEW ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | NOT ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | NULL ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80)
        | AND | COMMA | DIV | DOT | DOTDOT | EQ | GEQ | GT | LEQ | LOOP | LT | MINUS | NEQ | OR | PLUS | REM | RPAREN | SEMICOLON | THEN | TIMES ->
            _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce15 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState149 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | REVERSE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let x = () in
                let _v : (unit option) =     ( Some x ) in
                _menhir_goto_option_REVERSE_ _menhir_env _menhir_stack _v
            | BOOL _ | CHAR _ | CHAR_VAL | IDENT _ | INT _ | LPAREN | MINUS | NEW | NOT | NULL ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _v : (unit option) =     ( None ) in
                _menhir_goto_option_REVERSE_ _menhir_env _menhir_stack _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState131 | MenhirState134 | MenhirState148 | MenhirState191 | MenhirState187 | MenhirState157 | MenhirState158 | MenhirState162 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AFFECT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _v
            | CHAR _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _v
            | CHAR_VAL ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState170
            | IDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _v
            | INT _v ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _v
            | LPAREN ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState170
            | MINUS ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState170
            | NEW ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState170
            | NOT ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState170
            | NULL ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState170
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState170)
        | LPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _v
            | CHAR _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _v
            | CHAR_VAL ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | IDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _v
            | INT _v ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _v
            | LPAREN ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | MINUS ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | NEW ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | NOT ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | NULL ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState166)
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (p : (unit Ast.ident))) = _menhir_stack in
            let _2 = () in
            let _v : (unit Ast.stmt) =                          ( decorate (Scall_proc (p, [])) () ) in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
        | AND | DIV | DOT | EQ | GEQ | GT | LEQ | LT | MINUS | NEQ | OR | PLUS | REM | TIMES ->
            _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState174 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AFFECT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _v
            | CHAR _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _v
            | CHAR_VAL ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState176
            | IDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _v
            | INT _v ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _v
            | LPAREN ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState176
            | MINUS ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState176
            | NEW ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState176
            | NOT ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState176
            | NULL ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState176
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState176)
        | AND | DIV | DOT | EQ | GEQ | GT | LEQ | LT | MINUS | NEQ | OR | PLUS | REM | TIMES ->
            _menhir_reduce15 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState202 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (x : (unit Ast.ident))) = _menhir_stack in
        let _v : (unit Ast.ident option) =     ( Some x ) in
        _menhir_goto_option_ident_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and file : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (unit Ast.proc) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = Obj.magic () in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | WITH ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR)
  

