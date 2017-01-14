let parse_only = ref false
let type_only  = ref false
    

let compile file =
  let print_file file =
    let ch = open_in file in
    try
      while true do
        print_endline (input_line ch)
      done;
    with End_of_file -> close_in ch
  in
  
  let out = Format.std_formatter in
  (*   
  let rec token_list () =
    match Lexer.token buf with
    | Parser.EOF -> []
    | t -> t :: (token_list ())
  in
  let print_tokens () =
    print_endline "Token list:";
    Printer.print_token_list out (token_list ());
    print_string "\n\n";
    Lexing.flush_input buf
  in
     
  let print_ast a =
    print_endline "AST:";
    Printer.print_ast out a;
    print_string "\n\n"
  in*)

  let print_ast_typed ta =
    print_endline "\n\nTyped AST:";
    Ast_typed.print_ast out ta;
    print_string "\n\n\n"
  in

  let ch = open_in file in
  let buf = Lexing.from_channel ch in
  buf.Lexing.lex_start_p <- { buf.Lexing.lex_start_p with Lexing.pos_fname = file };
  buf.Lexing.lex_curr_p  <- { buf.Lexing.lex_curr_p  with Lexing.pos_fname = file };
  
  try
    print_endline "Original Ada file:";
    (*print_file file;*)
    (*print_tokens ();*)
    
    let a = Parser.file Lexer.token buf in
    (*print_ast a;*)
    
    if not !parse_only then begin
      let ta = Typer.type_ast a in
      (*print_ast_typed ta;*)
      if not !type_only then begin
        let output = (Filename.chop_suffix file ".exp") ^ ".s" in
        Compiler.compile_program ta output;

        (*print_string "Compiled x86-64 file:\n";
          print_file output;*)
      end;
    end;
    close_in ch
  with
  | Utils.Lexing_error  (msg, l) -> Utils.report "Lexical" msg l; exit 1
  | Utils.Parsing_error (msg, l) -> Utils.report "Syntax"  msg l; exit 1
  | Utils.Typing_error  (msg, l) -> Utils.report "Typing"  msg l; exit 1
  | Parser.Error -> Utils.report "Syntax" "Undocumented syntax error"
                      (Lexing.lexeme_start_p buf, Lexing.lexeme_end_p buf); exit 1
  | _ ->            Utils.error "Unknown compiler error"; exit 2
           

let () =
  Arg.parse 
    [("--parse-only", Arg.Set parse_only, "\tGenerate the AST without typing");
     ("--type-only",  Arg.Set type_only,  "\tGenerate a typed AST");
    ] 
    compile 
    "usage: adac [options] file.adb"
