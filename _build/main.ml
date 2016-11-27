open Format
    

let parse_only = ref false
let type_only  = ref false


let report h file msg (s, e) =
  let loc = ({ s with Lexing.pos_fname = file }, e) in
  eprintf "%a:\n%s%s%s" Ast.Loc.print_loc loc h " erorr: " msg;
  pp_print_flush err_formatter ()

let error s =
  Format.eprintf "%s" s;
  Format.pp_print_flush Format.err_formatter ()
    

let compile file =
  let ch = open_in file in
  let buf = Lexing.from_channel ch in
  let out = Format.std_formatter in
  
  let print_file () =
    print_endline "Original Ada file:";
    try
      while true do
        print_endline (input_line ch)
      done;
    with End_of_file -> print_string "\n\n"; seek_in ch 0
  in

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
  in
  *)
 
  try
    print_file ();
    (*print_tokens ();*)
    let a = Parser.file Lexer.token buf in
    (*print_ast a;*)
    if not !parse_only then ignore (Typer.type_ast a);
    print_endline "Everything went fine!";
    close_in ch
  with
  | Utils.Lexing_error  (msg, l) -> report "Lexical" file msg l; exit 1
  | Utils.Parsing_error (msg, l) -> report "Syntax"  file msg l; exit 1
  | Utils.Typing_error  (msg, l) -> report "Typing"  file msg l; exit 1
  | Parser.Error -> report "Syntax" file "Undocumented syntax error"
                      (Lexing.lexeme_start_p buf, Lexing.lexeme_end_p buf); exit 1
  | _ ->            error "Unknown compiler error";    exit 2
           

let () =
  Arg.parse 
    [("--parse-only", Arg.Set parse_only, "\tGenerate the AST without typing");
     ("--type-only",  Arg.Set type_only,  "\tGenerate a typed AST");
    ] 
    compile 
    "usage: adac [options] file.adb"
