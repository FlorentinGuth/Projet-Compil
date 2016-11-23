let parse_only = ref false
let type_only  = ref false


let report file b e = 
  let l = b.Lexing.pos_lnum in
  let fc = b.Lexing.pos_cnum - b.Lexing.pos_bol + 1 in
  let lc = e.Lexing.pos_cnum - b.Lexing.pos_bol + 1 in
  Format.eprintf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc

let compile file =
  let ch = open_in file in
  let print_file () =
    try
      while true do
        print_endline (input_line ch)
      done;
    with End_of_file -> print_newline ()
  in
  
  let buf = Lexing.from_channel ch in
  let out = Format.std_formatter in
  try
    print_endline "Original Ada file:";
    print_file ();
    seek_in ch 0;
    print_endline "\nAST:";
    Printer.print_ast out (Parser.file Lexer.token buf);
    print_newline ();
    close_in ch
  with
  | Lexer.Lexical_error s -> report file (Lexing.lexeme_start_p buf)
                               (Lexing.lexeme_end_p buf);
    Format.eprintf "Lexical error: %s" s; exit 1
  | Parser.Error -> Format.eprintf "Echec du parser non documenté"; exit 1
  | _ -> exit 1
  

let () =
  Arg.parse 
    [("--parse-only", Arg.Set parse_only, "Generate the AST without typing");
     ("--type-only",  Arg.Set type_only,  "Generate a typed AST")
    ] 
    compile 
    "usage: adac [options] file.adb"
