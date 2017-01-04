open Format
    

(** General use printers *)

let print_option printer fmt (prec, opt) =
  match opt with
  | None -> ()
  | Some x -> fprintf fmt "%s%a" prec printer x

let print_if fmt (s, b) =
  if b then fprintf fmt "%s" s

let print_list print_sep print_elt =
  let rec printer fmt = function
    | [] -> ()
    | [x] -> fprintf fmt "%a" print_elt x
    | x :: xs -> fprintf fmt "%a%a%a" print_elt x print_sep () printer xs
  in printer

let print_sep s fmt () =
  fprintf fmt "%s" s

let print_to_string printer arg =
  printer Format.str_formatter arg;
  Format.flush_str_formatter ()
