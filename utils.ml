open Format


(** Errors *)

type pos = Lexing.position
type loc = pos * pos (* (start, end) *)

let print_loc fmt ((s, e) : loc) =
  let open Lexing in
  fprintf fmt "@[File@ \"%s\"@, line@ %d,@ characters %d-%d@]"
    s.pos_fname s.pos_lnum (s.pos_cnum - s.pos_bol)
    (e.pos_cnum - s.pos_bol)

exception Lexing_error  of string * loc
exception Parsing_error of string * loc
exception Typing_error  of string * loc
                                      

let report ?(warning = false) h msg (s, e) =
  let err = if warning then "Warning" else h ^ " error" in
  eprintf "%a:\n%s: %s" print_loc (s, e) err msg;
  pp_print_flush err_formatter ()

let error s =
  eprintf "%s" s;
  pp_print_flush err_formatter ()



(** The type of a decorated type *)
type ('desc, 'deco) node =
  {
    desc : 'desc;
    deco : 'deco;
  }

let decorate desc deco = 
  { desc; deco }
let replace_deco node deco =
  decorate node.desc deco
let decorate_dummy_loc desc = decorate desc (Lexing.dummy_pos, Lexing.dummy_pos)
let strip_deco n = n.desc



(** Helper functions *)

let assoc_to_hashtbl l =
  let t = Hashtbl.create 42 in
  List.iter (fun (k, v) -> Hashtbl.add t k v) l;
  t

let to_some default = function
  | None -> default
  | Some x -> x             



(** Adding functions to standard library modules *)

module List =
struct
  include List

  let find_duplicates cmp l =
    let rec find_adj = function
      | [] | [_] -> None
      | x :: ((y :: _) as tl) -> if cmp x y = 0 then Some (x, y) else find_adj tl
    in
    find_adj (List.stable_sort cmp l)

  let remove_none l =
    let rec aux acc = function
      | [] -> acc
      | None :: tl -> aux acc tl
      | (Some x) :: tl -> aux (x :: acc) tl
    in
    List.rev (aux [] l)
end
  
module Map =
struct
  include (Map : module type of Map with module Make := Map.Make)
          
  module Make(O : Map.OrderedType) =
  struct
    include Map.Make(O)
        
    let rec of_list = function
      | [] -> empty
      | (k, v) :: xs -> add k v (of_list xs)
  end
end

module Set =
struct
  include (Set : module type of Set with module Make := Set.Make)
          
  module Make(O : Set.OrderedType) =
  struct
    include Set.Make(O)
        
    let rec of_list = function
      | [] -> empty
      | x :: xs -> add x (of_list xs)
  end
end


module Sset = Set.Make(String)
module Smap = Map.Make(String)
