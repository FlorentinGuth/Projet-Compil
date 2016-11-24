open Ast

(** Errors *)

exception Lexing_error  of string * loc
exception Parsing_error of string * loc
exception Typing_error  of string * loc


(** Helper functions *)

let assoc_to_hashtbl l =
  let t = Hashtbl.create 42 in
  List.iter (fun (k, v) -> Hashtbl.add t k v) l;
  t


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
let option_to_list l = to_some [] l
                                 
let decorate desc deco = 
  { desc; deco }
let replace_deco node deco =
  decorate node.desc deco
let decorate_dummy desc = decorate desc (Lexing.dummy_pos, Lexing.dummy_pos)


module Map = struct
  include (Map : module type of Map with module Make := Map.Make)
          
  module Make(O : Map.OrderedType) =
  struct
    include Map.Make(O)
        
    let rec of_list = function
      | [] -> empty
      | (k, v) :: xs -> add k v (of_list xs)
  end
end