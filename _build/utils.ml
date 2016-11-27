open Ast

(** Errors *)

exception Lexing_error  of string * Loc.loc
exception Parsing_error of string * Loc.loc
exception Typing_error  of string * Loc.loc


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
                                 
let decorate desc deco = 
  { desc; deco }
let replace_deco node deco =
  decorate node.desc deco
let decorate_dummy_loc desc = decorate desc (Lexing.dummy_pos, Lexing.dummy_pos)
let decorate_dummy_typ desc = decorate desc Typ.Tnull


(** Adding functions to standard library modules *)

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

module Set = struct
  include (Set : module type of Set with module Make := Set.Make)
          
  module Make(O : Set.OrderedType) =
  struct
    include Set.Make(O)
        
    let rec of_list = function
      | [] -> empty
      | x :: xs -> add x (of_list xs)
  end
end
