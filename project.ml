open Core.Std

exception ImplementMe

type path = Left | Right 

(* signature for edit distance *)
module type DISTANCE = 
sig

  (* Type of distance *)
  type d

  (* Return distance between words *)
  val distance : string -> string -> d

  (* Zero distance *)
  val zero : d

  (* Return Left if d1 is closer to d2 than d3. Otherwise, return Right *)
  val closer_path : d -> d -> d -> path

  (* Tests for functions in this module *)
  val run_tests : unit 

end


(* signature for BKtree *)
module type BKTREE =
sig

  exception EmptyTree
  exception NodeNotFound

  (* type of distance *)
  type d

  (* type of tree *)
  type tree

  (* Returns an empty BKtree *)
  val empty : tree

  (* Take filename of a dictionary and return BKtree of the dictionary *)
  val load_dict : string -> tree

  (* Insert string into a BKtree *)
  val insert : string -> tree -> tree

  (* Search a BKtree for the given word. Return a list of tuples of the closest 
     word and the distance between them. *)
  val search : string -> tree -> (string * d) list

  (* returns true if word is the BKtree, false otherwise *)
  val is_member : string -> tree -> bool

  (* Same as search, but take in string list to search multiple strings at the
     same time. *)
  val multiple_search : string list -> tree -> (string * d) list list 

  (* Print out results of multiple_search in a readable format *)
  val print_result : string list -> tree -> unit 

  (* Delete the given word from a BKtree. May raise NodeNotFound exception. *)
  val delete : string -> tree -> tree

  (* Tests for functions in this module *)
  val run_tests : unit

end

(* implementation for Levenshtein Disance with brute force approach *)
module NaiveLevDistance : DISTANCE with type d=int = 
struct
  
  type d = int

  let distance s1 s2 =
    let (s1, s2) = (String.lowercase s1, String.lowercase s2) in
    let (len1, len2) = (String.length s1, String.length s2) in 
    let rec get_distance (p1:int) (p2:int) : int =
      if p1 = len1 then len2 - p2 else if p2 = len2  then len1 - p1
      else 
        let (c1, c2) = (String.get s1 p1, String.get s2 p2) in
        if c1 = c2 then get_distance (p1 + 1) (p2 + 1)
        else 1 + min (get_distance (p1 + 1) (p2 + 1)) (min 
                  (get_distance (p1 + 1) p2) (get_distance p1 (p2 + 1))) in
    if len1 = 0 then len2 
    else if len2 = 0 then len1
    else get_distance 0 0

  let zero = 0

  let closer_path d1 d2 d3 =
    if abs(d1 - d2) < abs(d1 - d3) then Left else Right 

  let run_tests =
    assert((distance "evidence" "providence") = 3);
    assert((distance "evidence" "provident") = 5);
    assert((distance "cook" "snook") = 2);
    assert((distance "" "") = 0);
    assert((distance "CS51" "CS51") = 0);
    assert((distance "cool" "Cool") = 0);
    ()

end



(* implementation for BKtree *)
module BKtree(D:DISTANCE) : BKTREE with type d=D.d =
struct

  exception EmptyTree
  exception NodeNotFound

  type d = D.d
  type tree = Leaf | Branch of tree * d * string * tree

  (* Returns an empty BKtree *)
  let empty = Leaf

  let load_dict filename = raise ImplementMe
  (*
    let str_list = In_channel.read_lines filename in 
    let rec load_str_list (lst: sting list) (t:tree) : tree =
      match lst with
      | [] -> t
      | hd::tl -> 
          let s = String.filter ~f:(fun c -> (c >= 'a' && c <= 'z')) hd in
  *)

  let rec search word t =
  (*
    match t with
    |Leaf -> []
    |Branch(l,d,s,r) -> 
        if word = s then (word * d) :: search  else
            if (D.distance word s) > (D.distance _ _) && 
               (D.distance _ _) - 1 <= d && d <= (D.distance _ _ ) then search word l
            if (D.distance _ _) - 1 < (D.distance _ _) + 1 then search word r
  *)

  let rec is_member word t = 
    match t with
    | Leaf -> false
    | Branch(l, _, s, r) -> 
        if word = s then true
        else (is_member word l) || (is_member word r)

  let multiple_search wlst t = raise ImplementMe

  let print_result lst = raise ImplementMe

  let rec insert word t = 
    if is_member word t then t else 
    match t with
    | Leaf -> Branch(empty, D.zero, word, empty)
    | Branch(l, d, s, r) -> 
        let d' = D.distance word s in
        match (l, r) with
        | (Leaf, _) -> Branch(Branch(empty, d', word, empty), d, s, r)
        | (_, Leaf) -> Branch(l, d, s, Branch(empty, d', word, empty))
        | (Branch(_, dl, _, _), Branch(_, dr, _, _)) -> 
            match (D.closer_path d' dl dr) with
            | Left -> Branch(insert word l, d, s, r)
            | Right -> Branch(l, d, s, insert word r)
            

  let delete word t = raise ImplementMe

  let run_tests = 
      ()


end

let _ = NaiveLevDistance.run_tests


(* implementation for Levenshtein Distance using dynamic programming concept 
module DynamicLevDistance : DISTANCE with type num=int =
struct
  type num = int
  let distance s1 s2 = pseudo code below
  let compare d1 d2 = raise ImplementMe
end
*)

(* implementation for Damerau–Levenshtein distance using dynamic programming 
module DamerauLevDistance : DISTANCE with type num=int =
struct
  type num = int
  let distance s1 s2 = raise ImplementMe
  let compare d1 d2 = raise ImplementMe
end
*)




