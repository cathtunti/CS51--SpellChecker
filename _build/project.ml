open Core.Std

exception ImplementMe

type path = Left | Right 
type order = Equal | Less | Greater
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
  type branch = Single of d * string | Mult of (d * string * branch list)
  type tree = Empty | Branch of branch

  (* Returns an empty BKtree *)
  let empty = Empty

  let load_dict filename = raise ImplementMe
  (*
    let str_list = In_channel.read_lines filename in 
    let rec load_str_list (lst: sting list) (t:tree) : tree =
      match lst with
      | [] -> t
      | hd::tl -> 
          let s = String.filter ~f:(fun c -> (c >= 'a' && c <= 'z')) hd in
  *)
  (********************)
  (* Helper Functions *)
  (********************)

  let same_word (w1: string) (w2:string) : bool = ((D.distance w1 w2) = D.zero)

  let extract_d (branch: branch) : d =
    match branch with
    | Single (d,_) -> d
    | Mult (d,_,_) -> d

  let compare_dist (d1: d) (d2: d) : order =
    if (d1 = d2) then Equal else if (d1 > d2) then Greater
    else Less

  (***********************)
  (* Interface Functions *)
  (***********************)

  let search word t = raise ImplementMe

  let rec is_member word t = raise ImplementMe
(*     match t with
    | Leaf -> false
    | Branch(l, d, s, r) -> 
        if word = s then true
        else (is_member word l) || (is_member word r) *)

  let multiple_search wlst t = raise ImplementMe

  let print_result lst = raise ImplementMe

  let rec insert (word: string) (tree: tree) : tree = 
    let rec add_to_branch (word: string) (br: branch) : branch = 
      let rec inject_to_lst (word: string) (d1: d) (b_lst: branch list) : branch list =
                match b_lst with
                | [] -> [Single(d1, word)]
                | [hd] -> (match compare_dist d1 (extract_d hd) with
                           | Equal -> [add_to_branch word hd]
                           | Less -> (inject_to_lst word d1 []) @ [hd]
                           | Greater -> hd::(inject_to_lst word d1 []))
                | hd::tl -> (match compare_dist d1 (extract_d hd) with
                             | Equal -> (add_to_branch word hd)::tl
                             | Less -> (inject_to_lst word d1 []) @ hd::tl
                             | Greater -> hd::(inject_to_lst word d1 tl)) in
      match br with
      | Single (d, s) -> 
          if (same_word s word) then br (* Single (d, s) *)
          else Mult (d, s, [Single ((D.distance s word), word)]) 
      | Mult (d, s, b_lst) -> 
          (* if we found the same word, then return as is *)
          if (same_word s word) then br (* Mult (d, s, b_lst) *)
          (* else look through its children (list) *)
          else (* let d1 = D.distance s word in *) Mult (d, s, (inject_to_lst word (D.distance s word) b_lst)) in
    match tree with
    | Empty -> Branch (Single (D.zero, word))
    | Branch b -> Branch (add_to_branch word b)
            

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




