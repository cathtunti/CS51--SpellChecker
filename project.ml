open Core.Std

exception ImplementMe

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

  val is_similar : d -> bool

  (* *)
  val in_range : d -> d -> bool

  val compare : d -> d -> order

  val sort : string -> string list -> string list

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
  val search : string -> tree -> string list

  (* returns true if word is the BKtree, false otherwise *)
  val is_member : string -> tree -> bool

  (* Same as search, but take in string list to search multiple strings at the
     same time. *)
  val multiple_search : string list -> tree -> string list list 

  (* Print out results of multiple_search in a readable format *)
  val print_result : string list -> tree -> unit 

  (* Tests for functions in this module *)
  val run_tests : unit -> unit

end

(* implementation for Levenshtein Disance with brute force approach *)
module NaiveLevDistance : DISTANCE with type d=int = 
struct
  
  type d = int

  let tolerance = 1

  let distance (s1:string) (s2:string) : d =
    let (s1, s2) = (String.lowercase s1, String.lowercase s2) in
    let (len1, len2) = (String.length s1, String.length s2) in 
    let rec get_distance (p1:int) (p2:int) : int =
      if p1 = len1 then len2 - p2 
      else if p2 = len2 then len1 - p1
      else let (c1, c2) = (String.get s1 p1, String.get s2 p2) in
           if c1 = c2 then get_distance (p1 + 1) (p2 + 1)
           else 1 + min (get_distance (p1 + 1) (p2 + 1)) (min 
                  (get_distance (p1 + 1) p2) (get_distance p1 (p2 + 1))) in
    if len1 = 0 then len2 
    else if len2 = 0 then len1
    else get_distance 0 0

  let zero = 0

  let is_similar (d:d) : bool =
    d <= tolerance

  let in_range (d1:d) (d2:d) : bool =
    abs(d1 - d2) <= tolerance

  let compare (d1:d) (d2:d) : order =
    if d1 = d2 then Equal 
    else if d1 > d2 then Greater
    else Less

  let sort (search:string) (wlst:string list) : string list = 
    List.sort ~cmp:(fun x y -> (distance search x) - (distance search y)) wlst

  let run_tests =
    assert((distance "evidence" "providence") = 3);
    assert((distance "evidence" "provident") = 5);
    assert((distance "evidence" "provoident") = 6);
    assert((distance "cook" "snook") = 2);
    assert((distance "" "") = 0);
    assert((distance "CS51" "CS51") = 0);
    assert((distance "cool" "Cool") = 0);
    ()

end

module DynamicLevDistance : DISTANCE with type d=int = 
struct

  type d = int
  let tolerance = 1

  let distance (s1:string) (s2:string) : int = 
    let (s1, s2) = (String.lowercase s1, String.lowercase s2) in
    let (len1, len2) = (String.length s1, String.length s2) in
    let rec get_distance (col:int) (row:int) (prev_row:int array) 
      (current_row:int array) : int = 
      let (c1, c2) = (String.get s1 (row - 1), String.get s2 (col - 1)) in
      let (del, sub, ins) = (Array.get current_row (col - 1), 
            Array.get prev_row (col - 1), Array.get prev_row col) in
      let d = min del (min sub ins) in
      let d' = if c1 <> c2 then 1 + d else d in
      if col = len2 && row = len1 then d'
      else
        (Array.set current_row col d';
        if col = len2 then get_distance 1 (row + 1) current_row 
                              (Array.create ~len:(len2 + 1) (row + 1))
        else get_distance (col + 1) row prev_row current_row) in
    if len1 = 0 then len2 
    else if len2 = 0 then len1
    else get_distance 1 1 (Array.init (len2 + 1) (fun i -> i)) 
                        (Array.create ~len:(len2 + 1) 1)

  let zero = 0

  let is_similar (d:d) : bool =
    d <= tolerance

  let in_range (d1:d) (d2:d) : bool =
    abs(d1 - d2) <= 1

  let compare (d1: d) (d2: d) : order =
    if d1 = d2 then Equal 
    else if d1 > d2 then Greater
    else Less

  let sort (search:string) (wlst:string list) : string list = 
    List.sort ~cmp:(fun x y -> (distance search x) - (distance search y)) wlst

  let run_tests =
    assert((distance "evidence" "providence") = 3);
    assert((distance "evidence" "provident") = 5);
    assert((distance "evidence" "provoident") = 6);
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


  (********************)
  (* Helper Functions *)
  (********************)

  let same_word (w1: string) (w2:string) : bool = ((D.distance w1 w2) = D.zero)

  let extract_d (branch: branch) : d =
    match branch with
    | Single (d,_) -> d
    | Mult (d,_,_) -> d


  (***********************)
  (* Interface Functions *)
  (***********************)

  let search (word: string) (tree: tree) : string list = 
    let rec search_br (word: string) (br: branch) : string list = 
      let rec search_br_lst (word: string) (d_ori : d) (b_lst: branch list) (return_lst: string list ) : string list =
        match b_lst with
        | [] -> return_lst
        | hd::tl -> 
            if (D.in_range d_ori (extract_d hd)) 
            then search_br word hd  @ (search_br_lst word d_ori tl return_lst)
            else (search_br_lst word d_ori tl return_lst)
      in
      match br with
      | Single (d, w) -> 
          (* if within tolerance range then add to list *)
          if D.is_similar (D.distance word w) then [w]
          else []
      | Mult (d, w, b_lst) -> 
          if D.is_similar (D.distance word w) 
          then (search_br_lst word (D.distance w word) b_lst [w]) 
          else (search_br_lst word (D.distance w word) b_lst [])
    in
    match tree with
    | Empty -> [] 
    | Branch b -> D.sort word (search_br word b)



  let is_member (word: string) (tree: tree) : bool = 
    let rec search_br (word: string) (br: branch) : bool =
      let rec search_br_lst (word: string) b_lst : bool =
        match b_lst with
        | [] -> false
        | hd::tl -> (search_br word hd) || search_br_lst word tl in
      match br with
      | Single (_, w) -> (word = w)
      | Mult (_, w, b_lst) -> (word = w) || search_br_lst word b_lst in
    match tree with
    | Empty -> false
    | Branch b -> search_br word b



  let rec multiple_search (w_lst: string list) (tree: tree) : string list list = 
    match w_lst with
    | [] -> []
    | hd::tl -> (search hd tree) :: (multiple_search tl tree)



  let print_result (input_lst: string list) (tree: tree) : unit = raise ImplementMe

  let insert (word: string) (tree: tree) : tree = 
    let rec add_to_branch (word: string) (br: branch) : branch = 
      let rec inject_to_lst (word: string) (d1: d) (b_lst: branch list) : branch list =
                match b_lst with
                | [] -> [Single(d1, word)]
                | [hd] -> (match D.compare d1 (extract_d hd) with
                           | Equal -> [add_to_branch word hd]
                           | Less -> (inject_to_lst word d1 []) @ [hd]
                           | Greater -> hd::(inject_to_lst word d1 []))
                | hd::tl -> (match D.compare d1 (extract_d hd) with
                             | Equal -> (add_to_branch word hd)::tl
                             | Less -> (inject_to_lst word d1 []) @ hd::tl
                             | Greater -> hd::(inject_to_lst word d1 tl)) in
      match br with
      | Single (d, s) -> 
          if (same_word s word) then br 
          else Mult (d, s, [Single ((D.distance s word), word)]) 
      | Mult (d, s, b_lst) -> 
          (* if we found the same word, then return as is *)
          if (same_word s word) then br 
          (* else look through its children (list) *)
          else (* let d1 = D.distance s word in *) Mult (d, s, (inject_to_lst word (D.distance s word) b_lst)) in
    match tree with
    | Empty -> Branch (Single (D.zero, word))
    | Branch b -> Branch (add_to_branch word b)

  let load_dict (filename:string) : tree = 
    let rec load_str_list (lst: string list) (t:tree) : tree =
      match lst with
      | [] -> t
      | hd::tl -> 
          let s = String.filter ~f:(fun c -> (c >= 'a' && c <= 'z')) hd in
          load_str_list tl (insert s t) in
    load_str_list (In_channel.read_lines filename) empty
            
  (***********************)
  (*        Test         *)
  (***********************)
  let test_insert () = 
    let (w1,w2,w3) = ("book", "books", "boo") in
    let t = insert w1 empty in
    let d0 = D.zero in
    assert (t = Branch(Single(d0, w1)));
    let t = insert w2 t in
    let d12 = D.distance w1 w2 in 
    assert (t = Branch(Mult(d0, w1, [Single(d12, w2)])));
    let t = insert w3 t in
    let d23 = D.distance w2 w3 in 
    assert (t = Branch(Mult(d0, w1, [Mult(d12, w2, [Single(d23, w3)])])));
    
    let (w4, w5, w6, w7, w8, w9, w10, w11) = ("book", "books", "boo", "boon", "cook", "cake", "cape", "cart") in
    let t = insert w4 empty in
    let d0 = D.zero in
    assert (t = Branch(Single(d0, w4)));
    let t = insert w5 t in
    let d45 = D.distance w4 w5 in
    assert (t = Branch(Mult(d0, w4, [Single(d45, w5)])));
    let t = insert w6 t in
    let d56 = D.distance w5 w6 in
    assert (t = Branch(Mult(d0, w4, [Mult(d45, w5, [Single(d56, w6)])])));
    let t = insert w7 t in
    let d67 = D.distance w6 w7 in
    assert (t = Branch(Mult(d0, w4, [Mult(d45, w5, [Mult(d56, w6, [Single(d67, w7)])])])));
    let t = insert w8 t in
    let d68 = D.distance w6 w8 in
    assert (t = Branch(Mult(d0, w4, [Mult(d45, w5, [Mult(d56, w6, [Single(d67, w7); Single(d68, w8)])])])));
    let t = insert w9 t in
    let d49 = D.distance w4 w9 in
    assert (t = Branch(Mult(d0, w4, [Mult(d45, w5, [Mult(d56, w6, [Single(d67, w7); Single(d68, w8)])]); 
                                     Single(d49, w9)])));
    let t = insert w10 t in
    let d9_10 = D.distance w9 w10 in
    assert (t = Branch(Mult(d0, w4, [Mult(d45, w5, [Mult(d56, w6, [Single(d67, w7); Single(d68, w8)])]);
                                     Mult(d49, w9, [Single(d9_10, w10)])])));
    let t = insert w11 t in
    let d9_11 = D.distance w9 w11 in
    assert (t = Branch(Mult(d0, w4, [Mult(d45, w5, [Mult(d56, w6, [Single(d67, w7); Single(d68, w8)])]); 
                                     Mult(d49, w9, [Single(d9_10, w10); Single(d9_11, w11)])])));
    ()

  let test_is_member () = 
    let (w4, w5, w6, w7, w8, w9, w10, w11) = ("book", "books", "boo", "boon", "cook", "cake", "cape", "cart") in
    let d0 = D.zero in
    let d45 = D.distance w4 w5 in
    let d56 = D.distance w5 w6 in
    let d67 = D.distance w6 w7 in
    let d68 = D.distance w6 w8 in
    let d49 = D.distance w4 w9 in
    let d9_10 = D.distance w9 w10 in
    let d9_11 = D.distance w9 w11 in
    let t =  Branch(Mult(d0, w4, [Mult(d45, w5, [Mult(d56, w6, [Single(d67, w7); Single(d68, w8)])]); 
                                  Mult(d49, w9, [Single(d9_10, w10); Single(d9_11, w11)])])) in
    let (w12, w13, w14) = ("random", "test", "nothing") in
    assert (is_member w4 t = true);
    assert (is_member w5 t = true);
    assert (is_member w6 t = true);
    assert (is_member w7 t = true);
    assert (is_member w8 t = true);
    assert (is_member w9 t = true);
    assert (is_member w10 t = true);
    assert (is_member w11 t = true);
    assert (is_member w12 t = false);
    assert (is_member w13 t = false);
    assert (is_member w14 t = true);
    
    ()
  
  let test_search () = raise ImplementMe

  let run_tests () = 
    test_insert ();
    test_is_member ();
    ()


end

let _ = NaiveLevDistance.run_tests
let _ = DynamicLevDistance.run_tests


module BKTree = (BKtree(DynamicLevDistance) : BKTREE with type d = DynamicLevDistance.d)

let _ = BKTree.run_tests


(* implementation for Damerau–Levenshtein distance using dynamic programming 
module DamerauLevDistance : DISTANCE with type num=int =
struct
  type num = int
  let distance s1 s2 = raise ImplementMe
  let compare d1 d2 = raise ImplementMe
end
*)




