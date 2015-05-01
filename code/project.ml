(*****************************************************************************)
(* Author: Mick Kajornrattana Catherine Tuntiserirat Billie Wei Erika Puente *)
(* Usage: type make to compile, type ./project.ml to run *)
(* Description: The program has two module signatures: DISTANCE and BKTREE.  *)
(*    The implementations DISTANCE include Levenshtein Distance written      *)
(*    using brute-force approach, Levenshtein Distance written written using *)
(*    dynamic programming approach and Dameraue Levenshtein Distance which   *)
(*    the Levenshtein Distance taking into account transposition.            *)
(*    The implementation of BKTree allows the search of a word in a          *)
(*    dictionary possible. We include the probability of a word so that      *)
(*    the suggested words are closer to what humans expect.                  *)
(*****************************************************************************)

open Core.Std
open Str

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

  (* True if distance (d) is within the tolerance (int), false otherwise *)
  val is_similar : int -> d -> bool

  (* For BKTree, true if two distances allow crawling on a branch of tree *)
  val in_range : int -> d -> d -> bool

  (* compare two distances if <,>,= *)
  val compare : d -> d -> order

  (* use probability to sort the return list from BKTree's search *)
  val sort : int -> string -> (string * float) list -> string list

  (* Tests for functions in this module *)
  val run_tests : unit

end


(* signature for BKTree *)
module type BKTREE =
sig

  (* type of distance *)
  type d

  (* type of tree *)
  type tree

  (* Returns an empty BKTree *)
  val empty : tree

  (* Take filename of a dictionary and return BKTree of the dictionary *)
  val load_dict : string -> tree

  (* Insert string into a BKTree *)
  val insert : string * float -> tree -> tree

  (* Search a BKTree for the given word. Return a list of tuples of the closest 
     word and the distance between them. *)
  val search : string -> tree -> string list

  (* returns true if word is the BKTree, false otherwise *)
  val is_member : string -> tree -> bool

  (* Same as search, but take in string list to search multiple strings at the
     same time. *)
  val multiple_search : string list -> tree -> string list list 

  (* Print out results of search in a readable format *)
  val print_result : string -> tree -> unit 

  (* Print out results of multiple_search in a readable format *)
  val print_mult_result : string list -> tree -> unit 

  (* Tests for functions in this module *)
  val run_tests : unit -> unit


end

(* implementation for Levenshtein Disance with brute force approach *)
module NaiveLevDistance : DISTANCE with type d=int = 
struct
  
  type d = int

  let distance (s1: string) (s2: string) : d =
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

  let is_similar (tolerance: int) (d: d) : bool = d <= tolerance

  let in_range (tolerance: int) (d1: d) (d2: d) : bool =
    abs(d1 - d2) <= tolerance

  let compare (d1: d) (d2: d) : order =
    if d1 = d2 then Equal 
    else if d1 > d2 then Greater
    else Less

  let sort (tolerance: int) (word: string) (wlst: (string * float) list) =
    let sort_func (tup1: string * float) (tup2: string * float) = 
      (* turn edit distance into probability. Look at write up for more info *)
      let t = float tolerance in
      let mult = 1000. in 
      let prob_of_dist d = 
        (mult -. 1.) *. mult ** (t -. float d) /. (-1. +. mult ** (t +. 1.)) in
      (* probability of a suggested word is intended, given 'word' is typed *)
      let prob1 = prob_of_dist (distance word (fst tup1)) *. snd tup1 in
      let prob2 = prob_of_dist (distance word (fst tup2)) *. snd tup2 in
      if prob1 = prob2 then 0
      else if prob2 > prob1 then 1
      else -1 in
    List.map ~f:(fun x -> fst x) (List.sort ~cmp:(sort_func) wlst) 

  let distance_tests () = 
    assert((distance "" "") = 0);
    assert((distance "a" "b") = 1);
    assert((distance "ab" "ba") = 2);
    assert((distance "cat" "tac") = 2);
    assert((distance "cool" "school") = 2);
    assert((distance "bottom" "button" = 2));
    assert((distance "evidence" "providence") = 3);
    assert((distance "evidence" "provident") = 5);
    assert((distance "evidence" "provoident") = 6);
    assert((distance "Levenshtein" "Levenstein") = 1);
    assert((distance "transposition" "transposition" = 0));
    assert((distance "rtansposition" "transposition" = 2));
    assert((distance "rtnasposition" "transposition" = 3));
    assert((distance "rtnasposiiton" "transposition" = 5));
    ()

  let sort_tests () = 
    assert((sort 0 "" []) = []);
    assert((sort 0 "" [("cook",0.2)]) = ["cook"]);
    assert((sort 0 "book" [("book",0.);("asfdasf",1.)]) = ["asfdasf";"book"]);
    assert((sort 2 "compiler" [("composer",1.3e-05);("compiler",1.3e-05);
           ("compilers",2.8e-06)]) = ["compiler";"compilers";"composer"]);
    ()

  let run_tests =
    distance_tests ();
    sort_tests ();
    ()

end


(* implementation for Levenshtein Disance with dynamic programming approach 
   using Wagner-Fischer matrix algorithm *)
module DynamicLevDistance : DISTANCE with type d=int = 
struct

  include NaiveLevDistance

  let distance (s1: string) (s2: string) : int = 
    let (s1, s2) = (String.lowercase s1, String.lowercase s2) in
    let (len1, len2) = (String.length s1, String.length s2) in
    let rec get_distance (col:int) (row:int) (prev_row:int array) 
      (current_row: int array) : int = 
      let (c1, c2) = (String.get s1 (row - 1), String.get s2 (col - 1)) in
      let (del, sub, ins) = (Array.get current_row (col - 1), 
            Array.get prev_row (col - 1), Array.get prev_row col) in
      let d = min del (min sub ins) in
      let d' = if c1 <> c2 then 1 + d else sub in
      if col = len2 && row = len1 then d'
      else
        (Array.set current_row col d';
         if col = len2 then get_distance 1 (row + 1) current_row 
                              (Array.create ~len:(len2 + 1) (row + 1))
         else get_distance (col + 1) row prev_row current_row) in
    if len1 = 0 then len2 
    else if len2 = 0 then len1
    else get_distance 1 1 (Array.init (len2 + 1) ~f:(fun i -> i)) 
                        (Array.create ~len:(len2 + 1) 1)

end

(* WARNING *)
(* Damerau CANNOT BE USED WITH BK-TREE (structurally). please see writeup *)
module DamerauLevDistance : DISTANCE with type d=int = 
struct

  include NaiveLevDistance

  let distance s1 s2 = 
    let (s1, s2) = (String.lowercase s1, String.lowercase s2) in
    let (len1, len2) = (String.length s1, String.length s2) in
    let rec get_distance (col:int) (row:int) (two_prev_row:int array) 
      (prev_row:int array) (current_row:int array) : int = 
      let (c1, c2) = (String.get s1 (row - 1), String.get s2 (col - 1)) in
      let (del, sub, ins) = (Array.get current_row (col - 1), 
            Array.get prev_row (col - 1), Array.get prev_row col) in
      let d = min del (min sub ins) in
      let d' = if c1 <> c2 then 1 + d else sub in
      let d' = 
        if col > 1 && row > 1 && c1 = (String.get s2 (col - 2)) && 
          c2 = (String.get s1 (row - 2)) then 
          min d' (1 + Array.get two_prev_row (col -2)) else d' in
      if col = len2 && row = len1 then d'
      else
        (Array.set current_row col d';
        if col = len2 then get_distance 1 (row + 1) prev_row current_row 
                              (Array.create ~len:(len2 + 1) (row + 1))
        else get_distance (col + 1) row two_prev_row prev_row current_row) in
    if len1 = 0 then len2 
    else if len2 = 0 then len1
    else get_distance 1 1 (Array.create ~len:(len2 + 1) ~-1) (Array.init 
      (len2 + 1) ~f:(fun i -> i))  (Array.create ~len:(len2 + 1) 1)

  (* Need to change the tests to take into account transposed characters *)
  let distance_tests () = 
    assert((distance "" "") = 0);
    assert((distance "a" "b") = 1);
    assert((distance "ab" "ba") = 1);
    assert((distance "cat" "tac") = 2);
    assert((distance "cool" "school") = 2);
    assert((distance "bottom" "button" = 2));
    assert((distance "evidence" "providence") = 3);
    assert((distance "evidence" "provident") = 5);
    assert((distance "evidence" "provoident") = 6);
    assert((distance "Levenshtein" "Levenstein") = 1);
    assert((distance "transposition" "transposition" = 0));
    assert((distance "rtansposition" "transposition" = 1));
    assert((distance "rtnasposition" "transposition" = 2));
    assert((distance "rtnasposiiton" "transposition" = 3));
    ()

  let sort_tests () = 
    assert((sort 0 "" []) = []);
    assert((sort 0 "" [("cook",0.2)]) = ["cook"]);
    assert((sort 0 "book" [("book",0.);("asfdasf",1.)]) = ["asfdasf";"book"]);
    assert((sort 2 "compiler" [("composer",1.3e-05);("compiler",1.3e-05);
           ("compilers",2.8e-06)]) = ["compiler";"compilers";"composer"]);
    ()

  let run_tests =
    distance_tests ();
    sort_tests ();
    ()

end

(*****************************)
(** BKTree with Probability **)
(*****************************)

(* implementation for BKTree *)
module BKTree(D:DISTANCE) : BKTREE with type d=D.d =
struct

  exception InvalidFile
  exception EmptyTree

  type d = D.d
  
  (* Single is a node with no child, where Mult has at least 1 children *)
  type branch = Single of d * (string * float) 
                | Mult of (d * (string * float) * branch list)
  
  type tree = Empty | Branch of branch

  (* Returns an empty BKTree *)
  let empty = Empty

  (* the max number of suggested words return to user *)
  let display_num = 10

  (********************)
  (* Helper Functions *)
  (********************)

  (* extract distance out of a brach type *)
  let extract_d (branch: branch) : d =
    match branch with
    | Single (d,_) | Mult (d,_,_) -> d

  (*************************)
  (* Functions in Signatur *)
  (*************************)
  
  let search (word: string) (tree: tree) : string list = 
    (* tolerance +1 for every 5 chars *)
    let tolerance = (String.length word) / 5 + 1 in
    (* traverse through branch *)
    let rec search_br (word: string) (br: branch) : (string * float) list = 
      (* traverse through other branches connected to Mult *)
      let rec search_br_lst (word: string) (d: d) (b_lst: branch list) 
        (return_lst: (string * float) list ) : (string * float) list =
        match b_lst with
        | [] -> return_lst
        | hd::tl -> 
            (* search if branch is within tolerance range before moving to
               the next child *)
            if (D.in_range tolerance d (extract_d hd)) then 
              search_br word hd  @ (search_br_lst word d tl return_lst)
            else (search_br_lst word d tl return_lst) in
      match br with
      | Single (_, (w,p)) -> 
          (* if similar enough then add to return list *)
          if D.is_similar tolerance (D.distance word w) then [(w,p)] else []
      | Mult (_, (w,p), b_lst) -> 
          (* if similar enough, add to return list and search its children *)
          if D.is_similar tolerance (D.distance word w) then 
            (search_br_lst word (D.distance w word) b_lst [(w,p)]) 
          else (search_br_lst word (D.distance w word) b_lst []) in
    match tree with
    | Empty -> [] 
    | Branch b -> D.sort tolerance word (search_br word b)


  let is_member (word: string) (tree: tree) : bool = 
    let rec search_br (word: string) (br: branch) : bool =
      (* traverse through other branches connected to Mult *)
      let rec search_br_lst (word: string) b_lst : bool =
        match b_lst with
        | [] -> false
        | hd::tl -> (search_br word hd) || search_br_lst word tl in
      match br with
      | Single (_, (w,_)) -> (word = w)
      | Mult (_, (w,_), b_lst) -> (word = w) || search_br_lst word b_lst in
    match tree with
    | Empty -> false
    | Branch b -> search_br word b


  let rec multiple_search (w_lst: string list) (t:tree) : string list list =
    match w_lst with
    | [] -> []
    | hd::tl -> (search hd t) :: (multiple_search tl t)

      
  let print_mult_result (input_lst: string list) (tree: tree) : unit = 
    let output = (multiple_search input_lst tree) in
    (* parse a return list of each input word *)
    let rec str_big_lst (output: string list list) : string = 
      (* truncate the return list to a given length *) 
      let rec truncate (len: int) (suggest: string list) : string list =
        if len = 0 then [] 
        else 
          match suggest with
          | [] -> []
          | hd::tl -> hd::(truncate (len - 1) tl) in
      (* parse each return word in a return list *)
      let rec str_sm_lst (output: string list) : string = 
        match (truncate display_num output) with
        | [] -> ""
        | hd::tl -> hd ^ " " ^ str_sm_lst tl in
      match output with
      | [] -> ""
      | hd::tl -> str_sm_lst hd ^ "\n" ^ str_big_lst tl in
    let out_string = str_big_lst output in
    if out_string = "\n" then (print_string "NO MATCH FOUND.\n"; flush_all ())
    else (print_string out_string; flush_all ())

  
  let print_result (input: string) (tree: tree) : unit =
    print_mult_result [input] tree 


  let insert (word_p: string * float) (tree: tree) : tree = 
    (* either add a word to Single or Mult *)
    let rec add_to_branch (word_p: string * float) (br: branch) : branch = 
      let (w, _) = word_p in
      (* add a word into a Mult by travering through its list of children *)
      let rec inject_to_lst (word_p: string * float) (d1: d) 
        (b_lst: branch list) : branch list =
        match b_lst with
        | [] -> [Single(d1, word_p)]
        | [hd] -> 
            (* word with lower edit dist. (from parent) is at front of list *)
            (match D.compare d1 (extract_d hd) with
             | Equal -> [add_to_branch word_p hd]
             | Less -> (inject_to_lst word_p d1 []) @ [hd]
             | Greater -> hd::(inject_to_lst word_p d1 []))
        | hd::tl -> 
            (match D.compare d1 (extract_d hd) with
             | Equal -> (add_to_branch word_p hd)::tl
             | Less -> (inject_to_lst word_p d1 []) @ hd::tl
             | Greater -> hd::(inject_to_lst word_p d1 tl)) in
      match br with
      | Single (d, (s, p)) -> 
          if (s = w) then br 
          else Mult (d, (s, p), [Single ((D.distance s w), word_p)]) 
      | Mult (d, (s, p), b_lst) -> 
          if (s = w) then br 
          (* traverse through Mult children to insert at the right place *)
          else Mult (d,(s,p),(inject_to_lst word_p (D.distance s w) b_lst)) in
    match tree with
    | Empty -> Branch (Single (D.zero, word_p))
    | Branch b -> Branch (add_to_branch word_p b)


  (* dictionary file must be in format "word\n 0.4454530e-20\n word_2\n ..." *)
  let load_dict (filename:string) : tree = 
    (* convert string list to tuple list *)
    let rec strlst_to_tuplst (strlst:string list) 
      (tuplst: (string * float) list) : (string * float) list =
      match strlst with
      | [] -> tuplst
      | hd1::hd2::tl -> 
          strlst_to_tuplst tl ((hd1, Float.of_string hd2)::tuplst)
      | _ -> raise InvalidFile in 
    let rec insert_tuplst (tuplst:(string * float) list) (t: tree) : tree =
      match tuplst with
      | [] -> t
      | hd::tl -> insert_tuplst tl (insert hd t) in
    insert_tuplst (strlst_to_tuplst (In_channel.read_lines filename) []) empty
         


  (***********************)
  (*        Test         *)
  (***********************)

  (* tuples of word and float for testing*)
  let (w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12, w13, w14, w15) = 
    (("book",0.1111),("books",0.2222),("boo",0.3333),("book",0.1111),
      ("books",0.2222),("boo",0.0001),("boon",0.0003),("cook",0.3234),
      ("cake",0.2342),("cape",0.2345),("cart",0.42094),("pool",0.0002), 
      ("food",0.2333),("form",0.22222),("pearl",0.23421)) 

  (* trees and branch for testing *)
  let (test_tree1, test_tree2, test_branch3) =
    let d0 = D.zero in
    let d45 = D.distance (fst w4) (fst w5) in
    let d56 = D.distance (fst w5) (fst w6) in
    let d67 = D.distance (fst w6) (fst w7) in
    let d68 = D.distance (fst w6) (fst w8) in
    let d49 = D.distance (fst w4) (fst w9) in
    let d9_10 = D.distance (fst w9) (fst w10) in
    let d9_11 = D.distance (fst w9) (fst w11) in
    let d4_12 = D.distance (fst w4) (fst w12) in
    let d12_13 = D.distance (fst w12) (fst w13) in
    let d12_15 = D.distance (fst w12) (fst w15) in
    let d13_14 = D.distance (fst w13) (fst w14) in
    (Branch(Mult(d0, w4, [Mult(d45, w5, [Mult(d56, w6, [Single(d67, w7); 
            Single(d68, w8)])]); Mult(d49, w9, [Single(d9_10, w10); 
            Single(d9_11, w11)])])), 
     Branch(Mult(d0, w4, [Mult(d45, w5, [Mult(d56, w6, [Single(d67, w7); 
            Single(d68, w8)])]); Mult(d49, w9, [Single(d9_10, w10); 
            Single(d9_11, w11)]); Mult(d4_12, w12, [Mult(d12_13, w13, 
            [Single(d13_14, w14)]); Single(d12_15, w15)])])), 
     Single(d9_11, w11))

  let test_insert () = 
    let t = insert w1 empty in
    let d0 = D.zero in
    assert (t = Branch(Single(d0, w1)));
    let t = insert  w2 t in
    let d12 = D.distance  (fst w1)  (fst w2) in 
    assert (t = Branch(Mult(d0, w1, [Single(d12, w2)])));
    let t = insert  w3 t in
    let d23 = D.distance  (fst w2)  (fst w3) in 
    assert (t = Branch(Mult(d0, w1, [Mult(d12, w2, [Single(d23, w3)])])));
    let t = insert  w4 empty in
    let d0 = D.zero in
    assert (t = Branch(Single(d0, w4)));
    let t = insert w5 t in
    let d45 = D.distance (fst w4) (fst w5) in
    assert (t = Branch(Mult(d0, w4, [Single(d45, w5)])));
    let t = insert w6 t in
    let d56 = D.distance (fst w5) (fst w6) in
    assert (t = Branch(Mult(d0, w4, [Mult(d45, w5, [Single(d56, w6)])])));
    let t = insert w7 t in
    let d67 = D.distance (fst w6) (fst w7) in
    assert (t = Branch(Mult(d0, w4, 
      [Mult(d45, w5, [Mult(d56, w6, [Single(d67, w7)])])])));
    let t = insert w8 t in
    let d68 = D.distance (fst w6) (fst w8) in
    assert (t = Branch(Mult(d0, w4, 
      [Mult(d45, w5, [Mult(d56, w6, [Single(d67, w7); Single(d68, w8)])])])));
    let t = insert w9 t in
    let d49 = D.distance (fst w4) (fst w9) in
    assert (t = Branch(Mult(d0, w4, 
      [Mult(d45, w5, [Mult(d56, w6, [Single(d67, w7); Single(d68, w8)])]); 
                        Single(d49, w9)])));
    let t = insert w10 t in
    let d9_10 = D.distance (fst w9) (fst w10) in
    assert (t = Branch(Mult(d0, w4, 
      [Mult(d45, w5, [Mult(d56, w6, [Single(d67, w7); Single(d68, w8)])]);
        Mult(d49, w9, [Single(d9_10, w10)])])));
    let t = insert w11 t in
    assert (t = test_tree1);
    ()

  let test_is_member () =
    assert (is_member "book" test_tree1);
    assert (not(is_member "bore" test_tree1));
    assert (is_member "books" test_tree1);
    assert (is_member "boo" test_tree1);
    assert (is_member "boon" test_tree1);
    assert (is_member "cook" test_tree1);
    assert (is_member "cake" test_tree1);
    assert (is_member "cape" test_tree1);
    assert (is_member "cart" test_tree1);
    assert (not(is_member "carts" test_tree1));
    ()
  
  let test_search () = 
    let (s1, s2, s3, s4, s5, s6) = 
      ("caqe", "boop", "care", "supercalifragilisticexs", "barn", "form") in
    assert (search s1 test_tree1 = ["cape"; "cake"]);
    assert (search s2 test_tree1 = ["boon"; "boo"; "book"]);
    assert (search s3 test_tree1 = ["cape"; "cake"]);
    assert (search s4 test_tree1 = []);
    assert (search s5 test_tree1 = []);
    assert (search s6 test_tree2 = ["form"]);
    ()
    
  let test_extract_d () = 
    let test_branch1 = 
      match test_tree1 with
      | Branch(br) -> br
      | _ -> raise EmptyTree in
    assert (extract_d test_branch1 = D.zero);
    assert (extract_d test_branch3 = D.distance (fst ("cake",0.2342)) 
                                      (fst ("cart",0.42094)));
    ()

  let run_tests () = 
    test_insert ();
    test_is_member ();
    test_search ();
    test_extract_d ();
    ()

end

(* run tests for Distance modules *)
let _ = DynamicLevDistance.run_tests
let _ = NaiveLevDistance.run_tests


(* Initialize BKTree modules *)
module BKTreeDynamic = 
  (BKTree(DynamicLevDistance) : BKTREE with type d = NaiveLevDistance.d)

module BKTreeNaive = 
  (BKTree(NaiveLevDistance) : BKTREE with type d = DynamicLevDistance.d)


(* run tests for BKTree modules *)
let _ = BKTreeDynamic.run_tests
let _ = BKTreeNaive.run_tests


(* The comparison runtime test for NaiveLevDistance and DynamicLevDistance *)
let _ =
  print_string "\nRuntimes for loading 500-word dictionary\n";
  print_string "  Dynamic Levenshtein Distance takes ";
  flush_all ();
  let start = Unix.gettimeofday () in 
  let _ = BKTreeDynamic.load_dict "../data/test_dict.txt" in
  let stop = Unix.gettimeofday () in
  print_float (stop -. start); print_string " seconds\n"; 
  flush_all ()

let _ =
   print_string "  Non-Dynamic Levenshtein Distance takes ";
  flush_all ();
  let start = Unix.gettimeofday () in 
  let _ = BKTreeNaive.load_dict "../data/test_dict.txt" in
  let stop = Unix.gettimeofday () in
  print_float (stop -. start); print_string " seconds\n\n"; 
  flush_all ()


(* Load the real dictionary *)
let _ = 
  print_string "Loading 100,000-word dictionary, please wait..."; 
  flush_all ()
let dict = BKTreeDynamic.load_dict "../data/cleaned_dict.txt"


(* uncomment to show some sample results 
let _ =
  print_string "\n\nSuggested words for 'hadvark' 'neccesssry' 'asent'\n";
  BKTreeDynamic.print_mult_result ["hadvark"; "neccesssry"; "asent"] dict
*)


(* Keep asking for an input from a user *)
try
  while true do
    print_string "\nInput: "; flush_all ();
    let word = input_line stdin in
    BKTreeDynamic.print_result word dict
    done;
  None
with
  End_of_file -> None
