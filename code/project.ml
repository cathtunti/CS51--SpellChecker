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

  (* for BK to decide wheter to add a work to a return list*)
  val is_similar : int -> d -> bool

  (* for BK to decide which branch of the tree to crawl *)
  val in_range : int -> d -> d -> bool

  val compare : d -> d -> order

  val sort : int -> string -> (string * float) list -> string list

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
  val insert : string * float -> tree -> tree

  (* Search a BKtree for the given word. Return a list of tuples of the closest 
     word and the distance between them. *)
  val search : string -> tree -> string list

  (* returns true if word is the BKtree, false otherwise *)
  val is_member : string -> tree -> bool

  (* Same as search, but take in string list to search multiple strings at the
     same time. *)
  val multiple_search : string list -> tree -> string list list 

  (* Print out results of multiple_search in a readable format *)
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

  let is_similar (tolerance: int) (d: d) : bool =
    d <= tolerance

  let in_range (tolerance: int) (d1: d) (d2: d) : bool =
    abs(d1 - d2) <= tolerance

  let compare (d1: d) (d2: d) : order =
    if d1 = d2 then Equal 
    else if d1 > d2 then Greater
    else Less

  let sort (tolerance: int) (word: string) (wlst: (string * float) list) = 
    let sort2 = 
      let f (tuple1:string*float) (tuple2:string*float) = 
        let k = float tolerance in
        let prob_of_dist d = 999. *. (1000. ** (k -. float d)) /. (-1. +. 1000. ** (k +. 1.)) in
        let val1 = prob_of_dist (distance word (fst tuple1)) *. snd tuple1 in
        let val2 = prob_of_dist (distance word (fst tuple2)) *. snd tuple2 in
        if val1 > val2 then -1
        else if val2 > val1 then 1
        else 0 in
      List.sort ~cmp:(f) in
    List.map ~f:(fun x -> fst x) (sort2 wlst) 

  let run_tests =
    assert((distance "evidence" "providence") = 3);
    assert((distance "evidence" "provident") = 5);
    assert((distance "evidence" "provoident") = 6);
    assert((distance "cook" "snook") = 2);
    assert((distance "" "") = 0);
    assert((distance "CS51" "CS51") = 0);
    assert((distance "cool" "Cool") = 0);
    assert((distance "cook" "cok") = 1);
    ()

end

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


  (*****************************)
  (****                      ***)
  (** BKTree with Probability **)
  (*****************************)
  (*****************************)

(* implementation for BKtree *)
module BKtree_prob(D:DISTANCE) : BKTREE with type d=D.d =
struct

  exception EmptyTree
  exception NodeNotFound
  exception InvalidFile

  type d = D.d
  type branch = Single of d * (string * float) | Mult of (d * (string * float) * branch list)
  type tree = Empty | Branch of branch

  (* Returns an empty BKtree *)
  let empty = Empty

  let display_num = 10


  (********************)
  (* Helper Functions *)
  (********************)

  let same_word (w1: string) (w2: string) : bool = ((D.distance w1 w2) = D.zero)

  let extract_d (branch: branch) : d =
    match branch with
    | Single (d,_) | Mult (d,_,_) -> d


  (***********************)
  (* Interface Functions *)
  (***********************)
  
  (* tolerance +1 for every 5 chars *)
  let find_tole (word: string) : int = (String.length word) / 6 + 1 
      

  let search (word: string) (tree: tree) : string list = 
    let tole = find_tole word in
    let rec search_br (word: string) (br: branch) : (string * float) list = 
      let rec search_br_lst (word: string) (d_ori: d) (b_lst: branch list) 
        (return_lst: (string * float) list ) : (string * float) list =
        match b_lst with
        | [] -> return_lst
        | hd::tl -> 
            if (D.in_range tole d_ori (extract_d hd)) 
            then search_br word hd  @ (search_br_lst word d_ori tl return_lst)
            else (search_br_lst word d_ori tl return_lst) 
        in
      match br with
      | Single (_, (w,p)) -> 
          (* if within tolerance range then add to list *)
          if D.is_similar tole (D.distance word w) then [(w,p)]
          else []
      | Mult (_, (w,p), b_lst) -> 
          if D.is_similar tole (D.distance word w) 
          then (search_br_lst word (D.distance w word) b_lst [(w,p)]) 
          else (search_br_lst word (D.distance w word) b_lst [])
    in
    match tree with
    | Empty -> [] 
    | Branch b -> D.sort tole word (search_br word b)


  let is_member (word: string) (tree: tree) : bool = 
    let rec search_br (word: string) (br: branch) : bool =
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


  let rec multiple_search (w_lst: string list) (tree: tree) : string list list = 
    match w_lst with
    | [] -> []
    | hd::tl -> (search hd tree) :: (multiple_search tl tree)

  
  let rec truncate (len: int) (suggest: string list) : string list =
      if len = 0 then [] else 
      match suggest with
      | [] -> []
      | hd::tl -> hd::(truncate (len - 1) tl)

      
  let print_mult_result (input_lst: string list) (tree: tree) : unit = 
    let output = (multiple_search input_lst tree) in
    let rec str_big_lst (output: string list list) : string = 
      let rec str_sm_lst (output: string list) : string = 
        match (truncate display_num output) with
        | [] -> ""
        | hd::tl -> hd ^ " " ^ str_sm_lst tl in
      match output with
      | [] -> ""
      | hd::tl -> str_sm_lst hd ^ "\n" ^ str_big_lst tl in
    print_string (str_big_lst output); (flush_all ())

  let print_result (input: string) (tree: tree) : unit =
    print_mult_result [input] tree 


  let insert (word_p: string * float) (tree: tree) : tree = 
    let rec add_to_branch (word_p: string * float) (br: branch) : branch = 
      let (w, _) = word_p in
      let rec inject_to_lst (word_p: string * float) (d1: d) (b_lst: branch list) 
        : branch list =
        match b_lst with
        | [] -> [Single(d1, word_p)]
        | [hd] -> (match D.compare d1 (extract_d hd) with
                   | Equal -> [add_to_branch word_p hd]
                   | Less -> (inject_to_lst word_p d1 []) @ [hd]
                   | Greater -> hd::(inject_to_lst word_p d1 []))
        | hd::tl -> (match D.compare d1 (extract_d hd) with
                     | Equal -> (add_to_branch word_p hd)::tl
                     | Less -> (inject_to_lst word_p d1 []) @ hd::tl
                     | Greater -> hd::(inject_to_lst word_p d1 tl)) in
      match br with
      | Single (d, (s, p)) -> 
          if (same_word s w) then br 
          else Mult (d, (s, p), [Single ((D.distance s w), word_p)]) 
      | Mult (d, (s, p), b_lst) -> 
          if (same_word s w) then br 
          else Mult (d, (s, p), (inject_to_lst word_p (D.distance s w) b_lst)) in
    match tree with
    | Empty -> Branch (Single (D.zero, word_p))
    | Branch b -> Branch (add_to_branch word_p b)

  let load_dict (filename:string) : tree = 
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
    print_string "Loading dictionary, please wait..."; flush_all ();
    insert_tuplst (strlst_to_tuplst (In_channel.read_lines filename) []) empty
            
  (***********************)
  (*        Test         *)
  (***********************)
  let test_insert () = 
    let (w1, w2, w3) = (("book", 0.1111), ("books", 0.2222), ("boo", 0.3333)) in
    let t = insert w1 empty in
    let d0 = D.zero in
    assert (t = Branch(Single(d0, w1)));
    let t = insert  w2 t in
    let d12 = D.distance  (fst w1)  (fst w2) in 
    assert (t = Branch(Mult(d0, w1, [Single(d12, w2)])));
    let t = insert  w3 t in
    let d23 = D.distance  (fst w2)  (fst w3) in 
    assert (t = Branch(Mult(d0, w1, [Mult(d12, w2, [Single(d23, w3)])])));
    
    let (w4, w5, w6, w7, w8, w9, w10, w11) = 
      (("book", 0.1111), ("books", 0.2222), ("boo", 0.0001), ("boon", 0.0003), ("cook", 0.3234), ("cake", 0.2342), ("cape", 0.2345), ("cart", 0.42094)) in
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
    let d9_11 = D.distance (fst w9) (fst w11) in
    assert (t = Branch(Mult(d0, w4, 
      [Mult(d45, w5, [Mult(d56, w6, [Single(d67, w7); Single(d68, w8)])]); 
        Mult(d49, w9, [Single(d9_10, w10); Single(d9_11, w11)])])));
    ()

  let test_is_member () = raise ImplementMe
  
  let test_search () = 
     let (w4, w5, w6, w7, w8, w9, w10, w11) = 
      (("book", 0.1111), ("books",0.2222), ("boo", 0.0001), ("boon", 0.0003), ("cook", 0.3234), ("cake", 0.2342), ("cape", 0.2345), ("cart", 0.42094)) in
    let d0 = D.zero in
    let d45 = D.distance (fst w4) (fst w5) in
    let d56 = D.distance (fst w5) (fst w6) in
    let d67 = D.distance (fst w6) (fst w7) in
    let d68 = D.distance (fst w6) (fst w8) in
    let d49 = D.distance (fst w4) (fst w9) in
    let d9_10 = D.distance (fst w9) (fst w10) in
    let d9_11 = D.distance (fst w9) (fst w11) in
    let t =  Branch(Mult(d0, w4, 
      [Mult(d45, w5, [Mult(d56, w6, [Single(d67, w7); Single(d68, w8)])]); 
        Mult(d49, w9, [Single(d9_10, w10); Single(d9_11, w11)])])) in
    let (s1, s2, s3, s4, s5) = 
      ("caqe", "boop", "care", "supercalifragilisticexpialidocious", "barn") in
    assert (search s1 t = ["cape"; "cake"]);
    assert (search s2 t = ["boon"; "boo"; "book"]);
    assert (search s3 t = ["cape"; "cake"]);
    assert (search s4 t = []);
    assert (search s5 t = []);
    
    let (w12, w13, w14, w15) = (("pool", 0.0002), ("food", 0.2333),("form", 0.22222), ("pearl", 0.23421)) in
    let s6 = "form" in
    let d4_12 = D.distance (fst w4) (fst w12) in
    let d12_13 = D.distance (fst w12) (fst w13) in
    let d12_15 = D.distance (fst w12) (fst w15) in
    let d13_14 = D.distance (fst w13) (fst w14) in
    let t = Branch(Mult(d0, w4, 
      [Mult(d45, w5, [Mult(d56, w6, [Single(d67, w7); Single(d68, w8)])]); 
        Mult(d49, w9, [Single(d9_10, w10); Single(d9_11, w11)]);
          Mult(d4_12, w12, [Mult(d12_13, w13, 
            [Single(d13_14, w14)]); Single(d12_15, w15)])])) in
    assert (search s6 t = ["form"]);
    ()
    
  let test_same_word () =
    assert (same_word "moo" "moo" = true);
    assert (same_word "root" "moo" = false);
    assert (same_word "123" "moo" = false);
    assert (same_word "123" "123" = true);
    ()
    
  let test_extract_d () =
    let (w4, w5, w6, w7, w8, w9, w10, w11) = 
      (("book", 0.1111), ("books",0.2222), ("boo", 0.0001), ("boon", 0.0003), ("cook", 0.3234), ("cake", 0.2342), ("cape", 0.2345), ("cart", 0.42094)) in
    let d0 = D.zero in
    let d45 = D.distance (fst w4) (fst w5) in
    let d56 = D.distance (fst w5) (fst w6) in
    let d67 = D.distance (fst w6) (fst w7) in
    let d68 = D.distance (fst w6) (fst w8) in
    let d49 = D.distance (fst w4) (fst w9) in
    let d9_10 = D.distance (fst w9) (fst w10) in
    let d9_11 = D.distance (fst w9) (fst w11) in
    let t = Mult(d0, w4, 
      [Mult(d45, w5, [Mult(d56, w6, [Single(d67, w7); Single(d68, w8)])]); 
        Mult(d49, w9, [Single(d9_10, w10); Single(d9_11, w11)])]) in
    let t2 = Single(d9_11, w11) in
    assert (extract_d t = d0);
    assert (extract_d t2 = d9_11);
    ()

  let run_tests () = 
    test_insert ();
    test_is_member ();
    test_search ();
    test_same_word ();
    test_extract_d ();
    ()

end

let _ = NaiveLevDistance.run_tests
let _ = DynamicLevDistance.run_tests


module BKTree = 
  (BKtree_prob(DynamicLevDistance) : BKTREE with type d = DynamicLevDistance.d)

let _ = BKTree.run_tests

let dict = BKTree.load_dict "../data/cleaned_dict.txt"

try
  while true do
    print_string "\nInput: "; flush_all ();
    let word = input_line stdin in
    BKTree.print_result word dict
    done;
  None
with
  End_of_file -> None
;;







