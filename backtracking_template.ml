exception NotImplemented
exception Fail 
    
let split index grid =
  let rec aux i acc rest =
    match rest with
    | [] -> (List.rev acc, [])
    | h :: t ->
        if i = 0 then (List.rev acc, rest)
        else aux (i - 1) (h :: acc) t
  in
  aux index [] grid
;;


let transpose nono = 
  let len = List.length (List.hd nono) in
  let cols = List.fold_left (fun cols row -> List.map2 (fun col square -> (square::col)) cols row) (gen_list len []) nono
  in List.map (fun l -> List.rev l) cols
;;

let rec binary_permutations n =
  if n = 0 then [[]]
  else
    let smaller = binary_permutations (n - 1) in
    List.map (fun l -> Black :: l) smaller @ List.map (fun l -> White :: l) smaller
;;

let find_first_row_with_grays nono = 
  let rec find' nono row_id = 
    match nono with
    | [] -> raise NoGrays
    | x::xs -> if List.mem Unknown x then row_id else find' xs (row_id+1) in
  find' nono 0
;;

let replace_grays row_id combo nono =
  let rec replace' row combo = 
    match row with
    | [] -> []
    | x::xs -> if x = Unknown then (List.hd combo)::replace' xs (List.tl combo) else x::replace' xs combo in
  let new_row = replace' (List.nth nono row_id) combo in
  List.mapi (fun i r -> if i = row_id then new_row else r) nono
;;

(* Given a single row and associated clues for that row return a boolean true or false based on whether or not that row could be valid. In other words the function should return false if and only if it is impossible for the row to be valid given the associated clues, and true otherwise. 
  verify_row : cell list -> int list -> bool
  *)
let rec verify_row (row: row) (clues: int list) = raise NotImplemented

(* Use verify row and transpose to verify all the columns of a nonogram. return a boolean 
ver_cols : cell list list -> int list list -> bool
*)
let rec ver_cols nono clues = 
  raise NotImplemented

(* verify a nonogram by checking that all the rows and all the columns are valid. return a boolean 
ver_rows_and_cols : cell list list -> int list list -> int list list -> bool
*)
let rec ver_rows_and_cols nono row_cls col_cls = 
  raise NotImplemented


(* given a nonogram with some unknown cells generate a list of valid child nonograms by filling in the highest row with unknown cells with all possible valid configuration based on the row clue. Child nonograms must be valid by both row and column. 
generate_children : cell list list -> int list list -> int list list -> cell list list list
*)
let generate_children nono row_cls col_cls = 
  raise NotImplemented

(* Solve with Exceptions and Backtracking *)
(* Assume you have access to a function apply_rules grid -> int list list -> int list list that applies the rules recursivley until no more cells change by using the rules. The function should raise exception Fail if there does not exist a valid nonogram, otherwise it should return a valid nonogram. You function must use generate children and exceptions for backtracking. 
  solve_backtracking : int list list -> int list list -> cell list list
  *)
let solve_backtracking row_cls col_cls =  
  raise NotImplemented
;;