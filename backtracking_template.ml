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
  if n = 0 then [[]] (* Base case: single empty list when n = 0 *)
  else
    let smaller = binary_permutations (n - 1) in
    (* Prepend 0 and 1 to all smaller permutations *)
    List.map (fun l -> Black :: l) smaller @ List.map (fun l -> White :: l) smaller
;;

let find_first_row_with_grays nono = 
  let rec find' nono row_id = 
    match nono with
    | [] -> raise NoGrays
    | x::xs -> if List.mem Unknown x then row_id else find' xs (row_id+1) in
  find' nono 0
;;

(* for a given row replace all the gray with the combo given and insert that into the nono  *)
let replace_grays row_id combo nono =
  let rec replace' row combo = 
    match row with
    | [] -> []
    | x::xs -> if x = Unknown then (List.hd combo)::replace' xs (List.tl combo) else x::replace' xs combo in
  let new_row = replace' (List.nth nono row_id) combo in
  List.mapi (fun i r -> if i = row_id then new_row else r) nono
;;

let rec verify_row (row: row) (clues: int list) = raise NotImplemented

let rec ver_cols nono clues = 
  raise NotImplemented

let rec ver_rows_and_cols nono row_cls col_cls = 
  raise NotImplemented

(* let insert_rows child_rows nono row_id = 
    List.mapi (fun i r -> if i = row_id then child_rows else r) nono
  ;; *)

let generate_children nono row_cls col_cls = 
  raise NotImplemented

(* Solve with Exceptions and Backtracking *)
(* Assume you have access to a function that uses the 11 rules for solving nonograms and applies them recursively until there are no more changes, and then a function that fills in gray squares to generate all posible new rows *)
let solve_backtracking row_cls col_cls =  
  raise NotImplemented
;;