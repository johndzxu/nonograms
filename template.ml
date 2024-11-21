type cell = Black | White | Unknown 
type row = cell list
type grid = row list
exception NotImplemented
exception Fail
exception NoGrays  
  
type run = {start_pos: int; end_pos: int; length: int}

    (* ------ GIVEN FUNCTIONS ----------*)
let init_grid rows cols =
      let rec init_row (row: cell list) (cols: int): cell list =
        match cols with
        | 0 -> row
        | _ -> init_row (Unknown::row) (cols-1)
      in
    
      let rec init_grid' (row: cell list) (grid: row list) (rows: int) =
        match rows with
        | 0 -> grid
        | _ -> init_grid' row (row::grid) (rows-1)
      in
    
      let row = init_row [] cols in
      init_grid' row [] rows
    
let gen_list n e =
  let rec gen' n cont = 
    match n with
    | 0 -> cont []
    | _ -> gen' (n-1) (fun l -> cont (e::l)) in gen' n (fun a -> a) 
    
let split index grid =
  let rec aux i acc rest =
    match rest with
    | [] -> (List.rev acc, [])
    | h :: t ->
        if i = 0 then (List.rev acc, rest)
        else aux (i - 1) (h :: acc) t
  in
  aux index [] grid
    

let transpose nono = 
  let len = List.length (List.hd nono) in
  let cols = List.fold_left (fun cols row -> List.map2 (fun col square -> (square::col)) cols row) (gen_list len []) nono
  in List.map (fun l -> List.rev l) cols
    

let rec binary_permutations n =
  if n = 0 then [[]]
  else
    let smaller = binary_permutations (n - 1) in
    List.map (fun l -> Black :: l) smaller @ List.map (fun l -> White :: l) smaller
      

let find_first_row_with_grays nono = 
  let rec find' nono row_id = 
    match nono with
    | [] -> raise NoGrays
    | x::xs -> if List.mem Unknown x then row_id else find' xs (row_id+1) in
  find' nono 0
    

let replace_grays row_id combo nono =
  let rec replace' row combo = 
    match row with
    | [] -> []
    | x::xs -> if x = Unknown then (List.hd combo)::replace' xs (List.tl combo) else x::replace' xs combo in
  let new_row = replace' (List.nth nono row_id) combo in
  List.mapi (fun i r -> if i = row_id then new_row else r) nono
    

(* -------- TEMPLATES ----------------*)
let update_row (row: cell list) (index: int) (value: cell) =
  List.mapi (fun i cell -> if i = index then value else cell) row

(* Color the intersection of all possible solutions black *)
let rule_1_1 (row: row) (runs: run list): row =
  raise NotImplemented

(* Color cells that does not belong to any run ranges white *)
let rule_1_2 (row: row) (runs: run list): row =
  raise NotImplemented

(* If first or last cell of a run range is black,
and all runs covering the cell other than the current run have length one,
color the cell before or after white *)
let rule_1_3 (row: cell list) (runs: run list): cell list =
  raise NotImplemented

(* If the start of a run range is before the start of the previous run range,
or if the end of a run range is after the end of the next run range,
  update the run range *)
let rule_2_1 (row: cell list) (runs: run list): run list =
  raise NotImplemented

(* Ensure there is a white cell between consecutive black runs *)
let rule_2_2 (row: cell list) (runs: run list): run list =
  raise NotImplemented

(* Connect black segments if it belongs to only one run range *)
let rule_3_1 (row: cell list) (runs: run list): cell list * run list =
  raise NotImplemented

(* Initialize run ranges given the row clues *)
let init_runs (row: cell list) (row_cls: int list): run list =
  raise NotImplemented
  
(* Apply rules to a row *)
let apply_rules_row (row: cell list) (runs: run list): cell list =
  raise NotImplemented
  
(* Apply rules to all rows in a grid *)
let apply_rules_rows (grid: row list) (cls: int list list): row list = 
  let runs = (List.map2 init_runs grid cls) in
  let grid' = (List.map2 apply_rules_row grid runs) in
  grid'
  
(* Apply rules to all rows and columns in a nonogram *)
let apply_rules (nono: row list) (row_cls: int list list) (col_cls: int list list): row list =
  let rec iterate nono =
    let nono' = apply_rules_rows nono row_cls in
    let nono' = transpose nono' in
    let nono' = apply_rules_rows nono' col_cls in
    let nono' = transpose nono' in
    if nono' = nono then nono else (iterate nono') in
  iterate nono
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
  
(*
Return all possible rows of length 'cols' that satsisfy the clue in 'clues'.
Do so with a success continuation *)
let rec all_rows' (clues: int list) (cols: int) (sc: int list list -> 'a) : 'a =
  raise NotImplemented

let all_rows (clues: int list) (cols: int) = all_rows' clues cols (fun a -> a)

(* given a nonogram with some unknown cells generate a list of valid child nonograms by filling in the highest row with unknown cells with all possible valid configuration based on the row clue. Child nonograms must be valid by both row and column. 
generate_children : cell list list -> int list list -> int list list -> cell list list list
*)
let generate_children nono row_cls col_cls = 
  raise NotImplemented 
  
(*Solve the nonogram to give exactly 1 valid result with row clues '
row_cls_ and column clues in 'col_cls' with Exceptions*)
let solve (row_cls: int list) (col_cls: int list) : grid = 
  
  let rec s_row (row_cls: int list) (nono: grid) = 
    raise NotImplemented
  and s_stack (row_cls: int list) (nono: grid) (stack: grid) = 
    raise NotImplemented
    
  in raise NotImplemented
  
(* Solve the nonogram to give exactly 1 valid result with row clues '
  row_cls_ and column clues in 'col_cls' with a fail continuation*)
let solve_cont (row_cls: int list) (col_cls: int list) : grid = 
  
  let rec s_row (row_cls: int list) (nono: grid) (fc: unit -> grid) : grid =
    raise NotImplemented
      
  and s_stack (row_cls: int list) (nono: grid) (stack: grid) (fc: unit -> grid) : grid =
    raise NotImplemented 
      
  in raise NotImplemented
  
(*Find ALL solutions to the given nonogram with a success continuation*)
let solve_all row_cls col_cls = 
  
  let rec s_row (row_cls: int list) (nono: grid) (sc: grid list -> 'a) : 'a = 
    raise NotImplemented
      
  and s_stack (row_cls: int list) (nono: grid) (stack: grid) (sc: grid list -> 'a) : 'a = 
    raise NotImplemented 
  
  in raise NotImplemented
    
    (* Solve with Exceptions and Backtracking *)
(* Assume you have access to a function apply_rules grid -> int list list -> int list list that applies the rules recursivley until no more cells change by using the rules. The function should raise exception Fail if there does not exist a valid nonogram, otherwise it should return a valid nonogram. You function must use generate children and exceptions for backtracking. 
  solve_backtracking : int list list -> int list list -> cell list list
*)
let solve_backtracking row_cls col_cls =  
  raise NotImplemented
;;

  