open Rules

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

let rec list_init n f =
  if n <= 0 then []
  else f (n - 1) :: list_init (n - 1) f

         
let rev_find_opt predicate lst =
  let rec aux = function
    | [] -> None
    | x :: xs -> 
        match aux xs with
        | Some _ as result -> result
        | None -> if predicate x then Some x else None
  in
  aux lst


let rec verify_col (grid: grid) (clues: int list) i =
  match clues with 
  | [] -> List.for_all (fun x -> (List.nth x i = White || List.nth x i = Unknown)) grid
  | h::t -> 
      if List.nth (List.hd grid) i = Black then 
        let (firstk, rest) = split h grid in 
        if List.for_all (fun x -> (List.nth x i = Black || List.nth x i = Unknown)) (firstk) then ( 
          match rest with 
          | [] -> (clues = [])
          | r::rows -> if List.nth r i = White || List.nth r i = Unknown then verify_col rows t i else false
        ) else false 
      else if List.nth (List.hd grid) i = White then let r::rows = grid in verify_col rows clues i
      else
        (
          let (firstk, rest) = split h grid in 
          if List.for_all (fun x -> (List.nth x i = Black || List.nth x i = Unknown)) (firstk) then ( 
            match rest with 
            | [] -> (clues = [])
            | r::rows -> if List.nth r i  = White || List.nth r i  = Unknown then verify_col rows t i else false
          ) else false
        ) || let r::rows = grid in verify_col rows clues i
;;

(* A helper function to initialize a grid with unknown cells *)
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

(* Utility function to update a cell in a row *)
let update_row row index value =
  List.mapi (fun i cell -> if i = index then value else cell) row


(* Apply all logical rules iteratively *)
(* Function to initialize run ranges based on the row and clues *)
let initialize_run_ranges row row_clues =
  let n = List.length row in
  let k = List.length row_clues in

  (* Helper function to calculate the sum of (LB_i + 1) for given range *)
  let sum_clues_with_gaps clues from_idx to_idx =
    List.fold_left (fun acc i -> acc + List.nth clues i + 1) 0 (list_init (to_idx - from_idx + 1) (fun i -> from_idx + i))
  in

  (* Compute each run's start and end positions *)
  List.mapi (fun j lbj ->
    let start_pos =
      if j = 0 then 0
      else sum_clues_with_gaps row_clues 0 (j - 1)
    in
    let end_pos =
      if j = k - 1 then n - 1
      else (n - 1) - sum_clues_with_gaps row_clues (j + 1) (k - 1)
    in
    { start_pos; end_pos; length = lbj }
  ) row_clues



let apply_rules_row row run_ranges =
  let rec iterate row run_ranges =
    (* Apply each rule in sequence *)
    let new_row = rule_1_1 run_ranges row in
    let new_row = rule_1_2 run_ranges new_row in
    let new_row = rule_1_3 run_ranges new_row in
    let new_row = rule_1_4 run_ranges new_row in
    let new_row = rule_1_5_part1 run_ranges new_row in
    let new_row = rule_1_5_part2 run_ranges new_row in
    let new_run_ranges = rule_2_1 run_ranges in
    (*let new_run_ranges = rule_2_2 new_run_ranges new_row in *)
    let new_run_ranges = rule_2_3 new_run_ranges new_row in
    let (new_run_ranges, new_row) = rule_3_1 new_run_ranges new_row in
  (*  let run_ranges = rule_3_2 run_ranges new_row in
    let new_row = rule_3_3 run_ranges new_row in *)
    if new_row = row && new_run_ranges = run_ranges
      then new_row
      else iterate new_row new_run_ranges
  in
  iterate row run_ranges

let apply_rules_rows (grid: row list) (clues: int list list): grid = 
  let run_ranges_grid = (List.map2 initialize_run_ranges grid clues) in
  let new_grid = (List.map2 apply_rules_row grid run_ranges_grid) in
  new_grid

let gen_list n e =
  let rec gen' n cont = 
    match n with
    | 0 -> cont []
    | _ -> gen' (n-1) (fun l -> cont (e::l)) in gen' n (fun a -> a) 
    
let transpose nono = 
  let len = List.length (List.hd nono) in
  let cols = List.fold_left (fun cols row -> List.map2 (fun col square -> (square::col)) cols row) (gen_list len []) nono
  in List.map (fun l -> List.rev l) cols

let apply_logical_rules (grid: row list) (row_clues: int list list) (col_clues: int list list): grid =
  let rec iterate grid =
  let new_grid = apply_rules_rows grid row_clues in
  let new_grid = transpose new_grid in
  let new_grid = apply_rules_rows new_grid col_clues in
  let new_grid = transpose new_grid in
  if new_grid = grid then grid else (iterate new_grid) in

  iterate grid

(* Backtracking function placeholder *)
let backtrack grid run_ranges = 
  failwith "Backtracking not implemented yet"

(* Main solver function *)
let solve_nonogram grid run_ranges =
  let logical_solved = apply_logical_rules grid run_ranges in
  backtrack logical_solved run_ranges


let grid =
  [[Unknown; Unknown; Unknown; Unknown];
   [Unknown; Unknown; Unknown; Unknown];
   [Unknown; Unknown; Unknown; Unknown];
   [Unknown; Unknown; Unknown; Unknown]];;
let row_clues = [[1]; [1; 2]; [2; 1]; [1; 1]];;
let col_clues = [[4]; [1]; [1]; [3]];;
apply_logical_rules grid row_clues col_clues ;;

