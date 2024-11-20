open Rules

type cell = Black | White | Unknown 
type row = cell list
type grid = row list

type run_range = { start_pos : int; end_pos : int; length : int }

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
let apply_logical_rules grid run_ranges =
  let rec iterate grid =
    (* Apply all rules and determine changes *)
    let new_grid = grid in
    (* Apply each rule in sequence *)
    let new_grid = rule_1_1 run_ranges new_grid in
    let new_grid = rule_1_2 run_ranges new_grid in
    let new_grid = rule_1_3 run_ranges new_grid in
    let new_grid = rule_1_4 run_ranges new_grid in
    let new_grid = rule_1_5_part1 run_ranges new_grid in
    let new_grid = rule_1_5_part2 run_ranges new_grid in
    let run_ranges = rule_2_1 run_ranges in
    let run_ranges = rule_2_2 run_ranges new_grid in
    let run_ranges = rule_2_3 run_ranges new_grid in
    let (run_ranges, new_grid) = rule_3_1 run_ranges new_grid in
    let run_ranges = rule_3_2 run_ranges new_grid in
    let new_grid = rule_3_3 run_ranges new_grid in
    if new_grid = grid then grid else iterate new_grid
  in
  iterate grid

(* Backtracking function placeholder *)
let backtrack grid run_ranges = 
  failwith "Backtracking not implemented yet"

(* Main solver function *)
let solve_nonogram grid run_ranges =
  let logical_solved = apply_logical_rules grid run_ranges in
  backtrack logical_solved run_ranges
