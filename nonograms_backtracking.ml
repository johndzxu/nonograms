exception NotImplemented
exception Fail 
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

(* Rule 1.1: Paint cells that are common to all possible solutions *)
let rule_1_1 run_ranges row =
  List.fold_left (fun acc_range run ->
      let intersection_start = max run.start_pos (run.end_pos - run.length + 1) in
      let intersection_end = min run.end_pos (run.start_pos + run.length - 1) in
      List.mapi (fun i cell ->
          if i >= intersection_start && i <= intersection_end && cell = Unknown then
            Black
          else
            cell
        ) acc_range
    ) row run_ranges

(* Rule 1.2: Leave cells outside all possible ranges as empty *)
let rule_1_2 run_ranges row =
  let bounds = List.fold_left (fun (min_start, max_end) range ->
      (min min_start range.start_pos, max max_end range.end_pos)
    ) (max_int, min_int) run_ranges in
  List.mapi (fun i cell ->
      if i < fst bounds || i > snd bounds then White else cell
    ) row

(* Rule 1.3: Handle edge cases with length-1 black runs *)
let rule_1_3 run_ranges row =
  List.fold_left (fun acc_range run ->
      if run.length = 1 then
        let acc_range = if List.nth acc_range run.start_pos = Black then
            update_row acc_range (run.start_pos - 1) White
          else
            acc_range
        in
        if List.nth acc_range run.end_pos = Black then
          update_row acc_range (run.end_pos + 1) White
        else
          acc_range
      else
        acc_range
    ) row run_ranges

(* Rule 1.4: Prevent over-expanding black segments *)
let rule_1_4 run_ranges row =
  List.mapi (fun i cell ->
      match cell with
      | Unknown ->
        (* Check neighboring black segments *)
          let left_black = if i > 0 then List.nth row (i - 1) = Black else false in
          let right_black = if i < List.length row - 1 then List.nth row (i + 1) = Black else false in
          let segment_length =
            let rec compute_length idx direction acc =
              if idx < 0 || idx >= List.length row then acc
              else if List.nth row idx = Black then compute_length (idx + direction) direction (acc + 1)
              else acc
            in
            let left_length = compute_length (i - 1) (-1) 0 in
            let right_length = compute_length (i + 1) 1 0 in
            left_length + right_length + 1
          in
        (* Determine max length allowed for the overlapping runs *)
          let max_length = List.fold_left (fun max_len run ->
              if i >= run.start_pos && i <= run.end_pos then max max_len run.length else max_len
            ) 0 run_ranges in
          if left_black || right_black then
            if segment_length > max_length then White else Unknown
          else Unknown
      | _ -> cell
    ) row

(* Rule 1.5: Handle "walls" created by empty cells *)
let rule_1_5 run_ranges row =
  List.mapi (fun i cell ->
      if cell = Black then
      (* Check for "walls" on both sides and color accordingly *)
        let rec find_wall idx direction =
          if idx < 0 || idx >= List.length row then None
          else match List.nth row idx with
            | White -> Some idx
            | _ -> find_wall (idx + direction) direction
        in
        let left_wall = find_wall (i - 1) (-1) in
        let right_wall = find_wall (i + 1) 1 in
        let expand_left = match left_wall with
          | Some lw -> i - lw <= List.fold_left (fun min_len run -> min min_len run.length) max_int run_ranges
          | None -> false
        in
        let expand_right = match right_wall with
          | Some rw -> rw - i <= List.fold_left (fun min_len run -> min min_len run.length) max_int run_ranges
          | None -> false
        in
        if expand_left || expand_right then Black else cell
      else cell
    ) row

(* Rule 2.1: Adjust ranges based on sequence constraints *)
let rule_2_1 run_ranges row =
  let rec adjust_ranges ranges =
    match ranges with
    | [] -> []
    | [last] -> [last]
    | first :: (second :: _ as rest) ->
        let new_first = 
          { first with end_pos = min first.end_pos (second.start_pos - second.length - 1) }
        in
        let new_second = 
          { second with start_pos = max second.start_pos (new_first.end_pos + new_first.length + 1) }
        in
        new_first :: adjust_ranges (new_second :: List.tl rest)
  in
  adjust_ranges run_ranges

(* Rule 2.2: Adjust ranges based on adjacent colored cells *)
let rule_2_2 run_ranges row =
  List.map (fun run ->
      let start = if List.nth row run.start_pos = Black then run.start_pos + 1 else run.start_pos in
      let end_pos = if List.nth row run.end_pos = Black then run.end_pos - 1 else run.end_pos in
      { run with start_pos = start; end_pos = end_pos }
    ) run_ranges

(* Rule 2.3: Refine ranges for overlapping black segments *)
let rule_2_3 run_ranges row =
  List.map (fun run ->
      let overlapping_segments =
        List.fold_left (fun segments i ->
            if List.nth row i = Black then
              match segments with
              | [] -> [(i, i)]
              | (start, finish) :: rest when i = finish + 1 -> (start, i) :: rest
              | _ -> (i, i) :: segments
            else segments
          ) [] (list_init (run.end_pos - run.start_pos + 1) (fun i -> i + run.start_pos))
      in
      List.fold_left (fun refined_run (start, finish) ->
          if finish - start + 1 > run.length then
            if start > refined_run.start_pos then { refined_run with end_pos = start - 2 }
            else if finish < refined_run.end_pos then { refined_run with start_pos = finish + 2 }
            else refined_run
          else refined_run
        ) run overlapping_segments
    ) run_ranges

(* Rule 3.1: Fill gaps between scattered black segments *)
let rule_3_1 run_ranges row =
  List.mapi (fun i cell ->
      if cell = Unknown then
        let left_black = List.exists (fun run -> run.start_pos <= i && i <= run.end_pos && List.nth row run.start_pos = Black) run_ranges in
        let right_black = List.exists (fun run -> run.start_pos <= i && i <= run.end_pos && List.nth row run.end_pos = Black) run_ranges in
        if left_black && right_black then Black else cell
      else cell
    ) row

(* Rule 3.2: Skip invalid segments and update ranges *)
let rule_3_2 run_ranges row =
  List.map (fun run ->
      let valid_start = 
        List.find_opt (fun i -> List.nth row i = Black) (list_init (run.end_pos - run.start_pos + 1) (fun i -> i + run.start_pos))
      in
      let valid_end =
        rev_find_opt (fun i -> List.nth row i = Black) (list_init (run.end_pos - run.start_pos + 1) (fun i -> i + run.start_pos))
      in
      match (valid_start, valid_end) with
      | (Some s, Some e) -> { run with start_pos = s; end_pos = e }
      | _ -> run
    ) run_ranges

(* Rule 3.3: Handle non-overlapping ranges *)
let rule_3_3 run_ranges row =
  List.mapi (fun i cell ->
      if cell = Unknown then
        let overlapping = List.filter (fun run -> run.start_pos <= i && i <= run.end_pos) run_ranges in
        if List.length overlapping = 1 then Black else cell
      else cell
    ) row

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
    let new_grid = rule_1_5 run_ranges new_grid in
    let run_ranges = rule_2_1 run_ranges new_grid in
    let run_ranges = rule_2_2 run_ranges new_grid in
    let run_ranges = rule_2_3 run_ranges new_grid in
    let new_grid = rule_3_1 run_ranges new_grid in
    let run_ranges = rule_3_2 run_ranges new_grid in
    let new_grid = rule_3_3 run_ranges new_grid in
    if new_grid = grid then grid else iterate new_grid
  in
  iterate grid

let gen_list n e =
  let rec gen' n cont = 
    match n with
    | 0 -> cont []
    | _ -> gen' (n-1) (fun l -> cont (e::l)) in gen' n (fun a -> a) 
  
let rec all_rows' clues cols sc =
  match (clues, cols) with
  | ([], _) -> sc [gen_list cols White]
  | (x::xs, _) when cols<x -> sc []
  | (x::xs, _) when cols=x -> all_rows' xs 0 (fun l -> sc (List.map (fun l -> (gen_list x Black)@l) l))
  | (x::xs, _) -> all_rows' xs (cols-x-1) (fun l1 ->
      all_rows' clues (cols-1) (fun l2 ->
          sc ((List.map (fun l3 -> (gen_list x Black)@[White]@l3) l1)@(List.map (fun l3 -> White::l3) l2))))
                                
let all_rows clues cols = all_rows' clues cols (fun a -> a) 
    
let rec ver_col clues col exp =
  match (clues, col) with
  | ([],[]) -> true
  | ([], x::xs) when exp = Black -> false
  | ([], x::xs) -> (x=White)&&ver_col [] xs White
  | (x::xs, []) -> false
  | (cl::cls, c::cs) when exp=Black -> if c = Black then match (cl-1) with
      | 0 -> ver_col cls cs White 
      | n -> ver_col (n::cls) cs Black
      else false
  | (cl::cls, c::cs) when exp=White -> if c = Black then false else
        ver_col (cl::cls) cs Unknown
  | (cl::cls, c::cs) when exp=Unknown -> if c = Black then match (cl-1) with
      | 0 -> ver_col cls cs White 
      | n -> ver_col (n::cls) cs Black
      else ver_col (cl::cls) cs Unknown
  | _ -> raise Fail
           
let transpose nono = 
  let len = List.length (List.hd nono) in
  let cols = List.fold_left (fun cols row -> List.map2 (fun col square -> (square::col)) cols row) (gen_list len []) nono
  in List.map (fun l -> List.rev l) cols
    
          
let rec ver_grid nono clues = 
  let clue_cols = List.map2 (fun clue col -> (clue,col)) clues (transpose nono) in
  List.for_all (fun (clue, col) -> ver_col clue col Unknown) clue_cols
  
(* Solve with Exceptions and Backtracking *)
(* Assume you have access to a function that uses the 11 rules for solving nonograms and applies them recursively until there are no more changes, and then a function that fills in gray squares to generate all posible new rows *)
let solve row_cls col_cls =  
let rec s_row row_cls col_cls nono = 
    if ver_grid nono col_cls then 
      let new_nono = apply_rules nono in 
      let children = generate_children new_nono in 
      try_children row_cls col_cls children
    else raise Fail
  and try_children row_cls col_cls children = 
    match children with 
    | [] -> raise Fail
    | x::xs -> (
        try s_row row_cls col_cls x 
        with Fail -> s_stack row_cls col_cls nono xs
      ) 

;;


let find_first_row_with_grays nono = 
  let rec find' nono row_id = 
    match nono with
    | [] -> raise Fail
    | x::xs -> if List.mem Unknown x then row_id else find' xs (row_id+1) in
  find' nono 0
;;

let number_of_grays row_id nono = 
  List.length (List.filter (fun x -> x = Unknown) (List.nth nono row_id))
;;

let rec binary_permutations n =
  if n = 0 then [[]] (* Base case: single empty list when n = 0 *)
  else
    let smaller = binary_permutations (n - 1) in
    (* Prepend 0 and 1 to all smaller permutations *)
    List.map (fun l -> White :: l) smaller @ List.map (fun l -> Black :: l) smaller
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

(* let insert_rows child_rows nono row_id = 
    List.mapi (fun i r -> if i = row_id then child_rows else r) nono
  ;; *)

let generate_children nono =
    let row_id = find_first_row_with_grays nono in
    let gray_combos = binary_permutations (number_of_grays row_id nono) in
    List.map (fun combo -> replace_grays row_id combo nono) gray_combos
    (* filter based on row and column clues? *)
  ;;
  
let empty_nono rows cols = 
  let rec empty' cols = 
    match cols with 
    | 0 -> []
    | _ -> (gen_list rows Unknown)::empty' (cols-1) in
  empty' cols
  ;;