type cell = Black | White | Unknown 
type row = cell list
type grid = row list

type run_range = {  start_pos : int;  end_pos : int; length : int }


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
let rule_1_5_part1 run_ranges row =
  List.fold_left (fun updated_row (i, cell) ->
    if cell = Black then
      let rec find_wall idx direction =
        if idx < 0 || idx >= List.length updated_row then None
        else match List.nth updated_row idx with
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
      let row_with_left = if expand_left && i + 1 < List.length updated_row then update_row updated_row (i + 1) Black else updated_row in
      let row_with_both = if expand_right && i - 1 >= 0 then update_row row_with_left (i - 1) Black else row_with_left in
      row_with_both
    else
      updated_row
  ) row (List.mapi (fun i cell -> (i, cell)) row)

  (* Helper function to find a black segment containing a given index *)
let find_segment row index =
  let rec find_start idx =
    if idx < 0 || List.nth row idx <> Black then idx + 1
    else find_start (idx - 1)
  in
  let rec find_end idx =
    if idx >= List.length row || List.nth row idx <> Black then idx - 1
    else find_end (idx + 1)
  in
  let start = find_start index in
  let finish = find_end index in
  (start, finish)

(* Helper function to get all black runs covering a specific index *)
let runs_covering_index index run_ranges =
  List.filter (fun run -> run.start_pos <= index && index <= run.end_pos) run_ranges

(* Rule 1.5 (second part) implementation *)
let rule_1_5_part2 run_ranges row =
  List.fold_left (fun updated_row (i, cell) ->
    if cell = Black then
      (* Find the segment containing the current cell *)
      let (s, e) = find_segment updated_row i in
      let segment_length = e - s + 1 in
      (* Find all runs covering this segment *)
      let covering_runs = runs_covering_index i run_ranges in
      let all_same_length =
        List.for_all (fun run -> run.length = segment_length) covering_runs
      in
      if all_same_length then
        (* Mark cells before s and after e as White if within bounds *)
        let updated_row = if s - 1 >= 0 then update_row updated_row (s - 1) White else updated_row in
        let updated_row = if e + 1 < List.length updated_row then update_row updated_row (e + 1) White else updated_row in
        updated_row
      else
        updated_row
    else
      updated_row
  ) row (List.mapi (fun i cell -> (i, cell)) row)



(* Rule 2.1: Adjust ranges based on sequence constraints *)
let rule_2_1 run_ranges =
  let rec adjust_ranges ranges =
    match ranges with
    | [] -> []
    | [last] -> [last]
    | first :: (second :: rest) ->
        let new_first = 
          { first with end_pos = min first.end_pos (second.end_pos - second.length - 1) }
        in
        let new_second = 
          { second with start_pos = max second.start_pos (new_first.start_pos + new_first.length + 1) }
        in
        new_first :: adjust_ranges (new_second :: rest)
  in
  adjust_ranges run_ranges


(* Rule 2.2: Adjust ranges based on adjacent colored cells *)
let rule_2_2 run_ranges row =
  List.map (fun run_range ->
    let new_start =
      if run_range.start_pos > 0 && List.nth row (run_range.start_pos - 1) = Black then
        run_range.start_pos + 1
      else
        run_range.start_pos
    in
    let new_end =
      if run_range.end_pos < List.length row - 1 && List.nth row (run_range.end_pos + 1) = Black then
        run_range.end_pos - 1
      else
        run_range.end_pos
    in
    { run_range with start_pos = new_start; end_pos = new_end }
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
