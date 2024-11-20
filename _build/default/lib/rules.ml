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
  (* Helper function to find runs covering a specific cell, excluding a specific run *)
  let runs_covering_cell index exclude_run =
    List.filter (fun run ->
      run.start_pos <= index && index <= run.end_pos && run != exclude_run
    ) run_ranges
  in

  (* Iterate through all black runs *)
  List.fold_left (fun updated_row run ->
    let updated_row =
      if run.start_pos > 0 then
        let covering_runs = runs_covering_cell run.start_pos run in
        let all_lengths_one = List.for_all (fun r -> r.length = 1) covering_runs in
        if covering_runs != [] && all_lengths_one then
          update_row updated_row (run.start_pos - 1) White
        else
          updated_row
      else
        updated_row
    in

    if run.end_pos < List.length row - 1 then
      let covering_runs = runs_covering_cell run.end_pos run in
      let all_lengths_one = List.for_all (fun r -> r.length = 1) covering_runs in
      if covering_runs != [] && all_lengths_one then
        update_row updated_row (run.end_pos + 1) White
      else
        updated_row
    else
      updated_row
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
    (* Identify black segments within the range of the current run *)
    let black_segments =
      let rec find_segments i current_segment acc =
        if i > run.end_pos then
          if current_segment <> [] then List.rev (current_segment :: acc) else List.rev acc
        else if i < run.start_pos || List.length row <= i then
          find_segments (i + 1) [] acc
        else
          match List.nth row i with
          | Black -> find_segments (i + 1) (i :: current_segment) acc
          | _ -> if current_segment <> [] then find_segments (i + 1) [] (current_segment :: acc) else find_segments (i + 1) [] acc
      in
      find_segments run.start_pos [] []
    in

    (* Refine the range of the run based on black segments *)
    List.fold_left (fun updated_run segment ->
      let segment_start = List.hd (List.rev segment) in
      let segment_end = List.hd segment in
      let segment_length = segment_end - segment_start + 1 in
      if segment_length > run.length then
        (* Determine whether the segment belongs to the former or later black runs *)
        if segment_start >= updated_run.start_pos && segment_end <= updated_run.end_pos then
          if segment_start > updated_run.start_pos then
            { updated_run with start_pos = segment_end + 2 }
          else if segment_end < updated_run.end_pos then
            { updated_run with end_pos = segment_start - 2 }
          else
            updated_run
        else
          updated_run
      else
        updated_run
    ) run black_segments
  ) run_ranges


(* Rule 3.1: Fill gaps between scattered black segments *)
let rule_3_1 (run_ranges: run_range list) row =
  List.fold_left (fun (updated_run_ranges, updated_row) (j, run) ->
    (* Find the first colored cell (Black) after r(j-1)e *)
    let cm =
      let rec find_first_black i =
        if i >= run.end_pos then None
        else if List.nth updated_row i = Black then Some i
        else find_first_black (i + 1)
      in
      find_first_black (if j > 0 then (List.nth run_ranges (j - 1)).end_pos + 1 else 0)
    in

    (* Find the last colored cell (Black) before r(j+1)s *)
    let cn =
      let rec find_last_black i =
        if i < run.start_pos then None
        else if List.nth updated_row i = Black then Some i
        else find_last_black (i - 1)
      in
      find_last_black (if j < List.length run_ranges - 1 then (List.nth run_ranges (j + 1)).start_pos - 1 else List.length row - 1)
    in

    (* If both cm and cn are found, color all cells between cm and cn *)
    match (cm, cn) with
    | Some m, Some n ->
      let new_row =
        List.mapi (fun i cell ->
          if i >= m && i <= n then Black else cell
        ) row
      in

      (* Update rj_s and rj_e *)
      let u = run.length - (n - m + 1) in
      
      (updated_run_ranges@[{ run with start_pos = max 0 (m - u);
                                     end_pos = min (List.length row - 1) (n + u);}]
      , new_row)
    | _ -> (updated_run_ranges@[run], row)
  ) ([], row) (List.mapi (fun i run -> (i, run)) run_ranges)


(* Rule 3.2: Skip invalid segments and update ranges *)
let rule_3_2 run_ranges row =
  (* Helper function to find segments bounded by empty cells *)
  let find_segments start_idx end_idx =
    let rec aux i current_segment segments =
      if i > end_idx then
        if current_segment <> [] then List.rev (current_segment :: segments) else List.rev segments
      else
        match List.nth row i with
        | Black -> aux (i + 1) (i :: current_segment) segments
        | White | Unknown ->
          if current_segment <> [] then aux (i + 1) [] (current_segment :: segments)
          else aux (i + 1) [] segments
    in
    aux start_idx [] []
  in

  (* Process each run *)
  List.fold_left (fun (updated_run_ranges, updated_row) run ->
    let segments = find_segments run.start_pos run.end_pos in
    let lbj = run.length in

    (* Step 1 & 2: Find the first segment >= LBj *)
    let new_start =
      List.fold_left (fun acc segment ->
        if acc = None && List.length segment >= lbj then Some (List.hd (List.rev segment))
        else acc
      ) None segments
    in

    (* Step 3 & 4: Find the last segment >= LBj *)
    let new_end =
      List.fold_left (fun acc segment ->
        if acc = None && List.length segment >= lbj then Some (List.hd segment)
        else acc
      ) None (List.rev segments)
    in

    (* Update run range *)
    let updated_run = {
      run with
      start_pos = (match new_start with Some s -> s | None -> run.start_pos);
      end_pos = (match new_end with Some e -> e | None -> run.end_pos);
    } in

    (* Step 5: Mark remaining segments < LBj as White *)
    let updated_row =
      List.fold_left (fun row segment ->
        if List.length segment < lbj then
          let belongs_to_other_runs =
            List.exists (fun r ->
              r != run && r.start_pos <= List.hd segment && List.hd segment <= r.end_pos
            ) updated_run_ranges
          in
          if not belongs_to_other_runs then
            List.mapi (fun i cell -> if List.mem i segment then White else cell) row
          else
            row
        else
          row
      ) updated_row segments
    in

    (updated_run :: updated_run_ranges, updated_row)
  ) ([], row) (List.rev run_ranges)


(* Rule 3.3: Handle non-overlapping ranges *)
let rule_3_3 run_ranges row =
  List.mapi (fun i cell ->
      if cell = Unknown then
        let overlapping = List.filter (fun run -> run.start_pos <= i && i <= run.end_pos) run_ranges in
        if List.length overlapping = 1 then Black else cell
      else cell
    ) row


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
    let new_run_ranges = rule_2_1 run_ranges in
    (*let new_run_ranges = rule_2_2 run_ranges new_grid in *)
    let new_run_ranges = rule_2_3 run_ranges new_grid in
    let (new_run_ranges, new_grid) = rule_3_1 run_ranges new_grid in
  (*  let run_ranges = rule_3_2 run_ranges new_grid in
    let new_grid = rule_3_3 run_ranges new_grid in *)
    if new_grid = grid && new_run_ranges = run_ranges
      then (grid, run_ranges)
      else iterate new_grid
  in
  iterate grid