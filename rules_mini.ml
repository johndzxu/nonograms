type cell = Black | White | Unknown 
type row = cell list
type grid = row list

type run = {start_pos: int; end_pos: int; length: int}

(* Functions from elsewhere *)
(* START *)
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
    
let transpose nono = 
  let len = List.length (List.hd nono) in
  let cols = List.fold_left (fun cols row -> List.map2 (fun col square -> (square::col)) cols row) (gen_list len []) nono
  in List.map (fun l -> List.rev l) cols

(* END *)

(* Intersection of all possibilities *)
let rule_1_1 row runs =
  List.fold_left (fun row' run ->
    let u = (run.end_pos - run.start_pos + 1 - run.length) in
    let intersection_start = (run.start_pos + u) in
    let intersection_end = (run.end_pos - u) in
      List.mapi (fun i cell ->
          if i >= intersection_start && i <= intersection_end && cell = Unknown then
            Black
          else
            cell
        ) row'
    ) row runs


(* Does not belong to any run *)
let rule_1_2 row runs =
  List.mapi (fun i cell ->
    if (not (List.exists (fun run -> run.start_pos <= i && i <= run.end_pos) runs))
      && (cell = Unknown)
      then White
    else cell
    ) row


(* Eliminate overlaps *)
let rule_2_1 row runs =
  List.mapi (fun i run -> 
    let left_bound =
      if (i > 0) then
        (List.nth runs (i - 1)).start_pos + (List.nth runs (i - 1)).length + 1
      else 0 in
    let right_bound =
      if (i < List.length runs - 1) then
        (List.nth runs (i + 1)).end_pos - (List.nth runs (i + 1)).length - 1
      else (List.length row - 1) in
    if (run.start_pos < left_bound)
      then {run with start_pos = left_bound}
    else
    if (run.end_pos > right_bound)
      then {run with end_pos = right_bound}
    else run)
  runs

(* White cell between runs *)
let rule_2_2 row runs =
  List.map (fun run ->
    let start' = if run.start_pos > 0 && List.nth row (run.start_pos - 1) = Black
                  then (run.start_pos + 1)
                  else (run.start_pos) in
    let end' = if run.end_pos < (List.length row - 1) && List.nth row (run.end_pos + 1) = Black
                then run.end_pos - 1
                else run.end_pos
    in {run with start_pos = start'; end_pos = end'}
  ) runs


let rule_3_1 row runs =
  List.fold_left (fun (row', runs') (j, run) ->
    let cm =
      let rec find_first_black i =
        if i > run.end_pos then None
        else if List.nth row' i = Black then Some i
        else find_first_black (i + 1)
      in
      find_first_black (if j > 0 then (List.nth runs (j - 1)).end_pos + 1 else 0)
    in
    let cn =
      let rec find_last_black i =
        if i < run.start_pos then None
        else if List.nth row' i = Black then Some i
        else find_last_black (i - 1)
      in
      find_last_black (if j < (List.length runs - 1) then (List.nth runs (j + 1)).start_pos - 1 else List.length row - 1)
    in

    match (cm, cn) with
    | Some m, Some n ->
      let row' =
        List.mapi (fun i cell ->
          if i >= m && i <= n then Black else cell
        ) row'
      in
      let u = run.length - (n - m + 1) in
      (row', runs'@[{run with start_pos = max 0 (m - u);
                    end_pos = min (List.length row - 1) (n + u);}])
      | _ -> (row', runs'@[run])
  ) (row, []) (List.mapi (fun i run -> (i, run)) runs)


let init_runs row row_cls =
  let n = List.length row in
  let k = List.length row_cls in

  let sum_lb cls from_idx to_idx =
    List.fold_left (fun acc (i, clue) -> if (from_idx <= i && i <= to_idx)
      then acc + clue + 1 else acc)
    0 (List.mapi (fun i clue -> (i, clue)) cls)
  in

  List.mapi (fun j clue ->
    let start_pos =
      if j = 0 then 0
      else sum_lb row_cls 0 (j - 1)
    in
    let end_pos =
      if j = k - 1 then n - 1
      else (n - 1) - sum_lb row_cls (j + 1) (k - 1)
    in
    {start_pos; end_pos; length = clue}
  ) row_cls


let apply_rules_r row runs =
  let rec iterate row runs =
    let row' = rule_1_1 row runs in
    let row' = rule_1_2 row' runs in
    let runs' = rule_2_1 row' runs in
    let runs' = rule_2_2 row' runs' in
    let row', runs' = rule_3_1 row' runs' in
    if row' = row && runs' = runs
      then row
      else iterate row' runs'
  in
  iterate row runs


let apply_rules_s stack cls = 
  let runs = (List.map2 init_runs stack cls) in
  let new_stack = (List.map2 apply_rules_r stack runs) in
  new_stack


let apply_rules nono row_cls col_cls =
  let rec iterate nono =
    let nono' = apply_rules_s nono row_cls in
    let nono' = transpose nono' in
    let nono' = apply_rules_s nono' col_cls in
    let nono' = transpose nono' in
  if nono' = nono then nono else (iterate nono') in
  iterate nono