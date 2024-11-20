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
    let runs' = runs in
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