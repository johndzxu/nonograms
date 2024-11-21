exception NotImplemented

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

let update_row (row: cell list) (index: int) (value: cell) =
  List.mapi (fun i cell -> if i = index then value else cell) row

(* Intersection of all possibilities *)
let rule_1_1 (row: row) (runs: run list): row =
  raise NotImplemented

(* Does not belong to any run *)
let rule_1_2 (row: row) (runs: run list): row =
  raise NotImplemented

(* Covering run are all one *)
let rule_1_3 (row: cell list) (runs: run list): cell list =
  raise NotImplemented

(* Eliminate overlaps *)
let rule_2_1 (row: cell list) (runs: run list): run list =
  raise NotImplemented

(* White cell between runs *)
let rule_2_2 (row: cell list) (runs: run list): run list =
  raise NotImplemented

(* Connect segments *)
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