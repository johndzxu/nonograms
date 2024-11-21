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

let update_row row index value =
  List.mapi (fun i cell -> if i = index then value else cell) row

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


let rule_1_3 row runs =
  let find_covering_runs i exclude_run =
    List.filter (fun run ->
        run.start_pos <= i && i <= run.end_pos && run != exclude_run
      ) runs
  in
  List.fold_left (fun row' run ->
      let row' =
        if (List.nth row' run.start_pos) != Black
        then row' else
        if run.start_pos > 0 then
          let covering_runs = find_covering_runs run.start_pos run in
          let all_lengths_one = List.for_all (fun r -> r.length = 1) covering_runs in
          if all_lengths_one then
            update_row row' (run.start_pos - 1) White
          else
            row'
        else
          row'
      in

      if (List.nth row' run.end_pos) != Black
      then row' else
      if run.end_pos < List.length row - 1 then
        let covering_runs = find_covering_runs run.end_pos run in
        let all_lengths_one = List.for_all (fun r -> r.length = 1) covering_runs in
        if all_lengths_one then
          update_row row' (run.end_pos + 1) White
        else
          row'
      else
        row'
    ) row runs



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


(* Connect segments *)
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


let apply_rules_row row runs =
  let rec iterate row runs =
    let row' = rule_1_1 row runs in
    let row' = rule_1_2 row' runs in
    let row' = rule_1_3 row' runs in
    let runs' = rule_2_1 row' runs in
    let runs' = rule_2_2 row' runs' in
    let row', runs' = rule_3_1 row' runs' in
    if row' = row && runs' = runs
    then row
    else iterate row' runs'
  in
  iterate row runs


let apply_rules_rows grid cls = 
  let runs = (List.map2 init_runs grid cls) in
  let grid' = (List.map2 apply_rules_row grid runs) in
  grid'


let apply_rules nono row_cls col_cls =
  let rec iterate nono =
    let nono' = apply_rules_rows nono row_cls in
    let nono' = transpose nono' in
    let nono' = apply_rules_rows nono' col_cls in
    let nono' = transpose nono' in
    if nono' = nono then nono else (iterate nono') in
  iterate nono

(* 
*
*
*
*
*
*
*
*
*
*
*)

(* ------ GIVEN FUNCTIONS ----------*)
let rec verify_row (row: row) (clues: int list) =
  match (clues, row) with 
  | ([], []) -> true
  | ([], _) -> List.for_all (fun x -> (x = White || x = Unknown)) row
  | (_, []) -> false
  | (h::t, c::cells) when c = Black ->  
      let (firstk, rest) = split h row in 
      if List.for_all (fun x -> x = Black || x = Unknown) (firstk) then ( 
        match rest with 
        | [] -> t = []
        | r::rows -> if r = White || r = Unknown then verify_row rows t else false
      ) else false 
  | (h::t, c::cells) when c = White -> verify_row cells clues
  | (h::t, c::cells) ->
      (
        let (firstk, rest) = split h row in 
        if List.for_all (fun x -> (x = Black || x = Unknown)) (firstk) then ( 
          match rest with 
          | [] -> t = []
          | r::rows -> if r  = White || r  = Unknown then verify_row rows t else false
        ) else false
      ) || verify_row cells clues
  
          
let rec ver_cols nono clues = 
  let transposed_nono = transpose nono in 
  let verified_cols = List.map2 (fun clue row -> verify_row row clue) clues transposed_nono in
  List.for_all (fun x -> x) verified_cols
  
let rec ver_rows_and_cols nono row_cls col_cls = 
  let transposed_nono = transpose nono in 
  let verified_cols = List.map2 (fun clue row -> verify_row row clue) col_cls transposed_nono in
  let verified_rows = List.map2 (fun clue row -> verify_row row clue) row_cls nono in 
  List.for_all (fun x -> x) verified_cols && List.for_all (fun x -> x) verified_rows

    (*Return All of the rows of lengths cols that satisfy the clue*)
let rec all_rows' clues cols sc =
  match (clues, cols) with
  | ([], _) -> sc [gen_list cols White]
  | (x::xs, _) when cols<x -> sc []
  | (x::xs, _) when cols=x -> all_rows' xs 0 (fun l -> sc (List.map (fun l -> (gen_list x Black)@l) l))
  | (x::xs, _) -> all_rows' xs (cols-x-1) (fun l1 ->
      all_rows' clues (cols-1) (fun l2 ->
          sc ((List.map (fun l3 -> (gen_list x Black)@[White]@l3) l1)@(List.map (fun l3 -> White::l3) l2))))
                                
let all_rows clues cols = all_rows' clues cols (fun a -> a) 

let generate_children nono row_cls col_cls = 
  let row_id = find_first_row_with_grays nono in
  let gray_combos = binary_permutations (
      List.length (List.filter (fun x -> x = Unknown) (
          List.nth nono row_id
        ))) in 
  let all_children = List.map (fun combo -> replace_grays row_id combo nono) gray_combos in
  List.filter (fun child -> (ver_rows_and_cols child row_cls col_cls)) all_children

    (*Solve with Exceptions*)
let solve row_cls col_cls =
  let width = List.length col_cls in
  
  let rec s_row row_cls nono = 
    match row_cls with 
    | [] -> let nono = List.rev nono in 
        if ver_cols nono col_cls then nono else raise Fail
    | cl::cls -> s_stack row_cls nono (all_rows cl width) 
  and s_stack row_cls nono stack = 
    match (stack, row_cls) with 
    | ([], _) -> raise Fail
    | (x::xs, cl::oth_cls) ->
        (try s_row oth_cls (x::nono) 
         with Fail -> s_stack row_cls nono xs)
    | _-> raise Fail
            
  in s_row row_cls []
  
(* Solve with fail continuation *)
let solve_cont row_cls col_cls =
  let width = List.length col_cls in
  
  let rec s_row row_cls nono fc =
    match row_cls with 
    | [] -> let nono = List.rev nono in
        if ver_cols nono col_cls then nono else fc ()
    | cl::cls -> s_stack row_cls nono (all_rows cl width) fc
      
  and s_stack row_cls nono stack fc =
    match (stack, row_cls) with
    | ([], _) -> fc ()
    | (x::xs, cl::oth_cls) -> s_row oth_cls (x::nono) 
                                (fun () -> s_stack row_cls nono xs fc)
    | _ -> fc ()
      
  in s_row row_cls [] (fun () -> raise Fail)
  
(*Find ALL solutions with success continuation*)
let solve_all row_cls col_cls =
  let width = List.length col_cls in
  
  let rec s_row row_cls nono (sc: grid list -> 'a) : 'a = 
    match row_cls with 
    | [] -> let nono = List.rev nono in
        if ver_cols nono col_cls then sc [nono] else sc []
    | cl::cls -> s_stack row_cls nono (all_rows cl width) sc
      
  and s_stack row_cls nono stack (sc: grid list -> 'a) : 'a = 
    match (stack, row_cls) with 
    | ([], _) -> sc []
    | (x::xs, cl::oth_cls) -> s_row oth_cls (x::nono) 
                                (fun (l : grid list) -> s_stack row_cls nono xs 
                                    (fun (l2: grid list) -> sc (l@l2)))
    | _ -> sc []
  
  in s_row row_cls [] (fun a -> a) 
    
(* Solve with Exceptions and Backtracking *)
let solve_backtracking row_cls col_cls =  
  let rec s_row row_cls col_cls nono = 
    let new_nono = apply_rules nono row_cls col_cls in
    if ver_cols new_nono col_cls then 
      try 
        try_children row_cls col_cls (generate_children new_nono row_cls col_cls)
      with NoGrays -> new_nono
    else raise Fail
  and try_children row_cls col_cls children = 
    match children with 
    | [] -> raise Fail
    | x::xs -> (
        try s_row row_cls col_cls x 
        with Fail -> try_children row_cls col_cls xs
      ) 
  in
  s_row row_cls col_cls (init_grid (List.length row_cls) (List.length col_cls))

