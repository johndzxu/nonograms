type cell = Black | White | Unknown
type row = cell list
type nonogram = row list
exception NotImplemented
exception Fail 
    
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
  let rec solve' nono row_cls col_cls = 
    if ver_grid nono col_cls then nono else
      let new_rows = all_rows row_cls (List.length (List.hd nono)) in
      let new_nono = List.map (fun row -> row::nono) new_rows in
      let rec solve'' nono = 
        match nono with
        | [] -> raise Fail
        | n::ns -> if ver_grid n col_cls then n else solve'' ns in
      solve'' (List.map (fun n -> solve' n row_cls col_cls) new_nono) in
  solve' [] row_cls col_cls  
;;
                     

(* let rec s_row row_cls col_cls nono = 
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

;; *)


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
    (* children = insert_rows child_rows nono row_id *)
  ;;
  
