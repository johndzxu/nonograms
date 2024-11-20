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
  
(*Solve with Exceptions*)
let solve row_cls col_cls =
  let width = List.length col_cls in
  
  let rec s_row row_cls nono = 
    match row_cls with 
    | [] -> let nono = List.rev nono in 
        if ver_grid nono col_cls then nono else raise Fail
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
        if ver_grid nono col_cls then nono else fc ()
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
  
  let rec s_row row_cls nono sc depth = 
    match row_cls with 
    | [] -> let nono = List.rev nono in
        if ver_grid nono col_cls then sc [nono] else sc []
    | cl::cls -> s_stack row_cls nono (all_rows cl width) sc (depth+1)
      
  and s_stack row_cls nono stack sc depth = 
    match (stack, row_cls) with 
    | ([], _) -> sc []
    | (x::xs, cl::oth_cls) -> s_row oth_cls (x::nono) 
                                (fun l -> s_stack row_cls nono xs (fun l2 -> sc (l@l2)) depth) depth
    | _ -> sc []
  
  in s_row row_cls [] (fun a -> a) 0

solve_all [[3]; [2; 1]; [3; 2]; [2; 2]; [6]; [1; 5]; [6]; [1]; [2]]
[[1; 2]; [3; 1]; [1; 5]; [7; 1]; [5]; [3]; [4]; [3]];;