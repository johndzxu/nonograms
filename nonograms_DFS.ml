type cell = Black | White | Unknown
type row = cell list
type nonogram = row list
exception NotImplemented
exception Fail 
    
  
    (* THE FOLLOWING CODE IS USED ELSEWHERE!!!! BE CAREFUL NOT TO REPEAT 
   THESE FUNCTIONS IN FINAL FILE!!!!*)
    (*START*)
let gen_list n e =
  let rec gen' n cont = 
    match n with
    | 0 -> cont []
    | _ -> gen' (n-1) (fun l -> cont (e::l)) in gen' n (fun a -> a) 
    
let transpose nono = 
  let len = List.length (List.hd nono) in
  let cols = List.fold_left (fun cols row -> List.map2 (fun col square -> (square::col)) cols row) (gen_list len []) nono
  in List.map (fun l -> List.rev l) cols 

let split index grid =
  let rec aux i acc rest =
    match rest with
    | [] -> (List.rev acc, [])
    | h :: t ->
        if i = 0 then (List.rev acc, rest)
        else aux (i - 1) (h :: acc) t
  in
  aux index [] grid 

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
    (*END*)

    (*---------------------------------------------*)
  
let rec all_rows' clues cols sc =
  match (clues, cols) with
  | ([], _) -> sc [gen_list cols White]
  | (x::xs, _) when cols<x -> sc []
  | (x::xs, _) when cols=x -> all_rows' xs 0 (fun l -> sc (List.map (fun l -> (gen_list x Black)@l) l))
  | (x::xs, _) -> all_rows' xs (cols-x-1) (fun l1 ->
      all_rows' clues (cols-1) (fun l2 ->
          sc ((List.map (fun l3 -> (gen_list x Black)@[White]@l3) l1)@(List.map (fun l3 -> White::l3) l2))))
                                
let all_rows clues cols = all_rows' clues cols (fun a -> a) 
  
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
  
  let rec s_row row_cls nono (sc: nonogram list -> 'a) : 'a = 
    match row_cls with 
    | [] -> let nono = List.rev nono in
        if ver_cols nono col_cls then sc [nono] else sc []
    | cl::cls -> s_stack row_cls nono (all_rows cl width) sc
      
  and s_stack row_cls nono stack (sc: nonogram list -> 'a) : 'a = 
    match (stack, row_cls) with 
    | ([], _) -> sc []
    | (x::xs, cl::oth_cls) -> s_row oth_cls (x::nono) 
                                (fun (l : nonogram list) -> s_stack row_cls nono xs 
                                    (fun (l2: nonogram list) -> sc (l@l2)))
    | _ -> sc []
  
  in s_row row_cls [] (fun a -> a) 
  
  
                     
    