type cell = 
  | Unknown
  | Black
  | White

type row = cell list 
type grid = row list 
                     
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

