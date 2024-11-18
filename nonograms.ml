type cell = Black | White | Unknown
type row = cell list
type grid = row list

(* Helper to initialize grid *)
let initialize_grid rows cols =
  Array.make_matrix rows cols Unknown |> Array.to_list |> List.map Array.to_list

(* Logical rules *)
let apply_logical_rules (grid : grid) : grid =
  (* Implement rules like Rule 1.1, 1.2, etc. Here’s a placeholder function. *)
  grid (* No-op for now; replace with actual rule logic *)

(* Backtracking function *)
let rec backtrack grid row col clues =
  if row >= List.length grid then Some grid (* Base case: all rows processed *)
  else if col >= List.length (List.nth grid row) then
    backtrack grid (row + 1) 0 clues (* Move to next row *)
  else
    match List.nth (List.nth grid row) col with
    | Black | White -> backtrack grid row (col + 1) clues (* Skip fixed cells *)
    | Unknown ->
        (* Try setting this cell to Black *)
        let grid_with_black = List.mapi (fun r row ->
          if r = row then List.mapi (fun c cell -> if c = col then Black else cell) row else row) grid
        in
        (match backtrack grid_with_black row (col + 1) clues with
        | Some solution -> Some solution
        | None ->
            (* Try setting this cell to White *)
            let grid_with_white = List.mapi (fun r row ->
              if r = row then List.mapi (fun c cell -> if c = col then White else cell) row else row) grid
            in
            backtrack grid_with_white row (col + 1) clues)

(* Solver function *)
let solve_nonogram rows cols clues =
  let initial_grid = initialize_grid rows cols in
  let grid_after_rules = apply_logical_rules initial_grid in
  match backtrack grid_after_rules 0 0 clues with
  | Some solution -> solution
  | None -> failwith "No solution found"

