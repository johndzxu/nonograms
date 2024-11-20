open Rules
open Alcotest

let pp_cell fmt = function
  | Unknown -> Format.fprintf fmt "Unknown"
  | Black -> Format.fprintf fmt "Black"
  | White -> Format.fprintf fmt "White"

let pp_row fmt row =
  Format.fprintf fmt "[ ";
  List.iter (fun cell -> Format.fprintf fmt "%a " pp_cell cell) row;
  Format.fprintf fmt "]"

let pp_run_range fmt run_range =
  Format.fprintf fmt "{ start_pos = %d; end_pos = %d; length = %d }"
    run_range.start_pos run_range.end_pos run_range.length

let test_rule_3_1 () =
  let test_row = [Unknown; Unknown; Unknown; Black; Black; Unknown; Black; Unknown; Unknown; Unknown] in
  let test_run_ranges = [{ start_pos = 0; end_pos = 2; length = 2 }; { start_pos = 1; end_pos = 7; length = 5 }] in
  let expected_row = [Unknown; Unknown; Unknown; Black; Black; Black; Black; Unknown; Unknown; Unknown] in
  let expected_run_ranges = [{ start_pos = 0; end_pos = 2; length = 2 }; { start_pos = 2; end_pos = 7; length = 5 }] in
  
  let result_run_ranges, result_row = rule_3_1 test_run_ranges test_row in

  Alcotest.(check (list (of_pp pp_cell))) "Rule 3.1 updates row" expected_row result_row;
  Alcotest.(check (list (of_pp pp_run_range))) "Rule 3.1 updates ranges" expected_run_ranges result_run_ranges
  
let test_rule_3_2 () =
  let test_row = [Unknown; Unknown; Unknown; White; Unknown; Unknown; White; Unknown; Unknown; Unknown; Unknown; Unknown] in
  let test_run_ranges = [{ start_pos = 0; end_pos = 2; length = 2 }; { start_pos = 2; end_pos = 11; length = 4 }] in
  let expected_row = [Unknown; Unknown; Unknown; White; Black; Black; White; Unknown; Unknown; Unknown; Unknown; Unknown] in
  let expected_run_ranges = [{ start_pos = 0; end_pos = 2; length = 2 }; { start_pos = 7; end_pos = 11; length = 4 }] in

  let result_run_ranges, result_row = rule_3_2 test_run_ranges test_row in

  Alcotest.(check (list (of_pp pp_cell))) "Rule 3.2 updates row" expected_row result_row;
  Alcotest.(check (list (of_pp pp_run_range))) "Rule 3.2 updates ranges" expected_run_ranges result_run_ranges



let () =
Alcotest.run ~verbose:true "Rule 3 Tests" [
  "Rule 3.1", [ "Fill scattered black cells and update ranges", `Quick, test_rule_3_1 ];
  "Rule 3.2", ["", `Quick, test_rule_3_2];
]
