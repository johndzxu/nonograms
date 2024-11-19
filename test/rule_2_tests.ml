open Rules
open Alcotest

(* Pretty printer for run ranges *)
let pp_run_range fmt run_range =
  Format.fprintf fmt "{ start_pos = %d; end_pos = %d; length = %d }"
    run_range.start_pos run_range.end_pos run_range.length

(* Equality function for run ranges *)
let run_range_eq r1 r2 =
  r1.start_pos = r2.start_pos && r1.end_pos = r2.end_pos && r1.length = r2.length

(* Alcotest testable for run ranges *)
let run_range = Alcotest.testable pp_run_range run_range_eq

let test_rule_2_1 () =
  (* Input run ranges and row *)
  let test_row = [Unknown; Unknown; Unknown; Unknown; Unknown; Unknown; Unknown; Unknown; Unknown; Unknown] in
  let test_run_ranges = [{ start_pos = 2; end_pos = 6; length = 3 }] in
  (* Expected adjusted ranges *)
  let expected_run_ranges = [{ start_pos = 3; end_pos = 5; length = 3 }] in
  (* Apply Rule 2.1 *)
  let result_run_ranges = rule_2_1 test_run_ranges test_row in
  (* Check result *)
  Alcotest.(check (list (of_pp pp_run_range))) "Rule 2.1 adjusts ranges correctly" expected_run_ranges result_run_ranges


let test_rule_2_2 () =
  (* Input row and run ranges *)
  let test_row = [Unknown; Black; Unknown; Unknown; Unknown; Unknown; Unknown; Unknown; Unknown; Unknown] in
  let test_run_ranges = [{ start_pos = 0; end_pos = 4; length = 3 }] in
  (* Expected adjusted ranges *)
  let expected_run_ranges = [{ start_pos = 1; end_pos = 3; length = 3 }] in
  (* Apply Rule 2.2 *)
  let result_run_ranges = rule_2_2 test_run_ranges test_row in
  (* Check result *)
  Alcotest.(check (list (of_pp pp_run_range))) "Rule 2.2 adjusts ranges based on adjacent cells" expected_run_ranges result_run_ranges
  
let test_rule_2_3 () =
  (* Input row and run ranges *)
  let test_row = [Unknown; Unknown; Black; Unknown; Unknown; Unknown; Black; Unknown; Unknown; Unknown] in
  let test_run_ranges = [{ start_pos = 0; end_pos = 4; length = 3 }; { start_pos = 5; end_pos = 9; length = 3 }] in
  (* Expected adjusted ranges *)
  let expected_run_ranges = [{ start_pos = 2; end_pos = 4; length = 3 }; { start_pos = 6; end_pos = 8; length = 3 }] in
  (* Apply Rule 2.3 *)
  let result_run_ranges = rule_2_3 test_run_ranges test_row in
  (* Check result *)
  Alcotest.(check (list (of_pp pp_run_range))) "Rule 2.3 refines ranges for overlapping segments" expected_run_ranges result_run_ranges
  

let () =
Alcotest.run "Nonogram Solver Tests" [
  "Rule 2.1", [ "Adjusts ranges based on sequence constraints", `Quick, test_rule_2_1 ];
  "Rule 2.2", [ "Adjusts ranges based on adjacent cells", `Quick, test_rule_2_2 ];
  "Rule 2.3", [ "Refines ranges for overlapping segments", `Quick, test_rule_2_3 ];
]
