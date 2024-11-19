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

let cell_eq c1 c2 = c1 = c2
let row_eq r1 r2 = List.for_all2 cell_eq r1 r2

let row = testable pp_row row_eq

let test_row = [Unknown; Unknown; Unknown; Unknown; Unknown; Unknown; Unknown; Unknown; Unknown; Unknown]

let test_rule_1_1 () =
  let test_run_ranges = [{ start_pos = 2; end_pos = 6; length = 3 }] in
  let expected_row = [Unknown; Unknown; Unknown; Unknown; Black; Unknown; Unknown; Unknown; Unknown; Unknown] in

  let result_row = rule_1_1 test_run_ranges test_row in
  Alcotest.(check row) "" expected_row result_row

let test_rule_1_2 () =
  let test_run_ranges = [{ start_pos = 2; end_pos = 6; length = 3 }] in
  let expected_row = [White; White; Unknown; Unknown; Unknown; Unknown; Unknown; White; White; White] in

  let result_row = rule_1_2 test_run_ranges test_row in
  Alcotest.(check row) "" expected_row result_row

  let test_rule_1_3 () =
    let test_row = [Unknown; Unknown; Unknown; Black; Unknown; Unknown; Unknown; Unknown; Unknown; Unknown] in
    let test_run_ranges = [{ start_pos = 1; end_pos = 3; length = 1 }; { start_pos = 3; end_pos = 8; length = 3 }] in
    let expected_row = [Unknown; Unknown; White; Black; Unknown; Unknown; Unknown; Unknown; Unknown; Unknown] in
  
    let result_row = rule_1_3 test_run_ranges test_row in
    Alcotest.(check row) "" expected_row result_row

  let test_rule_1_4 () =
    let test_row = [Unknown; Black; Unknown; Black; Black; Unknown; Unknown; Unknown; Unknown; Unknown] in
    let test_run_ranges = [{ start_pos = 0; end_pos = 2; length = 2 }; { start_pos = 1; end_pos = 5; length = 3 }] in
    let expected_row = [Unknown; Black; White; Black; Black; Unknown; Unknown; Unknown; Unknown; Unknown] in
  
    let result_row = rule_1_4 test_run_ranges test_row in
    Alcotest.(check row) "" expected_row result_row

  let test_rule_1_5_part1 () =
    let test_row = [Unknown; Unknown; Unknown; White; Unknown; Black; Unknown; Unknown; Unknown; Unknown] in
    let test_run_ranges = [{ start_pos = 0; end_pos = 6; length = 3 }; { start_pos = 3; end_pos = 9; length = 4 }] in
    let expected_row = [Unknown; Unknown; Unknown; White; Unknown; Black; Black; Unknown; Unknown; Unknown] in

    let result_row = rule_1_5_part1 test_run_ranges test_row in
    Alcotest.(check row) "" expected_row result_row

    

    let test_rule_1_5_part2 () =
      let test_row = [Unknown; Unknown; Unknown; Black; Black; Black; Unknown; Unknown; Unknown; Unknown] in
      let test_run_ranges = [{ start_pos = 0; end_pos = 6; length = 3 }; { start_pos = 3; end_pos = 9; length = 3 }] in
      let expected_row = [Unknown; Unknown; White; Black; Black; Black; White; Unknown; Unknown; Unknown] in

      let result_row = rule_1_5_part2 test_run_ranges test_row in
      Alcotest.(check row) "" expected_row result_row

    (* List of tests *)
let () =
  Alcotest.run ~verbose:true "Rules Tests" [
    "Rule 1.1", [ "", `Quick, test_rule_1_1 ];
    "Rule 1.2", [ "", `Quick, test_rule_1_2 ];
    "Rule 1.3", [ "", `Quick, test_rule_1_3 ];
    "Rule 1.4", [ "", `Quick, test_rule_1_4 ];
    "Rule 1.5 1", [ "", `Quick, test_rule_1_5_part1 ];
    "Rule 1.5 2", [ "", `Quick, test_rule_1_5_part2 ];


  ]


