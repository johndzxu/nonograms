let grid = init_grid 4 4
let row_clues = [[1]; [1;2]; [2;1]; [1;1]] ;;
let col_clues = [[4]; [1]; [1]; [3]] ;;
apply_logical_rules grid row_clues col_clues ;;