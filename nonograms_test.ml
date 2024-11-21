let row_clues = [[1]; [1;2]; [2;1]; [1;1]]
let col_clues = [[4]; [1]; [1]; [3]]
solve_backtracking row_clues col_clues

let row_clues = [[1];[1;1];[1;1];[2];[1;1];[3;1]]
let col_clues = [[1];[1;1];[1;1];[1;1];[1;1];[1;1];[2]]
solve_backtracking row_clues col_clues ;;

let row_clues = [[1];[3];[2;2];[3];[1]]
let col_clues = [[1];[3];[2;2];[3];[1]]
solve_backtracking row_clues col_clues ;;

let row_clues = [[1];[1];[3];[2]]
let col_clues = [[1];[2];[1];[2];[1]]
solve_backtracking row_clues col_clues ;;
