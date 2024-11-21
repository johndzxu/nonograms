# What are Nonograms
A nonogram is a grid-based puzzle where cells must be filled or left empty based on clues provided for each row and column. Each clue specifies the length of a black run in the row or column, and the goal is to fill in the grid in a way that satisfies all the clues.

# A Nonogram Solver
We implemented a nonogram solver with two different algorithms in OCaml.

### Depth First Search
First we implemented a depth first search (DFS) algorithm which checks all possible patterns to brute force the solution. It traverses a tree that represents all possible combinations given the row clues, then goes through all of these cases and rejects the solutions that do not satisfy the column clues. This solution is very slow.


### Chronological Backtracking
We then implemented a significantly more efficient algorithm described in [Yu et al. (2009c)](https://link.springer.com/article/10.1007/s10489-009-0200-0). This algorithm consists of two main components: logical rules and a chronological backtracking (CB) algorithm.

At the beginning, we apply a set of eleven heuristic rules to the empty board, which partially fills in a grid based on the given clues. Each of the eleven rules is applied iteratively to each row and column until the rules no longer fill in cells.

Then, we use CB to fill in a possible pattern for the unknown cells in a row and use the column clues to verify it's validity. If it is a valid pattern, we apply all logic rules again before we use CB to fill in a possible pattern for the next row. We apply this algorithm recursively until all solutions are found.

# Citations
Yu, C., Lee, H., & Chen, L. (2009c). An efficient algorithm for solving nonograms. Applied Intelligence, 35(1), 18–31. https://doi.org/10.1007/s10489-009-0200-0
