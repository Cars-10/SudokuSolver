# Phase 2, Plan 2: DLX Algorithm Implementation

## Objective
Implement the core DLX search algorithm and the logic to map a Sudoku puzzle to an exact cover problem. This plan builds upon the scaffolding created in Plan 1 and will result in a fully functional, benchmarkable DLX-based Sudoku solver.

## Execution Context
- **Phase**: 2 (Algorithm - Dancing Links)
- **Goal**: Implement and integrate the DLX solver.
- **Depends On**: Successful completion of `phase-2-plan-1-scaffolding-PLAN.md`.

## Tasks

### 1. Implement DLX Search Algorithm
**Goal**: Implement the `search` function in `dlx_core.c` based on Knuth's Algorithm X.
- **Action**: Modify `dlx_core.c`.
- **Logic**:
  1. If the matrix is empty (root's right pointer is the root itself), a solution is found. Return success.
  2. Otherwise, choose a column `c` (e.g., the one with the fewest nodes).
  3. Cover column `c`.
  4. For each row `r` in `c`'s list:
     - Add `r` to the partial solution.
     - For each column `j` in `r`'s list, cover column `j`.
     - Recursively call `search`. If it returns success, propagate success upwards.
     - If not, backtrack: for each column `j` in `r`'s list, uncover column `j`.
  5. Uncover column `c`.
  6. If no solution was found, return failure.
- **Reference**: Use the web search results for "Algorithm X C implementation" as a guide.

### 2. Implement Sudoku to Exact Cover Mapping
**Goal**: Create the logic in `dlx_sudoku.c` to build the DLX matrix from a Sudoku puzzle.
- **Action**: Modify `dlx_sudoku.c`.
- **Logic**:
  - The matrix will have 324 columns representing the four Sudoku constraints:
    - 81 columns for `(row, col)` must be filled.
    - 81 columns for `(row, num)` must be unique.
    - 81 columns for `(col, num)` must be unique.
    - 81 columns for `(box, num)` must be unique.
  - Create a function `build_dlx_matrix(int puzzle[9][9])`.
  - For each cell `(r, c)` and each possible number `n` (1-9):
    - Create a row in the DLX matrix with `1`s in the four corresponding constraint columns.
  - If a cell `(r, c)` in the input puzzle already has a number `n`, these rows must be handled specially (pre-selected).

### 3. Implement Puzzle I/O and Main Loop
**Goal**: Update `dlx_sudoku.c` to read puzzle files and print solutions in the standard format.
- **Action**: Modify `dlx_sudoku.c`.
- **Details**:
  - Adapt the `readMatrixFile` and `printPuzzle` functions from the original `Languages/C/Sudoku.c`.
  - The `main` function should loop through input matrix files, call `build_dlx_matrix`, call `dlx_search`, and print the solution and iteration count.
  - The "iteration count" for DLX will be the number of times `search` is called.

### 4. Update Build Script for Execution
**Goal**: Update `runMe.sh` to execute the solver and capture metrics.
- **Action**: Modify `Algorithms/DLX/C/runMe.sh`.
- **Details**:
  - Add the `run_solver` function, mirroring the one in `Languages/common.sh`.
  - The `main` function should now call `compile` and then pass its arguments to `run_solver`.
  - Ensure it correctly calls the `dlx_solver` binary and passes the matrix files.

## Verification
- Run `./runMeGlobal.sh DLX/C 1`.
- **Success Criteria**:
  - The script must successfully compile and run the `dlx_solver`.
  - The solver must correctly solve `1.matrix`.
  - The output format (puzzle, iteration count, timing) must match the project's standard.
  - The iteration count should be a stable, repeatable number for a given puzzle.

## Output
- A fully functional `dlx_solver` in `Algorithms/DLX/C/`.
- Updated `dlx_core.c`, `dlx_sudoku.c`, and `runMe.sh`.
- A `metrics.json` file generated from a successful run.
