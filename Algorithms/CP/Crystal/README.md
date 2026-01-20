# Crystal CP Sudoku Solver

This is a Constraint Propagation (CP) implementation of a Sudoku solver in Crystal.

## Algorithm

It follows the "Red Pill" benchmark standard for CP solvers:
1.  **State:** Uses 9x9 grid of bitsets (UInt16) to track candidates.
2.  **Propagation:** 
    *   **Singleton Elimination:** If a cell has only 1 candidate, assign it.
    *   **Hidden Singles:** If a digit appears only once in a row/col/box, assign it.
3.  **Search:** Recursive backtracking with Minimum Remaining Values (MRV) heuristic.

## Usage

```bash
crystal build --release -o cp_solver cp.cr
./cp_solver <matrix_file>
```
