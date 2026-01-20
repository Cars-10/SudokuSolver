# Crystal DLX Sudoku Solver

This is a Dancing Links (DLX) implementation of a Sudoku solver in Crystal.

## Algorithm

It uses the Algorithm X implemented via Dancing Links (DLX) technique.
1.  **Exact Cover:** Sudoku is modeled as an exact cover problem.
2.  **Matrix:** 729 rows (9x9x9 candidates) x 324 columns (constraints).
3.  **Search:** Knuth's Algorithm X on the quadruply linked list structure.

## Usage

```bash
crystal build --release -o dlx_solver dlx.cr
./dlx_solver <matrix_file>
```
