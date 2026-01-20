# Constraint Propagation Sudoku Solver in AppleScript

This implementation uses simulated bitsets (integers) and the Minimum Remaining Values (MRV) heuristic.

## Algorithm

1.  **State:** 9x9 grid of integer bitsets (bits 1-9 represent candidates).
2.  **Propagation:** Singleton elimination and Hidden Singles (simulated).
3.  **Search:** Recursive backtracking with MRV.

## Usage

```bash
osascript cp.applescript <matrix_file>
```
