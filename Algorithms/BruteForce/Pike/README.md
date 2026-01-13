# Pike Sudoku Solver

Implementation of the Sudoku solver in Pike, a dynamic programming language with C-like syntax.

## Language Notes

Pike is a dynamic programming language with a syntax similar to C. It is easy to learn for C/C++ programmers and has a powerful module system.

## Implementation Details

- **Algorithm**: Exact match of the C brute-force backtracking algorithm.
- **Iteration Counting**: Matches C algorithm fingerprint (increment before validity check).
- **Data Structures**: Uses `array(array(int))` for the puzzle grid.

## Performance

Pike is interpreted but often faster than Python or Ruby for this type of workload.

## Validation

Matches iteration counts for all 5 reference matrices:
- Matrix 1: 656
- Matrix 2: 439,269
- Matrix 3: 98,847
- Matrix 4: 9,085
- Matrix 5: 445,778
