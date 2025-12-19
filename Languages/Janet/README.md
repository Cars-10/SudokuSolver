# Janet Sudoku Solver

Implementation of the Sudoku solver in Janet, a modern, imperative and functional Lisp.

## Language Notes

Janet is a dynamic language with a focus on simplicity, embeddability, and performance. It has a rich standard library and supports both functional and imperative programming styles.

## Implementation Details

- **Algorithm**: Exact match of the C brute-force backtracking algorithm.
- **Iteration Counting**: Matches C algorithm fingerprint (increment before validity check).
- **Data Structures**: Uses mutable arrays (`@[]`) for the puzzle grid.

## Performance

Janet is a bytecode-interpreted language, generally faster than pure Python but slower than JIT-compiled languages.

## Validation

Matches iteration counts for all 5 reference matrices:
- Matrix 1: 656
- Matrix 2: 439,269
- Matrix 3: 98,847
- Matrix 4: 9,085
- Matrix 5: 445,778
