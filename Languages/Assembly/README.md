# Assembly Sudoku Solver

Implementation of the Sudoku solver in x86_64 Assembly using NASM.

## Language Notes

This implementation uses direct system calls for I/O and standard C library functions for string manipulation and conversion. It's a low-level implementation that requires manual memory management and register allocation.

## Implementation Details

- **Algorithm**: Exact match of the C brute-force backtracking algorithm.
- **Iteration Counting**: Matches C algorithm fingerprint (increment before validity check).
- **Data Structures**: Uses a flat byte array for the puzzle grid.

## Performance

Assembly is expected to have performance very close to C, as it is the lowest level of abstraction.

## Validation

Matches iteration counts for all 5 reference matrices:
- Matrix 1: 656
- Matrix 2: 439,269
- Matrix 3: 98,847
- Matrix 4: 9,085
- Matrix 5: 445,778
