# Assembly Sudoku Solver

Implementation of the Sudoku solver in:
- x86_64 Assembly using NASM
- ARM64 Assembly (AArch64) using GCC

## Language Notes

This implementation uses standard C library functions for I/O and logical operations. It supports both x86_64 and ARM64 architectures.

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
