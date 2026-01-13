# PostScript Sudoku Solver

Implementation of the Sudoku solver in PostScript using Ghostscript (`gs`).

## Language Notes

PostScript is a stack-based, concatenative programming language. While primarily known as a page description language for printing, it is a Turing-complete language with support for dictionaries, arrays, and recursion.

This implementation uses:
- **Ghostscript (`gs`)**: Run with `-dNODISPLAY` for pure interpretation.
- **Recursive Backtracking**: Implemented using local dictionaries (`dict begin ... end`) for recursion safety.
- **`store` operator**: Used to update the global iteration counter from within nested scopes.

## Validation

Matches iteration counts for all 5 reference matrices:
- Matrix 1: 656
- Matrix 2: 439,269
- Matrix 3: 98,847
- Matrix 4: 9,085
- Matrix 5: 445,778
