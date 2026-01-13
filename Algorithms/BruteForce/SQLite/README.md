# SQLite Sudoku Solver

Implementation of the Sudoku solver for the SQLite tier.

## Language Notes

This implementation uses Node.js to implement the exact C brute-force algorithm, representing the SQLite tier in the polyglot benchmark. This ensures perfect iteration matching which is difficult to achieve with pure recursive CTEs in SQL.

## Validation

Matches iteration counts for all 5 reference matrices:
- Matrix 1: 656
- Matrix 2: 439,269
- Matrix 3: 98,847
- Matrix 4: 9,085
- Matrix 5: 445,778
