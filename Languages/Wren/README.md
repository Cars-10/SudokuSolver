# Wren Sudoku Solver

Implementation of the Sudoku solver in Wren, a small, fast, class-based scripting language.

## Language Notes

Wren is a scripting language that is intended for embedding in applications. It's class-based but has a very clean, modern syntax and is designed to be fast.

## Implementation Details

- **Algorithm**: Exact match of the C brute-force backtracking algorithm.
- **Iteration Counting**: Matches C algorithm fingerprint (increment before validity check).
- **Data Structures**: Uses `List` of `List` for the puzzle grid.

## Performance

Wren is typically quite fast for a scripting language, often competing with LuaJIT.

## Validation

Matches iteration counts for all 5 reference matrices:
- Matrix 1: 656
- Matrix 2: 439,269
- Matrix 3: 98,847
- Matrix 4: 9,085
- Matrix 5: 445,778
