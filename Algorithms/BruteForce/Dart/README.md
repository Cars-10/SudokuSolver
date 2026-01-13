# Dart Sudoku Solver

A brute-force Sudoku solver implemented in Dart, matching the C reference algorithm exactly.

## Algorithm

The solver uses a simple brute-force backtracking algorithm:
- Row-major order scanning (top-to-bottom, left-to-right)
- Try values 1-9 in ascending order
- Count EVERY placement attempt (before validity check)
- Backtrack when no valid value found

## Execution

```bash
# JIT mode (faster startup for small tasks)
dart run Sudoku.dart path/to/puzzle.matrix

# Compile to native (faster execution for large workloads)
dart compile exe Sudoku.dart -o Sudoku
./Sudoku path/to/puzzle.matrix
```

## Validation Status

| Matrix | Expected | Actual  | Status |
|--------|----------|---------|--------|
| 1      | 656      | 656     | ✓ Pass |
| 2      | 439,269  | 439,269 | ✓ Pass |
| 3      | 98,847   | 98,847  | ✓ Pass |
| 4      | 9,085    | 9,085   | ✓ Pass |
| 5      | 445,778  | 445,778 | ✓ Pass |

## Performance

- Dart VM provides JIT compilation for development
- Can compile to native code for optimal performance
- Part of Flutter ecosystem, cross-platform capable
