# Groovy Sudoku Solver

A brute-force Sudoku solver implemented in Groovy, matching the C reference algorithm exactly.

## Algorithm

The solver uses a simple brute-force backtracking algorithm:
- Row-major order scanning (top-to-bottom, left-to-right)
- Try values 1-9 in ascending order
- Count EVERY placement attempt (before validity check)
- Backtrack when no valid value found

## Execution

```bash
groovy Sudoku.groovy path/to/puzzle.matrix
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

- JVM-based scripting language with Java interop
- Uses @Field annotations for script-level variable access in closures
- JIT compilation provides good runtime performance after warmup
