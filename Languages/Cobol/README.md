# COBOL Sudoku Solver

Brute-force Sudoku solver implemented in GnuCOBOL.

## Status

**INCOMPLETE** - The solver compiles but has performance issues causing timeouts on complex puzzles.

## Known Issues

1. Simple puzzles (< 10 empty cells) solve correctly
2. Complex puzzles like Matrix 1 (45 empty cells) timeout after 2+ minutes
3. The algorithm is correct (validated on simple test cases) but appears to have runtime performance issues specific to GnuCOBOL

## Algorithm

The solver uses the standard brute-force backtracking algorithm:
1. Pre-compute list of all empty cells (row-major order)
2. Try values 1-9 for each empty cell
3. Validate against row, column, and 3x3 box constraints
4. Backtrack when no valid values remain

## Building

```bash
cobc -x -free -O2 -o Sudoku sudoku.cob
```

## Running

```bash
./Sudoku <matrix_file>
```

## Validation Status

| Matrix | Expected Iterations | Status   |
|--------|---------------------|----------|
| 1      | 656                 | TIMEOUT  |
| 2      | 439,269             | TIMEOUT  |
| 3      | 98,847              | TIMEOUT  |
| 4      | 9,085               | TIMEOUT  |
| 5      | 445,778             | TIMEOUT  |

## Notes

- GnuCOBOL 3.1.2.0
- Free-format source code (-free flag)
- Compiled with -O2 optimization
- COBOL's interpreted comparison operations may be significantly slower than compiled languages
- The backtracking algorithm requires millions of comparisons, amplifying any per-operation overhead
