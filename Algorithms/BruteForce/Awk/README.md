# Awk Sudoku Solver

Brute-force Sudoku solver implemented in GNU Awk (gawk).

## Implementation Notes

- Uses associative array for 2D puzzle grid: `puzzle[row, col]`
- 0-based indexing matching C reference
- Row-major search order for empty cells
- Iteration counter incremented before validity check (matching C algorithm exactly)

## Validation Status

| Matrix | Expected | Actual | Status |
|--------|----------|--------|--------|
| 1      | 656      | 656    | PASS   |
| 2      | 439,269  | 439,269| PASS   |
| 3      | 98,847   | 98,847 | PASS   |
| 4      | 9,085    | 9,085  | PASS   |
| 5      | 445,778  | 445,778| PASS   |

## Running

```bash
# Single matrix
gawk -f Sudoku.awk ../../Matrices/1.matrix

# Benchmark all matrices
./runMe.sh ../../Matrices/*.matrix
```

## Toolchain

- GNU Awk 5.2.1
- Interpreter (no compilation)

## Performance Characteristics

- Pattern-action paradigm well-suited for text processing
- Associative arrays provide flexible grid access
- Interpreted execution (slower than compiled languages)
- Efficient for this scale of problem
