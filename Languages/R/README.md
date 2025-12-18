# R Sudoku Solver

Brute-force backtracking Sudoku solver implemented in R, matching the C reference algorithm exactly.

## Algorithm

- **Search order**: Row-major (top-to-bottom, left-to-right)
- **Value order**: 1-9 ascending
- **Counting**: Every placement attempt is counted (algorithm fingerprint)

## Implementation Notes

### 1-Based Indexing

R uses 1-based indexing (unlike C's 0-based). Key adjustments:

```r
# Cell scanning: 1:9 instead of 0:8
for (r in 1:9) {
    for (c in 1:9) {
        # ...
    }
}

# Box calculation (1-indexed adjustment)
# C: box_row = (row / 3) * 3 where row is 0-8
# R: box_row = ((row - 1) %/% 3) * 3 + 1 where row is 1-9
box_row <- ((row - 1) %/% 3) * 3 + 1
box_col <- ((col - 1) %/% 3) * 3 + 1
```

### Global Variable Assignment

R uses `<<-` for global assignment in nested functions:

```r
count <<- count + 1L  # Global assignment in recursive function
puzzle[row, col] <<- val  # Modify global matrix
```

### Performance Characteristics

R is an interpreted statistical computing language, not optimized for:
- Deep recursion
- Tight loops
- Integer-heavy computation

**Expected behavior:**
- Matrix 1 (656 iterations): Completes quickly
- Larger matrices (100K+ iterations): May timeout

## Validation Status

| Matrix | Expected Iterations | Actual | Status |
|--------|---------------------|--------|--------|
| 1      | 656                 | 656    | PASS   |
| 2      | 439,269             | 439,269| PASS   |
| 3      | 98,847              | 98,847 | PASS   |
| 4      | 9,085               | 9,085  | PASS   |
| 5      | 445,778             | 445,778| PASS   |

Note: Modern R (4.5.2) handles the recursive algorithm efficiently. All matrices complete within 1.5 seconds.

## Running

```bash
# Direct execution
Rscript Sudoku.R ../../Matrices/1.matrix

# Via benchmark script
./runMe.sh ../../Matrices/1.matrix
```

## Requirements

- R 4.0+ (tested with 4.5.2)
- No external packages required
