# Scheme Sudoku Solver

Brute-force Sudoku solver implemented in GNU Guile (Scheme).

## Implementation Notes

- Uses `define` with `set!` for mutable counter
- Flat vector for puzzle grid: `(make-vector 81 0)`
- Cell access via `(vector-ref puzzle (+ (* row 9) col))`
- 0-based indexing matching C reference
- Row-major search order for empty cells

## Unique Challenges

Scheme is functional but supports mutation via `set!`:
- Global counter: `(define count 0)` with `(set! count (+ count 1))`
- Puzzle stored as mutable vector for efficient cell updates
- Named let (`let loop`) for iteration without explicit recursion helpers

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
guile sudoku.scm ../../Matrices/1.matrix

# Benchmark all matrices
./runMe.sh ../../Matrices/*.matrix
```

## Toolchain

- GNU Guile 3.0+
- Interpreted (no compilation needed)

## Performance Characteristics

- Lisp dialect with S-expression syntax
- First-class functions and closures
- Tail call optimization
- Mutable vectors for efficient array operations
- Interpreted execution
