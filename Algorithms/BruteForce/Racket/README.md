# Racket Sudoku Solver

Modern Scheme implementation using Racket's batteries-included approach.

## Implementation Details

- **Algorithm**: Brute-force backtracking, row-major traversal
- **Iteration Counting**: Before validity check (matches C reference exactly)
- **Data Structure**: Flat vector (81 elements) for 9x9 grid

## Files

- `Sudoku.rkt` - Main solver implementation
- `runMe.sh` - Benchmark runner (uses common.sh)
- `metadata.json` - Language metadata
- `metrics.json` - Benchmark results (generated)

## Running

```bash
# Single matrix
racket Sudoku.rkt ../../Matrices/1.matrix

# Full benchmark
./runMe.sh
```

## Algorithm Match

The solver exactly matches the C reference implementation:
1. Row-major cell traversal (row 0-8, then col 0-8)
2. Values tried in order 1-9
3. Iteration count incremented BEFORE validity check
4. Output format matches C exactly (path, input, Puzzle headers, solved, iterations)

## Validation Status

| Matrix | Expected | Actual | Status |
|--------|----------|--------|--------|
| 1      | 656      | 656    | PASS   |
| 2      | 439,269  | 439,269| PASS   |
| 3      | 98,847   | 98,847 | PASS   |
| 4      | 9,085    | 9,085  | PASS   |
| 5      | 445,778  | 445,778| PASS   |

## Toolchain

- Racket 8.x (cs variant with JIT compilation)
