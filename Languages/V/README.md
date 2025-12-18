# V Sudoku Solver

Simple, fast Sudoku solver implemented in V (Vlang).

## About V

V is a simple, fast, and safe compiled programming language. It compiles to native binaries and has C-like syntax but with modern features like memory safety, first-class functions, and built-in concurrency.

## Algorithm

Brute-force backtracking solver that:
1. Finds the first empty cell (row-major order)
2. Tries values 1-9 in ascending order
3. Counts every attempt before validity check (algorithm fingerprint)
4. Recursively solves until complete or backtracks

## Validation Status

| Matrix | Expected Iterations | Actual  | Status |
|--------|---------------------|---------|--------|
| 1      | 656                 | 656     | ✓ Pass |
| 2      | 439,269             | 439,269 | ✓ Pass |
| 3      | 98,847              | 98,847  | ✓ Pass |
| 4      | 9,085               | 9,085   | ✓ Pass |
| 5      | 445,778             | 445,778 | ✓ Pass |

**All 5 matrices validated successfully.**

## Build & Run

```bash
# Compile with production optimizations
v -prod -o Sudoku Sudoku.v

# Run on a matrix
./Sudoku /path/to/matrix.matrix

# Run benchmarks via script
./runMe.sh /app/Matrices/*.matrix
```

## Performance Notes

- V compiles extremely fast (often <1 second)
- Production builds (-prod) enable optimizations
- Expected performance similar to C/C++ due to native compilation
- Memory-safe by default with no garbage collection overhead
