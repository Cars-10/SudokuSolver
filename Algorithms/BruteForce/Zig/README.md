# Zig Sudoku Solver

Brute-force Sudoku solver implemented in Zig, a modern systems programming language.

## Algorithm

The solver uses the same brute-force backtracking algorithm as the C reference implementation:
1. Find the first empty cell (row-major order, top-to-bottom, left-to-right)
2. Try values 1-9 in ascending order
3. For each value, increment iteration counter and check validity
4. If valid, place value and recurse
5. If recursion returns solved, propagate success
6. Otherwise backtrack (reset cell to 0) and try next value

## Iteration Counting

Iterations are counted for EVERY placement attempt (valid or invalid), matching the C reference exactly. This serves as an algorithm fingerprint to verify implementation correctness.

## Validation Status

| Matrix | Expected Iterations | Actual | Status |
|--------|---------------------|--------|--------|
| 1      | 656                 | 656    | PASS   |
| 2      | 439,269             | 439,269| PASS   |
| 3      | 98,847              | 98,847 | PASS   |
| 4      | 9,085               | 9,085  | PASS   |
| 5      | 445,778             | 445,778| PASS   |

## Building

```bash
zig build-exe Sudoku.zig -O ReleaseFast
```

## Running

```bash
./Sudoku <matrix_file>
```

## Output Format

Matches C reference output exactly:
```
[9x9 solved grid with space-separated values]
Solved in Iterations=N
```

## Notes

- Zig 0.13.0 required
- Uses `@import("std")` for standard library
- Zero-cost abstractions for memory safety
- No hidden control flow
- Excellent C interoperability
