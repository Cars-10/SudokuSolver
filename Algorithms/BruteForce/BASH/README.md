# Bash Sudoku Solver

Brute-force Sudoku solver implemented in pure Bash, matching the C reference algorithm exactly.

## Algorithm

- Row-major order cell scanning (top-to-bottom, left-to-right)
- Tries values 1-9 in ascending order
- Counts EVERY placement attempt (before validity check)
- Backtracking when no valid value found

## Requirements

- Bash 3.2+ (tested with 3.2.57 and 5.x)
- `bc` command for timing precision (optional)

## Performance Notes

Bash is inherently slow for numerical computation, but this implementation successfully completes all 5 matrices within the 5-minute timeout:

| Matrix | Iterations | Time   |
|--------|------------|--------|
| 1      | 656        | ~0.1s  |
| 2      | 439,269    | ~60s   |
| 3      | 98,847     | ~13s   |
| 4      | 9,085      | ~1s    |
| 5      | 445,778    | ~59s   |

## Usage

```bash
# Single matrix
bash Sudoku.sh ../../Matrices/1.matrix

# Via benchmark script
./runMe.sh ../../Matrices/1.matrix
```

## Validation Results

| Matrix | Expected Iterations | Actual  | Status |
|--------|---------------------|---------|--------|
| 1      | 656                 | 656     | ✓ Pass |
| 2      | 439,269             | 439,269 | ✓ Pass |
| 3      | 98,847              | 98,847  | ✓ Pass |
| 4      | 9,085               | 9,085   | ✓ Pass |
| 5      | 445,778             | 445,778 | ✓ Pass |

**All 5 matrices validated with exact iteration counts matching C reference.**

## Output Format

Matches C reference exactly:
```
../../Matrices/1.matrix
9 2 0 0 0 0 5 8 4
... (raw matrix as read)

Puzzle:
9 2 0 0 0 0 5 8 4
... (initial state)

Puzzle:
9 2 1 6 3 7 5 8 4
... (solved state)

Solved in Iterations=656

Seconds to process 0.119
```

## Implementation Notes

- Uses 1D array to simulate 9x9 grid (index = row*9 + col)
- `date +%s.%N` for sub-second timing (falls back to `date +%s`)
- Pure Bash with no external dependencies except `bc` for timing
- Recursion via function calls
- Despite Bash's interpreted nature, completes all matrices within timeout
