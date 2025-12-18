# Lua Sudoku Solver

Brute-force Sudoku solver implemented in Lua, matching the C reference algorithm exactly.

## Algorithm

- Row-major order cell scanning (top-to-bottom, left-to-right)
- Tries values 1-9 in ascending order
- Counts EVERY placement attempt (before validity check)
- Backtracking when no valid value found

## Requirements

- Lua 5.4+ (tested with 5.4.8)

## Usage

```bash
# Single matrix
lua Sudoku.lua ../../Matrices/1.matrix

# Multiple matrices
lua Sudoku.lua ../../Matrices/*.matrix

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

Seconds to process 0.001
```

## Implementation Notes

- Uses Lua's 1-indexed arrays (adjusted from C's 0-indexed)
- `os.clock()` for CPU time measurement
- Pattern matching for matrix parsing
- `goto` statement for early exit from nested loops
