# Prolog Sudoku Solver

Brute-force Sudoku solver implemented in SWI-Prolog.

## Implementation Notes

- Uses dynamic predicates for mutable state: `puzzle/3` and `count/1`
- Explicit iteration counting with `inc_count` predicate (bypasses Prolog's native backtracking for counting)
- 0-based indexing matching C reference
- Row-major search order for empty cells via `find_empty/2`
- Uses `assertz`/`retract` for puzzle state manipulation

## Unique Challenges

Prolog has built-in backtracking, but we need to count iterations explicitly:
- Counter is stored as a dynamic fact
- `inc_count` retracts current count, increments, and asserts new value
- This ensures count persists across Prolog's natural backtracking

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
swipl -q -s sudoku.pl -- ../../Matrices/1.matrix

# Benchmark all matrices
./runMe.sh ../../Matrices/*.matrix
```

## Toolchain

- SWI-Prolog 9.0.4
- Interpreted (no compilation)

## Performance Characteristics

- Logic programming paradigm with declarative style
- Built-in backtracking (but manually controlled here for counting)
- Dynamic predicates allow mutable state
- Interpreted execution (slower than compiled languages)
