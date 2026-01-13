# Erlang Sudoku Solver

Brute-force Sudoku solver implemented in Erlang (escript).

## Implementation Notes

- Uses process dictionary for mutable state: `put/2` and `get/1`
- Counter stored as `put(count, N)` and incremented on each value attempt
- Puzzle cells stored as `put({puzzle, Row, Col}, Value)`
- 0-based indexing matching C reference
- Row-major search order for empty cells

## Unique Challenges

Erlang is immutable by default, so we use the process dictionary for mutable state:
- `put(count, get(count) + 1)` increments the iteration counter
- Puzzle grid stored as individual key-value pairs in process dictionary
- This allows backtracking without passing state through recursion

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
escript sudoku.erl ../../Matrices/1.matrix

# Benchmark all matrices
./runMe.sh ../../Matrices/*.matrix
```

## Toolchain

- Erlang/OTP 25+ (escript)
- Interpreted (no compilation needed)

## Performance Characteristics

- Functional concurrent language with actor model
- Process dictionary provides mutable state per process
- Pattern matching for control flow
- Lists and tuples for data structures
- Interpreted execution (escript mode)
