# Rust Sudoku Solver

A brute-force backtracking Sudoku solver implemented in Rust, designed to produce identical iteration counts to the C reference implementation.

## Algorithm

The solver uses a simple recursive backtracking algorithm:
1. Find the first empty cell (row-major order: top-to-bottom, left-to-right)
2. Try values 1-9 in ascending order
3. For each value, check if placement is valid (no conflicts in row, column, or 3x3 box)
4. If valid, place the value and recurse
5. If recursion fails, backtrack (reset cell to 0) and try next value
6. Count every placement attempt as an iteration (algorithm fingerprint)

## Validation

The iteration counts match the C reference exactly:

| Matrix | Iterations |
|--------|------------|
| 1 | 656 |
| 2 | 439,269 |
| 3 | 98,847 |
| 4 | 9,085 |
| 5 | 445,778 |
| 6 | 622,577,597 |

## Building

```bash
cd Sudoku
cargo build --release
```

The release binary will be at `target/release/Sudoku`.

## Running

```bash
./Sudoku_bin ../../Matrices/1.matrix
```

Or use the benchmark script:
```bash
./runMe.sh
```

## Dependencies

- Rust toolchain (cargo, rustc)
- `num-integer` crate for integer division

## Output Format

The solver produces output matching the C reference:
1. Filename
2. Input puzzle grid (as read)
3. "Puzzle:" header + input grid
4. "Puzzle:" header + solved grid
5. "Solved in Iterations=N"
6. "Seconds to process X.XXX"
