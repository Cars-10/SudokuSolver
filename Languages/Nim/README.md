# Nim Sudoku Solver

A brute-force Sudoku solver implemented in Nim, matching the C reference algorithm exactly.

## Algorithm

The solver uses a simple brute-force backtracking algorithm:
- Row-major order scanning (top-to-bottom, left-to-right)
- Try values 1-9 in ascending order
- Count EVERY placement attempt (before validity check)
- Backtrack when no valid value found

## Compilation

```bash
nim c -d:release -o:Sudoku Sudoku.nim
```

## Usage

```bash
./Sudoku path/to/puzzle.matrix
```

## Validation Status

| Matrix | Expected | Actual  | Status |
|--------|----------|---------|--------|
| 1      | 656      | 656     | ✓ Pass |
| 2      | 439,269  | 439,269 | ✓ Pass |
| 3      | 98,847   | 98,847  | ✓ Pass |
| 4      | 9,085    | 9,085   | ✓ Pass |
| 5      | 445,778  | 445,778 | ✓ Pass |

## Performance

- Compiles to native C code via Nim's C backend
- Performance comparable to hand-written C
- Uses ORC memory management for deterministic cleanup
