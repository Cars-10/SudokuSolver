# Pascal Sudoku Solver

Brute-force Sudoku solver implemented in Free Pascal.

## Implementation Notes

- Uses 2D array for puzzle grid: `puzzle: array[0..8, 0..8] of Integer`
- 0-based indexing matching C reference
- Row-major search order for empty cells
- Iteration counter incremented before validity check (matching C algorithm exactly)
- **Important**: Must use `Solve()` with parentheses for recursive calls (Free Pascal quirk)

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
# Compile and run single matrix
fpc -O2 sudoku.pas
./sudoku ../../Matrices/1.matrix

# Benchmark all matrices
./runMe.sh ../../Matrices/*.matrix
```

## Toolchain

- Free Pascal Compiler 3.2.2
- Compiled executable

## Performance Characteristics

- Classic structured programming language
- Compiled to native code (fast execution)
- Strong typing prevents many runtime errors
- Efficient recursive implementation
