# Ada Sudoku Solver

Brute-force backtracking Sudoku solver implemented in Ada, matching the C reference algorithm exactly.

## Algorithm

- **Search order**: Row-major (top-to-bottom, left-to-right)
- **Value order**: 1-9 ascending
- **Counting**: Every placement attempt is counted (algorithm fingerprint)

## Implementation Notes

### 0-Based Indexing

Ada supports flexible array indexing. This implementation uses 0-based arrays to match the C reference:

```ada
type Board_Type is array (0 .. 8, 0 .. 8) of Integer;

-- Cell scanning: 0..8 (matches C's 0-8)
for J in 0 .. 8 loop
    for I in 0 .. 8 loop
        -- ...
    end loop;
end loop;

-- Box calculation (same as C due to 0-based indexing)
X0 := (X / 3) * 3;
Y0 := (Y / 3) * 3;
```

### Strong Typing

Ada is known for its strong type safety, making it ideal for safety-critical systems. The implementation maintains type safety while matching the algorithm exactly.

### Compiled Performance

Ada compiles to native code via GNAT (GNU Ada Compiler), providing performance comparable to C.

## Validation Status

| Matrix | Expected Iterations | Actual | Status |
|--------|---------------------|--------|--------|
| 1      | 656                 | 656    | PASS   |
| 2      | 439,269             | 439,269| PASS   |
| 3      | 98,847              | 98,847 | PASS   |
| 4      | 9,085               | 9,085  | PASS   |
| 5      | 445,778             | 445,778| PASS   |

## Running

```bash
# Direct execution
gnatmake -O2 -o Sudoku Sudoku.adb
./Sudoku ../../Matrices/1.matrix

# Via benchmark script
./runMe.sh ../../Matrices/1.matrix
```

## Requirements

- GNAT (GNU Ada Compiler) - part of GCC
- On macOS: `brew install gnat`
- On Ubuntu/Debian: `apt install gnat`
