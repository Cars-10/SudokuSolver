# Vala Sudoku Solver

Object-oriented Sudoku solver implemented in Vala.

## About Vala

Vala is a programming language that compiles to C and uses the GLib/GObject type system. It provides C#-like syntax with features like automatic memory management, properties, signals, and generics, while producing efficient native code through the C compiler.

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
# Compile with GIO package (needed for file I/O)
valac --pkg gio-2.0 -o Sudoku Sudoku.vala

# Run on a matrix
./Sudoku /path/to/matrix.matrix

# Run benchmarks via script
./runMe.sh /app/Matrices/*.matrix
```

## Dependencies

- valac (Vala compiler)
- libglib2.0-dev (GLib development files)
- GIO library (included with GLib)

## Performance Notes

- Vala compiles to C, then to native code
- Performance comparable to hand-written C code
- GObject overhead is minimal for computation-heavy tasks
- Uses GLib's efficient file I/O via GIO
