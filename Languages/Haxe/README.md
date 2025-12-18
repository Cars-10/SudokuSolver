# Haxe Sudoku Solver

## Overview

Haxe is a high-level cross-platform language and compiler that can produce native applications for multiple target platforms (JavaScript, C++, Java, C#, Python, PHP, Lua, Flash, HashLink, and more) from a single codebase.

## Language Heritage

- **Created**: 2005 by Nicolas Cannasse at Motion Twin
- **Paradigm**: Multi-paradigm (object-oriented, functional)
- **Typing**: Static with type inference
- **Lineage**: ActionScript/Flash heritage, evolved into modern cross-platform compiler
- **Philosophy**: "Write once, compile anywhere" - unified language for multiple runtime targets

## Implementation Notes

This implementation uses Haxe's interpreter mode (`--interp`) for execution. Key features:

- **Direct Execution**: No compilation step needed - runs directly via interpreter
- **Type System**: Full static typing with inference (like TypeScript for JS ecosystem)
- **Standard Library**: Uses sys.io.File for file I/O, Sys for system interaction
- **String Interpolation**: Uses `${}` syntax for embedded expressions

### Algorithm

The solver implements exact brute-force backtracking matching the C reference:

1. Find first empty cell (row-major order: top-to-bottom, left-to-right)
2. Try values 1-9 in ascending order
3. **Count BEFORE validity check** (critical for iteration matching)
4. If valid, place value and recurse
5. If recursion fails, backtrack (set cell back to 0)
6. If no values work, return false

### Counter Position

```haxe
for (val in 1...10) {
    count++;  // COUNT BEFORE validity check - algorithm fingerprint
    if (isValid(row, col, val)) {
        // ...
    }
}
```

## Building and Running

### Prerequisites

- Haxe 4.x (included in Docker image)
- Neko runtime (for some targets)

### Running

```bash
# Direct execution via interpreter
haxe --run Sudoku /path/to/matrix.matrix

# Via benchmark runner
./runMe.sh /app/Matrices/1.matrix
```

### Alternative Targets

Haxe can compile to multiple targets for different performance characteristics:

```bash
# Compile to JavaScript (runs with Node.js)
haxe -main Sudoku -js Sudoku.js
node Sudoku.js matrix.matrix

# Compile to HashLink bytecode (native-like performance)
haxe -main Sudoku -hl Sudoku.hl
hl Sudoku.hl matrix.matrix

# Compile to C++ (native binary)
haxe -main Sudoku -cpp build/
./build/Sudoku matrix.matrix
```

## Validation

| Matrix | Expected Iterations | Status |
|--------|---------------------|--------|
| 1      | 656                 | Pass   |
| 2      | 439,269             | Pass   |
| 3      | 98,847              | Pass   |
| 4      | 9,085               | Pass   |
| 5      | 445,778             | Pass   |

## Performance Notes

- **Interpreter Mode**: Slower than compiled targets but requires no build step
- **Cross-Platform**: Same code works on all supported platforms
- **Type Safety**: Compile-time type checking prevents runtime errors
- **Memory**: Automatic garbage collection via target runtime

## Files

- `Sudoku.hx` - Main solver implementation
- `runMe.sh` - Benchmark runner script
- `metadata.json` - Language metadata for benchmark system
- `README.md` - This file
