# Java Sudoku Solver

## Overview

Java implementation of the brute-force Sudoku solver, matching the C reference algorithm exactly. Pure Java with no external dependencies.

## Validation Status

✅ **FULLY VALIDATED** - 2025-12-18

| Matrix | Expected Iterations | Actual | Status |
|--------|--------------------:|-------:|:------:|
| 1      | 656                 | 656    | ✓ PASS |
| 2      | 439,269             | 439,269| ✓ PASS |
| 3      | 98,847              | 98,847 | ✓ PASS |
| 4      | 9,085               | 9,085  | ✓ PASS |
| 5      | 445,778             | 445,778| ✓ PASS |

**Format Validation:** ✅ All matrices pass (output format matches C reference exactly)

## Algorithm

Pure brute-force recursive backtracking matching C reference:

- **Search order:** Row-major (top-to-bottom, left-to-right)
- **Candidate values:** 1-9 in ascending order
- **Iteration counting:** Every value placement attempt is counted
- **Validation:** Row, column, and 3x3 box constraints checked

## Requirements

- Java JDK 8+ (tested with OpenJDK 25)
- No external dependencies

## Execution

### Run with modular script (recommended)
```bash
cd Languages/Java
./runMe.sh ../../Matrices/1.matrix
```

### Run matrices 1-5
```bash
./runMe.sh ../../Matrices/{1,2,3,4,5}.matrix
```

### Manual compilation and execution
```bash
javac Sudoku.java
java Sudoku ../../Matrices/1.matrix
```

## Code Structure

| File         | Lines | Purpose                                    |
|--------------|------:|-------------------------------------------|
| Sudoku.java  | 145   | Main solver implementation                 |
| Sudoku.class | -     | Compiled bytecode                          |
| runMe.sh     | 103   | Modular benchmark script (uses common.sh)  |
| README.md    | -     | This file                                  |

### Key Methods

```java
private static boolean isValid(int row, int col, int val)  // Check if placement is valid
private static boolean solve()                              // Recursive brute-force solver
private static void readMatrixFile(String filename)         // Load puzzle from .matrix file
private static void printPuzzle()                           // Display current puzzle state
```

## Implementation Notes

- Uses labeled break (`outer:`) for early loop exit
- Uses `System.nanoTime()` for high-resolution timing
- Output format exactly matches C reference (spacing, headers, blank lines)
- Path normalization handles Docker `/app/Matrices/` prefix
- Handles Homebrew OpenJDK installation on macOS
