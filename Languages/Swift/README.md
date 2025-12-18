# Swift Sudoku Solver

## Overview

Swift implementation of the brute-force Sudoku solver, matching the C reference algorithm exactly. Pure Swift with no external dependencies.

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

- Swift 5.x+ (tested with Apple Swift on macOS)
- swiftc compiler available in PATH
- No external dependencies

## Execution

### Run with modular script (recommended)
```bash
cd Languages/Swift
./runMe.sh ../../Matrices/1.matrix
```

### Run matrices 1-5
```bash
./runMe.sh ../../Matrices/{1,2,3,4,5}.matrix
```

### Manual compilation and execution
```bash
swiftc -O -o Sudoku Sudoku.swift
./Sudoku ../../Matrices/1.matrix
```

## Code Structure

| File          | Lines | Purpose                                    |
|---------------|------:|-------------------------------------------|
| Sudoku.swift  | 125   | Main solver implementation                 |
| Sudoku        | -     | Compiled executable                        |
| runMe.sh      | 75    | Modular benchmark script (uses common.sh)  |
| README.md     | -     | This file                                  |

### Key Functions

```swift
func isPossible(row: Int, col: Int, num: Int) -> Bool  // Check if placement is valid
func solve(row: Int, col: Int) -> Bool                  // Recursive brute-force solver
func readBoard(filename: String) -> Bool                // Load puzzle from .matrix file
func printBoard()                                        // Display current puzzle state
```

## Implementation Notes

- Uses recursive backtracking with row/col tracking
- Uses `Date()` for timing with millisecond precision
- Output format exactly matches C reference (spacing, headers, blank lines)
- Echoes raw matrix during file parsing for validation
- Compiles with -O optimization flag for performance
- Available on macOS natively; Docker may require manual Swift installation
