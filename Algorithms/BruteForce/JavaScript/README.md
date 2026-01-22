# JavaScript Sudoku Solver

## Overview

Pure JavaScript (Node.js) implementation of the brute-force Sudoku solver, matching the C reference algorithm exactly. Uses ES modules, no external dependencies.

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

- Node.js 14+ (uses ES modules and performance.now())
- No npm dependencies required

## Execution

### Run with modular script (recommended)
```bash
cd Algorithms/BruteForce/JavaScript
./runMe.sh ../../Matrices/1.matrix
```

### Run matrices 1-5
```bash
./runMe.sh ../../Matrices/{1,2,3,4,5}.matrix
```

### Direct execution
```bash
node Sudoku.js ../../Matrices/1.matrix
```

## Code Structure

| File         | Lines | Purpose                                    |
|--------------|------:|--------------------------------------------|
| Sudoku.js    | 137   | Main solver implementation (ES modules)    |
| runMe.sh     | 90    | Modular benchmark script (uses common.sh)  |
| README.md    | -     | This file                                  |

### Key Functions

```javascript
function isValid(row, col, val)    // Check if placement is valid
function solve()                    // Recursive brute-force solver
function readMatrixFile(filename)   // Load puzzle from .matrix file
function printPuzzle()              // Display current puzzle state
```

## Implementation Notes

- Uses ES modules (`import` syntax)
- Uses labeled statement (`outer:`) with `break outer` for early loop exit
- Uses `performance.now()` for high-resolution timing
- Output format exactly matches C reference (spacing, headers, blank lines)
- Path normalization handles Docker `/app/Matrices/` prefix
