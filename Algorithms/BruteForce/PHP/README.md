# PHP Sudoku Solver

## Overview

Pure PHP implementation of the brute-force Sudoku solver, matching the C reference algorithm exactly. Uses no external dependencies.

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

- PHP 7.4+ (CLI mode)
- No external dependencies

## Execution

### Run with modular script (recommended)
```bash
cd Algorithms/BruteForce/PHP
./runMe.sh ../../Matrices/1.matrix
```

### Run matrices 1-5 (skip Matrix 6 - too slow for interpreted languages)
```bash
./runMe.sh ../../Matrices/{1,2,3,4,5}.matrix
```

### Direct execution
```bash
php Sudoku.php ../../Matrices/1.matrix
# or
./Sudoku.php ../../Matrices/1.matrix
```

## Performance

Validated performance on macOS (PHP 8.x):

| Matrix | Iterations | Time (s) | Notes            |
|--------|----------:|----------:|------------------|
| 1      | 656        | ~0.001    | Fast (simple)    |
| 2      | 439,269    | ~0.053    | Moderate         |
| 3      | 98,847     | ~0.013    | Moderate         |
| 4      | 9,085      | ~0.003    | Fast             |
| 5      | 445,778    | ~0.055    | Moderate         |

**Notes:**
- Matrix 6 (622M iterations) would take several minutes - skipped by default
- Times vary by CPU and PHP version
- PHP 8.x JIT can improve performance

## Code Structure

| File         | Lines | Purpose                                    |
|--------------|------:|--------------------------------------------|
| Sudoku.php   | 125   | Main solver implementation                 |
| runMe.sh     | 38    | Modular benchmark script (uses common.sh)  |
| README.md    | -     | This file                                  |

### Key Functions

```php
function is_possible($row, $col, $num)  // Check if placement is valid
function solve($row, $col)               // Recursive brute-force solver
function read_board($filename)           // Load puzzle from .matrix file
function print_board()                   // Display current puzzle state
```

## Implementation Notes

- Uses PHP arrays for the puzzle grid
- Global `$puzzle` and `$iterations` variables match C implementation style
- Output format exactly matches C reference (spacing, headers, blank lines)
- Uses `microtime(true)` for high-precision timing

## Output Format

Matches C reference exactly:

```
../../Matrices/1.matrix
9 2 0 0 0 0 5 8 4
0 0 0 5 0 0 0 0 3
... (initial puzzle)

Puzzle:
9 2 0 0 0 0 5 8 4
... (initial puzzle with header)

Puzzle:
9 2 1 6 3 7 5 8 4
... (solved puzzle)

Solved in Iterations=656

Seconds to process 0.001
```
