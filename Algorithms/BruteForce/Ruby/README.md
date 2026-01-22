# Ruby Sudoku Solver

## Overview

Pure Ruby implementation of the brute-force Sudoku solver, matching the C reference algorithm exactly. Uses no external gems.

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

- Ruby 2.5+ (standard library only)
- No external gems required

## Execution

### Run with modular script (recommended)
```bash
cd Algorithms/BruteForce/Ruby
./runMe.sh ../../Matrices/1.matrix
```

### Run matrices 1-5 (skip Matrix 6 - too slow for interpreted languages)
```bash
./runMe.sh ../../Matrices/{1,2,3,4,5}.matrix
```

### Direct execution
```bash
ruby Sudoku.rb ../../Matrices/1.matrix
```

## Code Structure

| File         | Lines | Purpose                                    |
|--------------|------:|--------------------------------------------|
| Sudoku.rb    | 123   | Main solver implementation                 |
| runMe.sh     | 38    | Modular benchmark script (uses common.sh)  |
| README.md    | -     | This file                                  |

### Key Functions

```ruby
def is_valid(row, col, val)    # Check if placement is valid
def solve                       # Recursive brute-force solver
def read_matrix_file(filename)  # Load puzzle from .matrix file
def print_puzzle                # Display current puzzle state
```

## Implementation Notes

- Uses Ruby global variables ($puzzle, $count) to match C implementation style
- Uses `catch`/`throw` for early exit from nested loops (matching C's `goto`)
- Output format exactly matches C reference (spacing, headers, blank lines)
- Path normalization handles Docker `/app/Matrices/` prefix
