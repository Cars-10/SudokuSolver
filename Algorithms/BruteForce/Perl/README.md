# Perl Sudoku Solver

## Overview

Pure Perl implementation of the brute-force Sudoku solver, matching the C reference algorithm exactly. Uses only core modules (Time::HiRes).

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

- Perl 5.10+
- Time::HiRes (core module, included with Perl)
- No external CPAN modules required

## Execution

### Run with modular script (recommended)
```bash
cd Algorithms/BruteForce/Perl
./runMe.sh ../../Matrices/1.matrix
```

### Run matrices 1-5 (skip Matrix 6 - too slow for interpreted languages)
```bash
./runMe.sh ../../Matrices/{1,2,3,4,5}.matrix
```

### Direct execution
```bash
perl Sudoku.pl ../../Matrices/1.matrix
```

## Code Structure

| File         | Lines | Purpose                                    |
|--------------|------:|--------------------------------------------|
| Sudoku.pl    | 135   | Main solver implementation                 |
| runMe.sh     | 38    | Modular benchmark script (uses common.sh)  |
| README.md    | -     | This file                                  |

### Key Functions

```perl
sub is_valid($row, $col, $val)    # Check if placement is valid
sub solve()                        # Recursive brute-force solver
sub read_matrix_file($filename)    # Load puzzle from .matrix file
sub print_puzzle()                 # Display current puzzle state
```

## Implementation Notes

- Uses `use strict` and `use warnings` for safety
- Uses labeled loop (`OUTER:`) with `last OUTER` for early exit (matching C's `goto`)
- Array of arrays for 2D puzzle grid
- Output format exactly matches C reference (spacing, headers, blank lines)
- Path normalization handles Docker `/app/Matrices/` prefix
