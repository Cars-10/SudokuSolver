# Raku Sudoku Solver

**Status:** Fully validated on all 5 matrices

## Overview

Raku (formerly Perl 6) implementation using native array syntax and modern functional features. Runs on Rakudo runtime.

## Algorithm

Exact match to C reference implementation:
- Find first empty cell (row-major order)
- Try values 1-9 in ascending order
- **COUNT BEFORE** validity check (critical for iteration matching)
- Recursive backtracking

## Implementation Details

- Uses shaped arrays: `my @puzzle[9;9]`
- Semicolon syntax for multi-dimensional indexing
- Raku's `^9` range operator (0..8)
- Native integer arithmetic with `div` operator

## Validation Results

| Matrix | Iterations | Status |
|--------|-----------|---------|
| 1 | 656 | ✓ |
| 2 | 439,269 | ✓ |
| 3 | 98,847 | ✓ |
| 4 | 9,085 | ✓ |
| 5 | 445,778 | ✓ |

All iteration counts match C reference exactly.

## Running

```bash
# Single matrix
raku Sudoku.raku /app/Matrices/1.matrix

# All matrices with metrics
./runMe.sh /app/Matrices/{1,2,3,4,5}.matrix
```

## Performance

Raku is interpreted, so performance is slower than compiled languages. Matrix 5 completes in ~1-2 seconds.

## Language Notes

- Raku is the successor to Perl 5 with complete redesign
- Supports gradual typing, grammars, and Unicode
- Uses MoarVM for JIT compilation
- Part of Tier 4 (Transpiled & Niche Languages)
