# GNU Octave Sudoku Solver

**Status:** Fully validated on all 5 matrices

## Overview

GNU Octave implementation compatible with MATLAB syntax. Uses 1-based indexing and procedural function definitions.

## Algorithm

Exact match to C reference implementation:
- Find first empty cell (row-major order)
- Try values 1-9 in ascending order
- **COUNT BEFORE** validity check (critical for iteration matching)
- Recursive backtracking

## Implementation Details

- **1-based indexing** (Octave/MATLAB convention, not 0-based)
- Global variables for puzzle state and counter
- Functions defined with `function ... endfunction` syntax
- Matrix operations using Octave's native array handling

## Key Differences from C

- Arrays indexed from 1, not 0
- Box calculation: `floor((row-1)/3)*3 + 1`
- Loop ranges: `for i = 1:9` instead of `for (int i = 0; i < 9; i++)`

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
octave --no-gui --no-window-system Sudoku.m /app/Matrices/1.matrix

# All matrices with metrics
./runMe.sh /app/Matrices/{1,2,3,4,5}.matrix
```

## Performance

Octave is interpreted with JIT compilation. Performance is moderate - Matrix 5 completes in ~0.5-1 second.

## Language Notes

- GNU Octave is a free MATLAB alternative
- Focus on numerical computation and matrix operations
- Compatible with most MATLAB scripts
- Part of Tier 4 (Transpiled & Niche Languages)
