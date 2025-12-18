# Rexx Sudoku Solver

## Overview

Rexx (Restructured Extended Executor) is a scripting language developed at IBM by Mike Cowlishaw in 1979. It was designed for ease of learning and reading, with a philosophy that "a language that is easy to read will be easy to write."

## Language Heritage

- **Created**: 1979 by Mike Cowlishaw at IBM
- **Paradigm**: Procedural, structured
- **Typing**: Dynamic (all data treated as strings by default)
- **Philosophy**: Human-readable syntax, minimal syntactic clutter
- **Used in**: IBM mainframes (MVS, OS/390, z/OS), OS/2, Amiga OS, embedded systems

## Implementation Notes

This implementation uses Regina Rexx, the most widely used open-source Rexx interpreter. Key features:

- **Compound Variables**: Rexx uses stem variables (like `puzzle.row.col`) for array-like structures
- **Procedure Expose**: Functions explicitly expose variables they need from the caller
- **String-Oriented**: All values are strings; arithmetic converts automatically
- **Case Insensitive**: Variable names and keywords are case-insensitive

### Algorithm

The solver implements exact brute-force backtracking matching the C reference:

1. Find first empty cell (row-major order: top-to-bottom, left-to-right)
2. Try values 1-9 in ascending order
3. **Count BEFORE validity check** (critical for iteration matching)
4. If valid, place value and recurse
5. If recursion fails, backtrack (set cell back to 0)
6. If no values work, return false

### Counter Position

```rexx
do val = 1 to 9
    count = count + 1  /* COUNT BEFORE validity check - algorithm fingerprint */
    if isValid(foundRow, foundCol, val) then do
        /* ... */
    end
end
```

### Array Implementation

Rexx uses compound (stem) variables for multi-dimensional arrays:

```rexx
/* 0-indexed to match C algorithm */
puzzle.0.0 = 9   /* Row 0, Column 0 */
puzzle.row.col = val
```

## Building and Running

### Prerequisites

- Regina Rexx 3.x (included in Docker image)

### Running

```bash
# Direct execution
regina Sudoku.rexx /path/to/matrix.matrix

# Via benchmark runner
./runMe.sh /app/Matrices/1.matrix
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

- **Interpreted**: Rexx is purely interpreted, expect slower execution
- **String-Based**: All data manipulation involves string operations
- **Legacy**: Despite its age, still actively used in enterprise environments
- **Recursion**: Regina handles deep recursion well for backtracking

## Historical Significance

Rexx was one of the first scripting languages designed for human readability:

- Influenced many later languages (Python cites Rexx as an influence)
- Default shell on OS/2
- Standard scripting language on IBM mainframes
- One of the first languages with a formal ANSI standard (1996)

## Files

- `Sudoku.rexx` - Main solver implementation
- `runMe.sh` - Benchmark runner script
- `metadata.json` - Language metadata for benchmark system
- `README.md` - This file
