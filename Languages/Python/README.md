# Python Sudoku Solver

## Overview

Pure Python implementation of the brute-force Sudoku solver, matching the C reference algorithm exactly. Uses no external dependencies (no NumPy).

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

- Python 3.6+ (uses f-strings)
- No external dependencies

## Execution

### Run with modular script (recommended)
```bash
cd Languages/Python
./runMe.sh ../../Matrices/1.matrix
```

### Run matrices 1-5 (skip Matrix 6 - too slow for interpreted languages)
```bash
./runMe.sh ../../Matrices/{1,2,3,4,5}.matrix
```

### Direct execution
```bash
python3 Sudoku.py ../../Matrices/1.matrix
```

## Performance

Validated performance on macOS (Python 3.11):

| Matrix | Iterations | Time (s) | vs C Reference |
|--------|----------:|----------:|---------------:|
| 1      | 656        | ~0.02     | ~20x slower    |
| 2      | 439,269    | ~0.31     | ~8x slower     |
| 3      | 98,847     | ~0.08     | ~9x slower     |
| 4      | 9,085      | ~0.02     | ~20x slower    |
| 5      | 445,778    | ~0.30     | ~7x slower     |

**Notes:**
- Matrix 6 (622M iterations) would take ~30+ minutes - skipped by default
- Times vary by CPU and Python version
- PyPy could significantly improve performance

## Code Structure

| File         | Lines | Purpose                                    |
|--------------|------:|--------------------------------------------|
| Sudoku.py    | 137   | Main solver implementation                 |
| runMe.sh     | 38    | Modular benchmark script (uses common.sh)  |
| README.md    | -     | This file                                  |

### Key Functions

```python
def is_valid(row, col, val)    # Check if placement is valid
def solve()                     # Recursive brute-force solver
def read_matrix_file(filename)  # Load puzzle from .matrix file
def print_puzzle()              # Display current puzzle state
```

## Implementation Notes

- Uses pure Python lists instead of NumPy for portability
- Global `puzzle` and `count` variables match C implementation style
- Output format exactly matches C reference (spacing, headers, blank lines)
- Path normalization handles Docker `/app/Matrices/` prefix

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
