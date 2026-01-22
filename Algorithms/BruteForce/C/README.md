# C Sudoku Solver - Reference Baseline

## Overview

This is the **authoritative reference implementation** of the brute-force Sudoku solver. All other language implementations must match this algorithm exactly, validated by iteration count matching.

## Validation Status

✅ **FULLY VALIDATED** - 2025-12-16

| Matrix | Expected Iterations | Actual | Status |
|--------|--------------------:|-------:|:------:|
| 1      | 656                 | 656    | ✓ PASS |
| 2      | 439,269             | 439,269| ✓ PASS |
| 3      | 98,847              | 98,847 | ✓ PASS |
| 4      | 9,085               | 9,085  | ✓ PASS |
| 5      | 445,778             | 445,778| ✓ PASS |

**Format Validation:** ✅ All matrices pass (output format matches reference exactly)

## Algorithm

Pure brute-force recursive backtracking with these characteristics:

- **Search order:** Row-major (top-to-bottom, left-to-right)
- **Candidate values:** 1-9 in ascending order
- **Iteration counting:** Every value placement attempt is counted
- **Validation:** Row, column, and 3x3 box constraints checked

### Key Properties

1. **Deterministic:** Same input always produces same iteration count
2. **No optimization:** No MRV, no constraint propagation, no heuristics
3. **Algorithm fingerprint:** Iteration count proves exact algorithmic match

See [`ALGORITHM.md`](./ALGORITHM.md) for detailed pseudocode and implementation requirements.

## Compilation

### Default (used in benchmarks)
```bash
gcc -O3 -o Sudoku Sudoku.c
```

### Compiler Variants

The C implementation supports different optimization levels for performance comparison:

| Variant | Flags    | Description                          |
|---------|----------|--------------------------------------|
| O0      | `-O0`    | No optimization (debugging)          |
| O1      | `-O1`    | Basic optimization                   |
| O2      | `-O2`    | Moderate optimization                |
| **O3**  | **`-O3`**| **Aggressive optimization (default)**|
| Ofast   | `-Ofast` | Maximum optimization (may break IEEE)|
| Os      | `-Os`    | Optimize for size                    |

**Usage:**
```bash
# Default O3
./runMe.sh

# Custom variant
VARIANT=Ofast ./runMe.sh
```

## Execution

### Run with modular script (recommended)
```bash
cd Algorithms/BruteForce/C
./runMe.sh ../../../Matrices/1.matrix
```

### Run all matrices
```bash
./runMe.sh ../../../Matrices/{1,2,3,4,5}.matrix
```

### Direct execution
```bash
gcc -O3 -o Sudoku Sudoku.c
./Sudoku ../../../Matrices/1.matrix
```

## Performance Baseline

Validated performance on Docker (ubuntu:24.04, gcc 13.3.0, -O3):

| Matrix | Iterations | Time (s) | Memory (KB) |
|--------|----------:|----------:|------------:|
| 1      | 656        | ~0.001    | ~1,400      |
| 2      | 439,269    | ~0.040    | ~1,400      |
| 3      | 98,847     | ~0.009    | ~1,400      |
| 4      | 9,085      | ~0.001    | ~1,400      |
| 5      | 445,778    | ~0.042    | ~1,400      |

**Notes:**
- Times vary by CPU - iteration counts are canonical
- All matrices complete well under 5-minute timeout
- Memory usage is constant (stack-based recursion)

## Output Format

The C solver produces output in this exact format:

```
../Matrices/1.matrix
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

### Format Requirements (for other languages)

1. **Path:** Relative path `../Matrices/N.matrix` (not absolute)
2. **Spacing:** Single space between digits, trailing space after last digit
3. **Headers:** "Puzzle:" exactly (capital P, colon, no spaces)
4. **Blank lines:** After path, between puzzles, after iteration count
5. **Iteration line:** `Solved in Iterations=N` (exact format, no spaces around =)

## Code Structure

| File         | Lines | Purpose                                    |
|--------------|------:|--------------------------------------------|
| Sudoku.c     | 167   | Main solver implementation                 |
| runMe.sh     | 70    | Modular benchmark script (uses common.sh)  |
| README.md    | -     | This file (overview and usage)             |
| ALGORITHM.md | -     | Detailed algorithm specification           |

### Key Functions

```c
int isValid(int row, int col, int val)  // Check if placement is valid
int solve()                              // Recursive brute-force solver
int readMatrixFile(char *filename)       // Load puzzle from .matrix file
void printPuzzle()                       // Display current puzzle state
```

## Reference Implementation

This C implementation serves as the standard for:

1. **Algorithm correctness** - Iteration counts define the exact algorithm
2. **Output format** - All languages must match C's output format exactly
3. **Performance baseline** - All languages are compared to C's speed

## Validation Commands

```bash
# Run and validate
cd Algorithms/BruteForce/C
./runMe.sh ../../../Matrices/{1,2,3,4,5}.matrix
node ../../Metrics/validate_run.js ./metrics.json

# Expected output
✅ ALL VALIDATIONS PASSED
Total:   5 validations
✓ Passed: 5
```

## Implementation Notes

- **Line 125:** Iteration counter increments (critical for algorithm fingerprint)
- **Lines 104-113:** Row-major search for first empty cell
- **Lines 124-136:** Try candidates 1-9 in order, recursive solve, backtrack
- **Lines 72-96:** Validation checks row, column, and 3x3 box
- **Lines 33-40:** Path normalization for consistent output format

## Compiler Warnings

The code may produce warnings about implicit `free()` declaration:
```
warning: implicit declaration of function 'free'
```

This is harmless (missing `#include <stdlib.h>`), does not affect functionality or validation.

## Next Steps (for other languages)

1. Copy the algorithm from `ALGORITHM.md`
2. Implement in your target language
3. Run with `runMe.sh` (or create language-specific script)
4. Validate: `node Metrics/validate_run.js Algorithms/BruteForce/YourLanguage/metrics.json`
5. Ensure iteration counts match C exactly
6. Ensure output format matches C exactly

**Zero tolerance:** Even 1 iteration difference means algorithm mismatch - must be fixed.
