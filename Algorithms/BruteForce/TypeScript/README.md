# TypeScript Sudoku Solver

## Overview

TypeScript implementation of the brute-force Sudoku solver, matching the C reference algorithm exactly. Compiles to JavaScript and runs on Node.js.

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

- Node.js 14+ (uses ES2020 features and performance.now())
- npm (for dependency management)
- TypeScript 5.0+ (included as dev dependency)

## Execution

### Run with modular script (recommended)
```bash
cd Algorithms/BruteForce/TypeScript
./runMe.sh ../../Matrices/1.matrix
```

### Run matrices 1-5
```bash
./runMe.sh ../../Matrices/{1,2,3,4,5}.matrix
```

### Manual compilation and execution
```bash
npm install
npm run build
node out/Sudoku.js ../../Matrices/1.matrix
```

## Code Structure

| File         | Lines | Purpose                                    |
|--------------|------:|-------------------------------------------|
| Sudoku.ts    | 136   | Main solver implementation (TypeScript)    |
| out/Sudoku.js| -     | Compiled JavaScript output                 |
| runMe.sh     | 103   | Modular benchmark script (uses common.sh)  |
| package.json | -     | NPM package configuration                  |
| tsconfig.json| -     | TypeScript compiler configuration          |
| README.md    | -     | This file                                  |

### Key Functions

```typescript
function isValid(row: number, col: number, val: number): boolean  // Check if placement is valid
function solve(): boolean                                          // Recursive brute-force solver
function readMatrixFile(filename: string): void                    // Load puzzle from .matrix file
function printPuzzle(): void                                       // Display current puzzle state
```

## Implementation Notes

- Uses TypeScript strict mode for type safety
- Compiles to ES2020 JavaScript with CommonJS modules
- Uses labeled statement (`outer:`) with `break outer` for early loop exit
- Uses built-in `performance.now()` for high-resolution timing
- Output format exactly matches C reference (spacing, headers, blank lines)
- Path normalization handles Docker `/app/Matrices/` prefix
- No external runtime dependencies (only dev dependencies for compilation)
