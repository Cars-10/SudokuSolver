# F# Sudoku Solver

F# implementation of the brute-force backtracking Sudoku solver.

## Algorithm

The solver uses the exact same brute-force backtracking algorithm as the C reference:

1. **Row-major search**: Find empty cells top-to-bottom, left-to-right
2. **Value ordering**: Try values 1-9 in ascending order
3. **Iteration counting**: Count EVERY placement attempt (the algorithm fingerprint)
4. **Backtracking**: On failure, reset cell to 0 and try next value

## Implementation Notes

- **Language**: F# (ML-family functional language on .NET)
- **Paradigm**: Uses mutable variables for imperative algorithm matching
- **2D Arrays**: Uses `Array2D` with `[row, col]` indexing
- **Timing**: Uses `System.Diagnostics.Stopwatch` for precision timing

### Key F# Features Used

```fsharp
// Mutable state for algorithm matching
let mutable puzzle = Array2D.zeroCreate<int> 9 9
let mutable count = 0

// 2D array access
puzzle.[row, col] <- value

// Iteration counting before validity check (critical for algorithm fingerprint)
for value in 1 .. 9 do
    count <- count + 1  // COUNT EVERY ATTEMPT
    if isValid row col value then
        ...
```

## Validation Status

| Matrix | Expected | Actual | Status |
|--------|----------|--------|--------|
| 1      | 656      | 656    | ✓ Pass |
| 2      | 439,269  | 439,269| ✓ Pass |
| 3      | 98,847   | 98,847 | ✓ Pass |
| 4      | 9,085    | 9,085  | ✓ Pass |
| 5      | 445,778  | 445,778| ✓ Pass |

## Building & Running

### Prerequisites

- .NET SDK 9.0 or later
- F# compiler (included with .NET SDK)

### Run Benchmark

```bash
./runMe.sh ../../Matrices/1.matrix
```

### Run All Matrices

```bash
./runMe.sh ../../Matrices/*.matrix
```

### Manual Build & Run

```bash
dotnet build -c Release
dotnet run -c Release -- ../../Matrices/1.matrix
```

## Output Format

Output matches C reference exactly:

```
../../Matrices/1.matrix
9 2 0 0 0 0 5 8 4
...

Puzzle:
9 2 0 0 0 0 5 8 4
...

Puzzle:
9 2 1 6 3 7 5 8 4
...

Solved in Iterations=656

Seconds to process 0.035
```

## Files

- `Sudoku.fs` - Main solver implementation
- `Sudoku.fsproj` - F# project file
- `runMe.sh` - Benchmark runner script
- `metrics.json` - Benchmark results (auto-generated)
