# C# Sudoku Solver

Brute-force backtracking Sudoku solver implemented in C#, matching the C reference algorithm exactly.

## Validation Status

| Matrix | Expected Iterations | Actual Iterations | Status |
|--------|---------------------|-------------------|--------|
| 1      | 656                 | 656               | ✓ Pass |
| 2      | 439,269             | 439,269           | ✓ Pass |
| 3      | 98,847              | 98,847            | ✓ Pass |
| 4      | 9,085               | 9,085             | ✓ Pass |
| 5      | 445,778             | 445,778           | ✓ Pass |

## Algorithm

The solver uses a brute-force backtracking approach with:
- **Row-major search**: Scans cells top-to-bottom, left-to-right
- **Sequential candidate values**: Tries values 1-9 in ascending order
- **Iteration counting**: Every placement attempt increments the counter

## Requirements

- .NET SDK 8.0+ (tested with .NET 9.0)

### macOS Setup

```bash
brew install dotnet
```

### Linux Setup

```bash
# Ubuntu/Debian
sudo apt-get install -y dotnet-sdk-8.0
```

## Build and Execution

### Using runMe.sh (Recommended)

```bash
cd Algorithms/BruteForce/C_Sharp
./runMe.sh ../../Matrices/1.matrix
```

Run all matrices:
```bash
./runMe.sh ../../Matrices/{1,2,3,4,5}.matrix
```

### Manual Build and Run

```bash
# Build
dotnet build --configuration Release

# Run
dotnet run --configuration Release -- ../../Matrices/1.matrix
```

## Output Format

The solver produces output matching the C reference format exactly:
1. Matrix file path
2. Raw matrix rows as read (space after each value)
3. "Puzzle:" + initial state
4. "Puzzle:" + solved state
5. "Solved in Iterations={count}"
6. "Seconds to process {time}"

## Implementation Notes

- Uses `int[,]` for 2D array (C# multidimensional array)
- Uses `Stopwatch` for high-precision timing
- Output uses `StringBuilder` for efficiency
- JIT compilation means first run is slower (subsequent runs are faster)

## Performance

C# with .NET JIT compilation provides excellent performance:
- Near-native speed after JIT warmup
- Efficient garbage collection for puzzle state
- `dotnet run --configuration Release` uses optimized build

## Files

- `Sudoku.cs` - Main solver implementation
- `Sudoku.csproj` - .NET project file
- `runMe.sh` - Benchmark script using common.sh
- `metrics.json` - Benchmark results
- `metadata.json` - Language metadata for benchmark UI
- `README.md` - This file
