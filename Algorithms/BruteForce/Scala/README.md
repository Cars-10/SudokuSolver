# Scala Sudoku Solver

Brute-force backtracking Sudoku solver implemented in Scala, matching the C reference algorithm exactly.

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

**Note:** This implementation deliberately uses imperative style (`var`, mutable `Array`, `while` loops) to ensure the algorithm matches the C reference exactly. Functional Scala idioms were avoided as they could change iteration order and affect the iteration count.

## Requirements

- Scala 3.x
- JVM (OpenJDK 11+)

### macOS Setup

```bash
brew install scala openjdk
```

## Compilation and Execution

### Using runMe.sh (Recommended)

```bash
cd Languages/Scala
./runMe.sh ../../Matrices/1.matrix
```

Run all matrices:
```bash
./runMe.sh ../../Matrices/{1,2,3,4,5}.matrix
```

### Manual Compilation and Run

```bash
# Compile
scalac Sudoku.scala

# Run with scala run (Scala 3)
scala run Sudoku.scala -- ../../Matrices/1.matrix
```

## Output Format

The solver produces output matching the C reference format exactly:
1. Matrix file path
2. Raw matrix rows as read
3. "Puzzle:" + initial state
4. "Puzzle:" + solved state
5. "Solved in Iterations={count}"
6. "Seconds to process {time}"

## Implementation Notes

- Uses `var` and mutable `Array[Array[Int]]` for puzzle state
- Uses imperative `while` loops for cell search (avoids functional `for` comprehension side effects on iteration order)
- Uses `return` statements for early exits (deprecated in Scala 3 but produces compiler warnings, not errors)
- Output uses `StringBuilder` for efficiency
- Timing uses `System.nanoTime()` for precision

## Files

- `Sudoku.scala` - Main solver implementation
- `runMe.sh` - Benchmark script using common.sh
- `metrics.json` - Benchmark results
- `README.md` - This file
