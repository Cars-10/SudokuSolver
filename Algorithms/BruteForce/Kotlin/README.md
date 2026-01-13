# Kotlin Sudoku Solver

Sudoku solver implementation in Kotlin, matching the C reference algorithm exactly.

## Validation Status

| Matrix | Expected Iterations | Actual Iterations | Status |
|--------|---------------------|-------------------|--------|
| 1      | 656                 | 656               | ✓ Pass |
| 2      | 439,269             | 439,269           | ✓ Pass |
| 3      | 98,847              | 98,847            | ✓ Pass |
| 4      | 9,085               | 9,085             | ✓ Pass |
| 5      | 445,778             | 445,778           | ✓ Pass |

**All matrices validated** - Iteration counts match C reference exactly.

## Algorithm

Exact port of the C reference brute-force backtracking algorithm:

- **Search order:** Row-major (top-to-bottom, left-to-right)
- **Candidate values:** 1-9 in ascending order
- **Iteration counting:** Every value placement attempt increments count
- **Validation:** Check row, column, and 3x3 box constraints

The algorithm fingerprint (iteration count) matches C exactly, confirming identical search behavior.

## Requirements

- Kotlin 1.9+ (tested with 2.2.21)
- JVM 11+ (tested with JRE 25.0.1)

## Compilation

```bash
# Compile to standalone JAR
kotlinc Sudoku.kt -include-runtime -d Sudoku.jar
```

## Execution

```bash
# Run solver on a matrix
java -jar Sudoku.jar ../../Matrices/1.matrix

# Run all matrices via benchmark script
./runMe.sh ../../Matrices/*.matrix
```

## Output Format

Matches C reference exactly:

1. Path to matrix file
2. Raw matrix rows (echo from file)
3. Blank line
4. "Puzzle:" header + initial state
5. Blank line
6. "Puzzle:" header + solved state
7. Blank line
8. "Solved in Iterations={count}"
9. Blank line
10. "Seconds to process {time:.3f}"

## Performance Notes

- JVM warm-up affects initial run times
- Memory overhead typical of JVM (~50MB for this application)
- Kotlin compiles to JVM bytecode, performance similar to Java
- Native compilation with Kotlin/Native could improve startup time

## Files

- `Sudoku.kt` - Main solver implementation
- `Sudoku.jar` - Compiled standalone JAR
- `runMe.sh` - Benchmark script (uses common.sh)
- `metrics.json` - Benchmark results
- `metadata.json` - Language metadata
