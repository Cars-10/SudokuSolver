# Julia Sudoku Solver

Brute-force backtracking Sudoku solver implemented in Julia, matching the C reference algorithm exactly.

## Algorithm

- **Search order**: Row-major (top-to-bottom, left-to-right)
- **Value order**: 1-9 ascending
- **Counting**: Every placement attempt is counted (algorithm fingerprint)

## Implementation Notes

### 1-Based Indexing

Julia uses 1-based indexing (unlike C's 0-based). Key adjustments:

```julia
# Cell scanning: 1:9 instead of 0:8
for r in 1:9
    for c in 1:9
        # ...
    end
end

# Box calculation (1-indexed adjustment)
# C: box_row = (row / 3) * 3 where row is 0-8
# Julia: box_row = div(row - 1, 3) * 3 + 1 where row is 1-9
box_row = div(row - 1, 3) * 3 + 1
box_col = div(col - 1, 3) * 3 + 1
```

### JIT Compilation

Julia is JIT-compiled, meaning:
- First run includes compilation overhead
- Subsequent runs show true performance
- Consider running twice if benchmarking

## Validation Status

| Matrix | Expected Iterations | Actual | Status |
|--------|---------------------|--------|--------|
| 1      | 656                 | 656    | PASS   |
| 2      | 439,269             | 439,269| PASS   |
| 3      | 98,847              | 98,847 | PASS   |
| 4      | 9,085               | 9,085  | PASS   |
| 5      | 445,778             | 445,778| PASS   |

## Running

```bash
# Direct execution
julia Sudoku.jl ../../Matrices/1.matrix

# Via benchmark script
./runMe.sh ../../Matrices/1.matrix
```

## Requirements

- Julia 1.6+ (tested with 1.12.2)
- Printf standard library (included)
