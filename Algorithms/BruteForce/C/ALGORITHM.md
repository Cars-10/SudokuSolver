# Brute-Force Sudoku Algorithm Specification

## Overview

This document defines the **exact algorithm** that all language implementations must follow. The iteration count serves as the algorithm's fingerprint - if your iteration count matches C's, your algorithm is correct.

## Algorithm Pseudocode

```
solve(puzzle):
    # Find first empty cell (row-major order)
    for row = 0 to 8:
        for col = 0 to 8:
            if puzzle[row][col] == 0:
                goto found_empty_cell

    # No empty cell found = solved
    print_puzzle()
    print "Solved in Iterations=" + count
    return SUCCESS

found_empty_cell:
    # Try candidates 1-9 in order
    for candidate = 1 to 9:
        count = count + 1  # INCREMENT ON EVERY ATTEMPT

        if is_valid(row, col, candidate):
            puzzle[row][col] = candidate

            if solve(puzzle) == SUCCESS:
                return SUCCESS

            puzzle[row][col] = 0  # Backtrack

    return FAILURE  # No valid candidate found
```

## Critical Implementation Details

### 1. Iteration Counting

**MOST IMPORTANT:** The iteration counter MUST increment inside the candidate loop (1-9), BEFORE the validity check.

```c
for (int val = 1; val <= 9; val++) {
    count++;  // ← COUNT EVERY ATTEMPT (even invalid ones)

    if (isValid(row, col, val)) {
        // ...
    }
}
```

**Why this matters:**
- Counting happens regardless of validity
- This ensures deterministic, reproducible counts
- The count is the algorithm's "fingerprint"

### 2. Search Order

**Row-Major Order** (top-to-bottom, left-to-right):

```
Cell order: [0,0] → [0,1] → ... → [0,8] →
            [1,0] → [1,1] → ... → [1,8] →
            ...
            [8,0] → [8,1] → ... → [8,8]
```

**NOT column-major, NOT random, NOT optimized** (e.g., MRV heuristic).

### 3. Candidate Order

Always try candidates in ascending order: **1, 2, 3, 4, 5, 6, 7, 8, 9**

**NOT** descending (9→1), **NOT** random, **NOT** ordered by constraint.

### 4. Validation Logic

Check if placing `val` at `[row, col]` violates constraints:

**Row check:**
```
for i = 0 to 8:
    if puzzle[row][i] == val:
        return INVALID
```

**Column check:**
```
for i = 0 to 8:
    if puzzle[i][col] == val:
        return INVALID
```

**3x3 Box check:**
```
box_row = (row / 3) * 3
box_col = (col / 3) * 3
for i = 0 to 2:
    for j = 0 to 2:
        if puzzle[box_row + i][box_col + j] == val:
            return INVALID
```

**Return VALID if all checks pass.**

### 5. Backtracking

After trying all candidates 1-9, if none lead to a solution:
```
puzzle[row][col] = 0  # Reset cell
return FAILURE
```

This causes the recursion to unwind and try the next candidate in the previous cell.

## Common Mistakes That Break Iteration Matching

### ❌ Wrong: Optimizations
```python
# MRV (Minimum Remaining Values)
cell = find_cell_with_fewest_candidates()  # ← WRONG

# Constraint propagation
eliminate_impossible_values()  # ← WRONG
```

### ❌ Wrong: Different Search Order
```python
# Column-major
for col in range(9):
    for row in range(9):  # ← WRONG ORDER
```

### ❌ Wrong: Different Candidate Order
```python
for val in range(9, 0, -1):  # ← WRONG (9 to 1)
```

###  ❌ Wrong: Counting Only Valid Placements
```python
if is_valid(row, col, val):
    count += 1  # ← WRONG (misses invalid attempts)
```

### ✅ Correct: Pure Brute-Force
```python
for val in range(1, 10):  # 1 to 9
    count += 1  # Count EVERY attempt
    if is_valid(row, col, val):
        # ... try placement
```

## Why Iteration Counts Matter

The iteration count is a **cryptographic hash** of your algorithm:

- **Same algorithm** → Same iteration count
- **Different algorithm** → Different iteration count (almost certainly)

### Example Iteration Counts

| Matrix | Iterations | Why This Many?                    |
|--------|----------:|-----------------------------------|
| 1      | 656        | Specific to this puzzle + algorithm|
| 2      | 439,269    | Harder puzzle = more backtracking |
| 5      | 445,778    | Similar difficulty to Matrix 2    |

If you get **different** counts, your algorithm differs from the reference. Common causes:
- Different search order
- Different candidate order
- Counting in wrong place
- Using optimizations/heuristics

## Output Format Requirements

### Path Format
```
../Matrices/1.matrix  ← Relative path (NOT /app/Matrices/1.matrix)
```

### Puzzle Format
```
9 2 0 0 0 0 5 8 4 ← Single space between digits
0 0 0 5 0 0 0 0 3 ← Trailing space after last digit
... (9 rows total)
```

### Iteration Line
```
Solved in Iterations=656  ← Exact format, no spaces around =
```

### Complete Output Structure
```
../Matrices/N.matrix
<initial puzzle digits>

Puzzle:
<initial puzzle with header>

Puzzle:
<solved puzzle>

Solved in Iterations=NNN

Seconds to process X.XXX
```

## Validation Process

1. **Run your solver** on Matrix 1
2. **Check iteration count:** Must be exactly **656**
3. **Check output format:** Must match reference exactly
4. **Run validation:** `node Metrics/validate_run.js your_metrics.json`
5. **Expected result:** ✅ ALL VALIDATIONS PASSED

If validation fails:
- Compare your algorithm line-by-line with this spec
- Check search order (row-major?)
- Check candidate order (1→9?)
- Check where you increment `count`
- Check output format (path, spacing, headers)

## Reference Implementation (C)

See `Sudoku.c` for the canonical implementation:

```c
int solve() {
  // Find first empty cell (row-major)
  int row = -1, col = -1;
  for (int r = 0; r < 9; r++) {
    for (int c = 0; c < 9; c++) {
      if (puzzle[r][c] == 0) {
        row = r;
        col = c;
        goto found_empty;
      }
    }
  }

found_empty:
  if (row == -1) {
    // Solved
    printPuzzle();
    printf("\nSolved in Iterations=%i\n\n", count);
    return 1;
  }

  // Try values 1-9
  for (int val = 1; val <= 9; val++) {
    count++;  // COUNT EVERY ATTEMPT

    if (isValid(row, col, val)) {
      puzzle[row][col] = val;

      if (solve() == 1) {
        return 1;  // Solved
      }

      puzzle[row][col] = 0;  // Backtrack
    }
  }

  return 0;  // Failed
}
```

## Summary Checklist

Before submitting your implementation, verify:

- [ ] Search order is row-major (top-to-bottom, left-to-right)
- [ ] Candidates tried in order 1-9
- [ ] Counter increments on EVERY candidate attempt
- [ ] Counter increments BEFORE validity check
- [ ] No optimizations (MRV, constraint propagation, etc.)
- [ ] Validation checks row, column, and 3x3 box
- [ ] Backtracking resets cell to 0
- [ ] Output format matches reference exactly
- [ ] Matrix 1 produces exactly **656** iterations
- [ ] All 5 matrices pass validation

**Zero tolerance:** Even 1 iteration difference means your algorithm is wrong.
