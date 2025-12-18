# Elixir Sudoku Solver

Elixir implementation of the brute-force backtracking Sudoku solver.

## Algorithm

The solver uses the exact same brute-force backtracking algorithm as the C reference:

1. **Row-major search**: Find empty cells top-to-bottom, left-to-right
2. **Value ordering**: Try values 1-9 in ascending order
3. **Iteration counting**: Count EVERY placement attempt (the algorithm fingerprint)
4. **Backtracking**: On failure, try next value (board state preserved via recursion)

## Implementation Notes

- **Language**: Elixir (functional language on Erlang VM)
- **Paradigm**: Purely functional with Agent for mutable counter
- **Data Structure**: Tuple of tuples for efficient random access
- **Timing**: Uses `System.monotonic_time/1` for precision timing

### Key Elixir Features Used

```elixir
# Agent for mutable iteration counter
{:ok, counter} = Agent.start_link(fn -> 0 end)
Agent.update(counter, &(&1 + 1))  # INCREMENT EVERY ATTEMPT

# Tuple of tuples for the board (efficient access)
board = {{1,2,3...}, {4,5,6...}, ...}
elem(elem(board, r), c)  # Access cell
put_elem(elem(board, r), c, num)  # Update cell

# Pattern matching for control flow
case find_empty(board) do
  nil -> {:ok, board}
  {r, c} -> try_values(board, r, c, 1, counter)
end
```

### Why Elixir Works Well

Despite being immutable, Elixir performed well because:
1. **Erlang tuple efficiency**: `put_elem` creates shallow copies
2. **Small tuple sizes**: 9-element rows are efficiently copied
3. **Agent process**: Separate process for counter avoids passing state
4. **JIT compilation**: Erlang/OTP 28+ has JIT for performance

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

- Elixir 1.14 or later
- Erlang/OTP 24 or later

### Run Benchmark

```bash
./runMe.sh ../../Matrices/1.matrix
```

### Run All Matrices

```bash
./runMe.sh ../../Matrices/*.matrix
```

### Manual Run

```bash
elixir Sudoku.exs ../../Matrices/1.matrix
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

Seconds to process 0.011
```

## Files

- `Sudoku.exs` - Main solver implementation (Elixir script)
- `runMe.sh` - Benchmark runner script
- `metrics.json` - Benchmark results (auto-generated)
