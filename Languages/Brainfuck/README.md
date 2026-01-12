# Brainfuck Sudoku Solver

## Overview

This is a Sudoku solver implemented in Brainfuck, the esoteric programming language with only 8 operators operating on a 1-dimensional tape of memory cells.

### The Entity-on-a-Tape Philosophy

Imagine yourself as a small entity living on an infinite tape of cells. Each cell holds a single number (0-255). You can only:
- See the cell you're standing on
- Move left (`<`) or right (`>`) one step at a time
- Increment (`+`) or decrement (`-`) the current cell
- Input (`,`) to the current cell or output (`.`) from it
- Loop (`[...]`) while the current cell is non-zero

You have **no memory except what you write on the tape itself**. The tape IS your memory, your workspace, your everything.

## Algorithm

Standard brute-force backtracking:
1. Find the next empty cell (row-major order)
2. Try values 1-9 in ascending order
3. For each value, check row/column/box constraints
4. If valid, place value and recurse to next empty cell
5. If no value works, backtrack (reset to 0, return to previous cell)
6. When no empty cells remain, puzzle is solved

## Tape Memory Layout

```
+--------+--------+--------+--------+     +--------+--------+
| Cell 0 | Flag 0 | Cell 1 | Flag 1 | ... | Cell80 | Flag80 |  Working Memory...
+--------+--------+--------+--------+     +--------+--------+
   [0]      [1]      [2]      [3]           [160]    [161]      [162] [163] ...
```

### Grid Region (Positions 0-161)

Each Sudoku cell uses 2 tape positions:
- **Even positions** (0, 2, 4, ...): Cell value (0-9, where 0 = empty)
- **Odd positions** (1, 3, 5, ...): Fixed flag (0 = editable, 1 = given clue)

**Cell-to-Tape Mapping:**
| Cell | Row | Col | Tape Position (Value) | Tape Position (Flag) |
|------|-----|-----|----------------------|---------------------|
| 0    | 0   | 0   | 0                    | 1                   |
| 1    | 0   | 1   | 2                    | 3                   |
| ...  | ... | ... | ...                  | ...                 |
| 40   | 4   | 4   | 80                   | 81                  |
| 80   | 8   | 8   | 160                  | 161                 |

Formula: `tape_position = cell_number * 2`

### Working Memory Region (Positions 162+)

```
[162] current_position    - Which cell (0-80) we're examining
[163] trial_value         - Value (1-9) we're currently trying
[164] result_flag         - Result of validity check (0=invalid, 1=valid)
[165] solved_flag         - Whether puzzle is complete (0=no, 1=yes)
[166] temp1               - Scratch space
[167] temp2               - Scratch space
[168] temp3               - Scratch space
[169+] backtrack_stack    - Stack of positions for backtracking
```

*Note: Layout may evolve as implementation progresses.*

## Prerequisites

- A Brainfuck compiler/interpreter. Recommended: `bfc` (compiles to native executable)
  ```bash
  # Install bfc (example for systems with cargo)
  cargo install bfc
  ```

## Limitations

- **Hardcoded puzzle**: Matrix 1 is embedded in the code (no file input in pure Brainfuck)
- **No iteration count**: Brainfuck has no easy way to track/output iteration counts
- **Performance**: Will be significantly slower than compiled languages
- **Timeout risk**: Complex matrices may exceed the 5-minute timeout

## Files

| File              | Description                              |
|-------------------|------------------------------------------|
| `Sudoku.bf`       | The Brainfuck solver implementation      |
| `runMe.sh`        | Benchmark runner script                  |
| `bf_interpreter.c`| Fallback C interpreter for Brainfuck     |
| `README.md`       | This file                                |

## Usage

```bash
cd Languages/Brainfuck
./runMe.sh
```

## Implementation Status

- [ ] US-001: Directory structure (in progress)
- [ ] US-002: Tape memory layout design
- [ ] US-003: Puzzle initialization
- [ ] US-004: Output routine
- [ ] US-005: Cell navigation
- [ ] US-006: Row constraint check
- [ ] US-007: Column constraint check
- [ ] US-008: Box constraint check
- [ ] US-009: Combined validity check
- [ ] US-010: Find next empty cell
- [ ] US-011: Backtracking solve loop
- [ ] US-012: runMe.sh script
- [ ] US-013: Test and verify
- [ ] US-014: Complete documentation
