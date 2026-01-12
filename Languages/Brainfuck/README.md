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

The working memory is organized into logical groups for different purposes:

```
CORE STATE (162-165):
+----------+----------+----------+----------+
| curr_pos | trial_v  | result   | solved   |
+----------+----------+----------+----------+
   [162]      [163]      [164]      [165]

SCRATCH SPACE (166-175):
+----------+----------+----------+----------+----------+
|  temp1   |  temp2   |  temp3   |  temp4   |  temp5   |
+----------+----------+----------+----------+----------+
   [166]      [167]      [168]      [169]      [170]

+----------+----------+----------+----------+----------+
|  temp6   |  temp7   |  temp8   |  temp9   |  temp10  |
+----------+----------+----------+----------+----------+
   [171]      [172]      [173]      [174]      [175]

NAVIGATION (176-180):
+----------+----------+----------+----------+----------+
| nav_pos  | nav_cnt  | row_num  | col_num  | box_idx  |
+----------+----------+----------+----------+----------+
   [176]      [177]      [178]      [179]      [180]

ARITHMETIC (181-185):
+----------+----------+----------+----------+----------+
|  div_q   |  div_r   |  mul_a   |  mul_b   |  mul_r   |
+----------+----------+----------+----------+----------+
   [181]      [182]      [183]      [184]      [185]

BACKTRACK STACK (186+):
+----------+----------+----------+----------+
| stack_sp | stack[0] | stack[1] | stack[2] | ...
+----------+----------+----------+----------+
   [186]      [187]      [188]      [189]
```

#### Detailed Working Memory Map

| Position | Name       | Description                                          |
|----------|------------|------------------------------------------------------|
| 162      | curr_pos   | Current cell index (0-80) being examined             |
| 163      | trial_val  | Value (1-9) currently being tested                   |
| 164      | result     | Validity check result: 0=invalid, 1=valid            |
| 165      | solved     | Puzzle complete flag: 0=no, 1=yes                    |
| 166-175  | temp1-10   | General scratch space for intermediate calculations  |
| 176      | nav_pos    | Saved position during navigation                     |
| 177      | nav_cnt    | Counter for navigation steps                         |
| 178      | row_num    | Current row number (0-8) for constraint checking     |
| 179      | col_num    | Current column number (0-8) for constraint checking  |
| 180      | box_idx    | Current box index (0-8) for constraint checking      |
| 181      | div_q      | Division quotient result                             |
| 182      | div_r      | Division remainder result                            |
| 183      | mul_a      | Multiplication operand A                             |
| 184      | mul_b      | Multiplication operand B                             |
| 185      | mul_r      | Multiplication result                                |
| 186      | stack_sp   | Stack pointer (index into backtrack stack)           |
| 187+     | stack[]    | Backtrack stack - stores positions of placed values  |

#### Common Operations Using Working Memory

**Finding Row/Column from Position:**
```
row = curr_pos / 9  (stored in row_num via div_q)
col = curr_pos % 9  (stored in col_num via div_r)
```

**Finding Row Start:**
```
row_start = row * 9  (multiply row_num by 9)
```

**Finding Box Top-Left:**
```
box_row = (row / 3) * 3
box_col = (col / 3) * 3
box_start = box_row * 9 + box_col
```

**Navigation Formula:**
```
tape_pos = cell_index * 2  (for value)
tape_pos = cell_index * 2 + 1  (for fixed flag)
```

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

- [x] US-001: Directory structure
- [x] US-002: Tape memory layout design
- [x] US-003: Puzzle initialization
- [x] US-004: Output routine
- [x] US-005: Cell navigation
- [x] US-006: Row constraint check
- [x] US-007: Column constraint check
- [x] US-008: Box constraint check
- [ ] US-009: Combined validity check
- [ ] US-010: Find next empty cell
- [ ] US-011: Backtracking solve loop
- [ ] US-012: runMe.sh script
- [ ] US-013: Test and verify
- [ ] US-014: Complete documentation
