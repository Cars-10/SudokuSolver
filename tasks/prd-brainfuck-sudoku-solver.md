# PRD: Brainfuck Sudoku Solver

## Introduction

Implement a Sudoku solver in Brainfuck, one of the most constrained programming languages in existence. Brainfuck has only 8 operators (`>` `<` `+` `-` `.` `,` `[` `]`) operating on a 1D tape of cells with a single pointer. This implementation takes on the persona of an "entity living on a 1D tape" that must solve a 9x9 grid while only seeing one cell at a time.

**Philosophy:** Derive the backtracking algorithm from first principles using only local tape movement - no global variables, no random access. The entity navigates by counting steps, leaving breadcrumbs, and feeling its way through the constraint space.

**Special Concession:** Unlike other language implementations, this solver only needs to output the solved puzzle. Iteration counting is not required due to the extreme constraints of the language.

## Goals

- Create a working Brainfuck Sudoku solver for at least matrix 1 (simplest puzzle)
- Document the tape memory layout extensively for educational value
- Derive backtracking algorithm from the "entity on a tape" perspective
- Use `bfc` compiler for best performance
- Provide clear comments explaining the tape-based reasoning

## The Entity's Worldview

*"I am an entity living on a 1D tape. I can only see the cell under my pointer. I can move left or right, increment or decrement the current cell, and loop while non-zero. I must solve a 9x9 grid that exists somewhere on my tape, but I experience it as a linear sequence of 81 cells. I leave markers to remember where I've been and what I've tried."*

### The 8 Operators (Entity's Abilities)

| Op | Meaning | Entity Perspective |
|----|---------|-------------------|
| `>` | Move pointer right | "I take one step forward" |
| `<` | Move pointer left | "I take one step backward" |
| `+` | Increment cell | "I add one to what I'm touching" |
| `-` | Decrement cell | "I subtract one from what I'm touching" |
| `.` | Output cell as ASCII | "I speak what I'm touching" |
| `,` | Input to cell | "I listen and remember" |
| `[` | Jump past `]` if zero | "If nothing here, skip ahead" |
| `]` | Jump back to `[` if non-zero | "If something here, go back" |

## User Stories

### US-001: Design Tape Memory Layout
**Description:** As a developer, I need to design how the 9x9 grid and working memory map onto the 1D tape so the entity can navigate consistently.

**Acceptance Criteria:**
- [ ] Document tape layout in ASCII diagram format
- [ ] Grid cells occupy positions 0-80 (81 cells for 9x9)
- [ ] Working memory region defined after the grid
- [ ] Each grid cell needs space for: value (0-9), original flag (fixed vs empty)
- [ ] Navigation markers/scratch space defined
- [ ] Layout documented in README.md and as comments in .bf file

**Entity Reasoning:**
*"My world is a tape. I need to know: where does my grid live? Where can I do scratch work? If I walk 81 steps from home, I've crossed my entire puzzle."*

---

### US-002: Implement Grid Initialization (Hardcoded)
**Description:** As a developer, I need to hardcode the puzzle into the Brainfuck source so the entity starts with the initial puzzle state on its tape.

**Acceptance Criteria:**
- [ ] Matrix 1 puzzle hardcoded into tape initialization
- [ ] Empty cells represented as 0
- [ ] Given cells represented as their digit (1-9)
- [ ] Original/fixed cell markers set for given digits
- [ ] Initialization code clearly commented

**Puzzle to Hardcode (Matrix 1):**
```
9 2 0 0 0 0 5 8 4
0 0 0 5 0 0 0 0 3
0 8 3 0 9 2 0 0 0
2 6 0 8 5 4 0 0 1
0 0 5 3 6 1 0 9 0
1 0 0 0 0 9 0 0 0
8 5 0 2 0 3 0 1 0
4 1 2 9 8 0 0 3 0
3 9 0 0 0 6 8 0 0
```

**Entity Reasoning:**
*"Before I begin my journey, the universe (the code) places stones on my tape. Some cells have numbers carved in stone (fixed), others are blank clay (empty) that I can write on."*

---

### US-003: Implement Row Constraint Check
**Description:** As a developer, I need to implement checking if a value already exists in the current row.

**Acceptance Criteria:**
- [ ] Given current position, can check all 9 cells in the same row
- [ ] Returns "valid" (can place) or "invalid" (conflict exists)
- [ ] Works by walking the tape to each row cell and comparing
- [ ] Position restored after check
- [ ] Heavily commented with entity perspective

**Entity Reasoning:**
*"To check my row, I must know which row I'm in. If I'm at position P, my row starts at (P / 9) * 9. I walk to the row's start, check each of 9 cells, then return home. I leave a breadcrumb so I can find my way back."*

---

### US-004: Implement Column Constraint Check
**Description:** As a developer, I need to implement checking if a value already exists in the current column.

**Acceptance Criteria:**
- [ ] Given current position, can check all 9 cells in the same column
- [ ] Returns "valid" or "invalid"
- [ ] Column cells are 9 steps apart on the tape
- [ ] Position restored after check
- [ ] Heavily commented with entity perspective

**Entity Reasoning:**
*"My column repeats every 9 steps. If I'm at position P, my column includes P, P+9, P+18... I hop in strides of 9, checking each landing spot, then hop back home."*

---

### US-005: Implement 3x3 Box Constraint Check
**Description:** As a developer, I need to implement checking if a value already exists in the current 3x3 box.

**Acceptance Criteria:**
- [ ] Given current position, can check all 9 cells in the same 3x3 box
- [ ] Correctly identifies box boundaries
- [ ] Returns "valid" or "invalid"
- [ ] Position restored after check
- [ ] Heavily commented with entity perspective

**Entity Reasoning:**
*"The 3x3 box is trickiest. I must find the top-left corner of my box. Row-of-box = (my_row / 3) * 3. Col-of-box = (my_col / 3) * 3. Then I check a 3x3 region: 3 cells, skip 6, 3 cells, skip 6, 3 cells."*

---

### US-006: Implement Combined isValid Check
**Description:** As a developer, I need to combine row, column, and box checks into a single validity function.

**Acceptance Criteria:**
- [ ] Calls row check, column check, and box check
- [ ] Returns valid only if ALL three pass
- [ ] Efficient - can short-circuit on first failure
- [ ] Clear documentation of the check sequence

**Entity Reasoning:**
*"A number is safe to place only if it doesn't appear in my row, my column, OR my box. Three journeys to make, but I can quit early if any journey finds a conflict."*

---

### US-007: Implement Find Next Empty Cell
**Description:** As a developer, I need to implement finding the next empty (0) cell in row-major order.

**Acceptance Criteria:**
- [ ] Scans from current position (or start) for cell with value 0
- [ ] Skips cells marked as "original/fixed"
- [ ] Returns position of next empty cell, or signals "none found" (solved!)
- [ ] Row-major order: left-to-right, top-to-bottom

**Entity Reasoning:**
*"I walk forward, one step at a time, looking for blank clay. When I find a zero that isn't carved in stone, I've found my next task. If I reach the end (position 81) without finding any, the puzzle is solved!"*

---

### US-008: Implement Backtracking Logic
**Description:** As a developer, I need to implement the core backtracking: try values 1-9, recurse, and undo on failure.

**Acceptance Criteria:**
- [ ] At empty cell, try values 1 through 9 in order
- [ ] For each value, check validity before placing
- [ ] If valid, place value and recurse to next empty cell
- [ ] If recursion fails, undo (set back to 0) and try next value
- [ ] If all 9 values fail, backtrack to previous cell
- [ ] Success when no empty cells remain

**Entity Reasoning:**
*"This is my dance:*
1. *Find blank clay*
2. *Try writing 1. Does it conflict? Try 2. Try 3... up to 9.*
3. *When a number fits, walk forward to find the next blank.*
4. *If I hit a dead end (nothing fits), erase my last mark and retreat.*
5. *I spiral deeper and shallower until the puzzle yields or I exhaust all paths."*

---

### US-009: Implement Output of Solved Puzzle
**Description:** As a developer, I need to output the solved grid in the expected format.

**Acceptance Criteria:**
- [ ] Output 9 rows of 9 digits each
- [ ] Digits separated by spaces
- [ ] Newline after each row
- [ ] Matches expected output format (space-separated digits)
- [ ] Output appears after "Puzzle:" header if possible (or simplified format)

**Expected Output Format:**
```
9 2 1 7 3 6 5 8 4
6 4 7 5 1 8 9 2 3
5 8 3 4 9 2 1 6 7
2 6 9 8 5 4 3 7 1
7 3 5 3 6 1 2 9 8
... (etc)
```

**Entity Reasoning:**
*"When no blank clay remains, I have won. I walk my grid from start to end, speaking each number aloud (outputting). I pause between numbers (space) and take a breath between rows (newline)."*

---

### US-010: Create runMe.sh Benchmark Script
**Description:** As a developer, I need to create the standard benchmark runner script.

**Acceptance Criteria:**
- [ ] Follows project's runMe.sh pattern
- [ ] Uses `bfc` to compile Brainfuck to native executable
- [ ] Handles the hardcoded puzzle (no matrix file input needed)
- [ ] Records timing via common.sh
- [ ] Generates metrics.json

**Note:** Since the puzzle is hardcoded, the script may need special handling or documentation explaining this limitation.

---

### US-011: Create README.md Documentation
**Description:** As a developer, I need comprehensive documentation explaining the implementation.

**Acceptance Criteria:**
- [ ] Explains the "entity on a tape" philosophy
- [ ] Contains ASCII diagram of tape memory layout
- [ ] Documents the backtracking algorithm in tape terms
- [ ] Lists prerequisites (`bfc` installation)
- [ ] Notes limitations (hardcoded puzzle, no iteration count)
- [ ] Includes example output

---

### US-012: Create Language Directory Structure
**Description:** As a developer, I need to set up the Brainfuck language directory.

**Acceptance Criteria:**
- [ ] Create `Languages/Brainfuck/` directory
- [ ] Create `Sudoku.bf` - the Brainfuck source
- [ ] Create `runMe.sh` - benchmark runner
- [ ] Create `README.md` - documentation
- [ ] Directory structure matches other language implementations

## Functional Requirements

- FR-1: The solver must output a valid solved Sudoku grid for Matrix 1
- FR-2: The solution must use only the 8 Brainfuck operators
- FR-3: The algorithm must implement backtracking (not brute-force all permutations)
- FR-4: The tape memory layout must be documented with ASCII diagrams
- FR-5: The code must be heavily commented (using Brainfuck comment convention)
- FR-6: The runMe.sh must integrate with the project's common.sh framework
- FR-7: Output format must show the solved 9x9 grid with space-separated digits

## Non-Goals (Out of Scope)

- **Iteration counting** - Not required for Brainfuck implementation
- **File input parsing** - Puzzle is hardcoded; no stdin parsing needed
- **All matrices** - Only Matrix 1 required; others are stretch goals
- **Performance parity** - Expected to be slowest implementation by far
- **Optimization** - Clarity and educational value over speed

## Technical Considerations

### Brainfuck Interpreter/Compiler
- **Use `bfc`** - Compiles Brainfuck to native code for best performance
- Installation: `brew install bfc` (macOS) or build from source
- Alternative: `beef` interpreter if `bfc` unavailable

### Tape Size Considerations
- Default tape is 30,000 cells (sufficient)
- Consider 8-bit vs 32-bit cell size (prefer 32-bit for easier math)

### Comment Convention in Brainfuck
Any character not in `><+-.,[]` is ignored, so comments are free:
```brainfuck
This is a comment
+++++ +++++ Initialize cell to 10
[-]   Clear current cell
```

### Division/Modulo Challenge
Brainfuck has no division. To compute row (pos/9) and column (pos%9):
- Use repeated subtraction
- Or pre-compute positions in initialization

## Design: Tape Memory Layout (Draft)

```
TAPE LAYOUT (Proposed):
======================

Cells 0-161: Grid storage (81 cells x 2 bytes each)
  Each grid cell uses 2 tape cells:
    [value: 0-9] [fixed: 0=empty, 1=given]

  Grid position P maps to tape position P*2

Cells 162-180: Working memory
  [current_pos]     Where am I in the grid?
  [current_value]   What value am I trying?
  [temp1]           Scratch space
  [temp2]           More scratch
  [row_start]       Computed row start
  [col_num]         Computed column number
  ...

NAVIGATION:
  To reach grid cell N: start at 0, move right N*2 times
  To check "is this cell fixed?": move right 1 from value cell
```

## Algorithm: Backtracking from First Principles

*From the entity's perspective:*

```
SOLVE:
  1. Walk forward looking for blank clay (value=0, fixed=0)
  2. If none found → VICTORY! Output grid and halt.
  3. If found → Remember this position (drop a breadcrumb)
  4. Set trial value to 1

TRY_VALUE:
  5. Check: does this value conflict with row/col/box?
  6. If conflict → increment trial value
  7. If trial value > 9 → BACKTRACK
  8. If no conflict → write value, goto SOLVE (recurse)

BACKTRACK:
  9. Erase current cell (set to 0)
  10. Retreat to previous breadcrumb
  11. Read what value was there, increment it
  12. Goto TRY_VALUE
```

## Success Metrics

- Brainfuck solver outputs correct solution for Matrix 1
- Code is readable with extensive comments
- README clearly explains the tape-based approach
- Implementation demonstrates backtracking conceptually
- Serves as educational example of constraint-based thinking

## Open Questions

1. **Cell size:** Should we require 32-bit cells (`bfc --cell-size 32`) or work within 8-bit constraints?
2. **Multiple matrices:** Should we create separate .bf files for each matrix, or is Matrix 1 sufficient?
3. **Timeout handling:** What if the solver doesn't complete within 300s? Document as known limitation?
4. **Alternative interpreters:** Should we provide fallback to `beef` if `bfc` isn't available?

## Appendix: Matrix 1 Solution (Expected Output)

```
9 2 1 7 3 6 5 8 4
6 4 7 5 1 8 9 2 3
5 8 3 4 9 2 1 6 7
2 6 9 8 5 4 3 7 1
4 7 5 3 6 1 2 9 8
1 3 8 6 2 9 4 5 7
8 5 6 2 4 3 7 1 9
4 1 2 9 8 7 6 3 5
3 9 4 1 7 6 8 2 5
```

(Note: Verify this is the actual solution for Matrix 1)
