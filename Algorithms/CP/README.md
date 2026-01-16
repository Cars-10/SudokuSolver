# Constraint Propagation (CP) Sudoku Solvers

This directory contains 52 CP implementations across different programming languages.

## Algorithm

Constraint propagation with MRV (Minimum Remaining Values) heuristic:
- **Bitsets** for candidate tracking (bits 1-9)
- **Singleton elimination**: If a cell has only one candidate, assign it
- **Hidden singles**: If a digit appears in only one cell in a unit, assign it
- **MRV search**: Choose cell with fewest candidates for backtracking

## Completed Implementations

### Working (52 languages)
All original 51 languages plus:
- **Tcl** ✓ (fully tested, all matrices passing)

### In Progress (4 languages - Code complete, debugging needed)
- **BASIC** (FreeBASIC) - `cp.bas` complete, macOS linker needs `-lgcc` fix
- **Rexx** - `cp.rexx` complete, function/procedure scope issues
- **Pike** - `cp.pike` complete, compiles but logic bug (returns no solution)
- **Icon** - `cp.icn` complete, compiles but needs runtime testing

All 4 have working code structure following CP algorithm pattern. Issues are:
- BASIC: FreeBASIC linking on macOS (works in Docker)
- Rexx: Global variable access in procedures
- Pike: Search or propagation logic error
- Icon: Runtime behavior needs validation

### Not Started (2 languages)
- Forth (stack-based, complex)
- Cobol (verbose, slow performance expected)

## Reference Implementation

See `Tcl/cp.tcl` for a clean, working reference implementation (~465 lines).

## Expected Results

| Matrix | Iterations (approx) |
|--------|---------------------|
| 1      | 67                  |
| 2      | 87,180              |
| 3      | 4,241               |
| 4      | 1,787               |
| 5      | 31,430              |
| 6      | timeout/very high   |

## Implementation Pattern

1. **Bitset operations**: has_candidate, remove_candidate, count_candidates
2. **Grid structure**: values[9][9], candidates[9][9]
3. **Core functions**: eliminate(), assign(), propagate()
4. **Search**: find_mrv_cell(), cp_search() with backtracking
5. **State management**: save_state(), restore_state()

## Notes

- CP is much faster than brute-force but slower than DLX on hard puzzles
- Matrix 6 (hardest) often timeouts with CP due to limited propagation
- Iteration counts vary slightly between languages due to implementation details
