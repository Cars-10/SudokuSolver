# Phase 17 Plan 3: Wren Algorithms (DLX + CP) Summary

Extended Wren language support with DLX and CP algorithm implementations, demonstrating Wren's lightweight class-based design and efficient bitwise operations for advanced Sudoku solving techniques.

## Accomplishments

- Implemented Dancing Links (DLX) algorithm in Wren with class-based circular linked lists
- Implemented Constraint Propagation (CP) algorithm in Wren with bitset candidate tracking
- Both implementations verified with correct iteration counts (DLX: 43, CP: 67 on Matrix 1)
- Leveraged Wren's reference semantics for mutable node structures in DLX
- Utilized Wren's bitwise operators for efficient candidate management in CP

## Files Created/Modified

- `Algorithms/DLX/Wren/dlx.wren` - DLX implementation with DlxNode class and circular doubly-linked lists
- `Algorithms/DLX/Wren/runMe.sh` - DLX benchmark script
- `Algorithms/DLX/Wren/metrics.json` - DLX benchmark results
- `Algorithms/CP/Wren/cp.wren` - CP implementation with bitset-based constraint propagation
- `Algorithms/CP/Wren/runMe.sh` - CP benchmark script
- `Algorithms/CP/Wren/metrics.json` - CP benchmark results

## Decisions Made

**DLX Implementation:**
- Used class-based node structure with instance variables for circular linked list
- Implemented clue pre-covering strategy (build rows for clues, then cover them before search)
- Applied Knuth's S heuristic for column selection (minimum size)
- Used Map/List structures for row tracking (_rowStarts, _rowInfo)

**CP Implementation:**
- Used bitset representation (0x3FE) for candidate tracking (bits 1-9)
- Implemented both singleton elimination and hidden singles strategies
- Applied MRV (Minimum Remaining Values) heuristic for cell selection
- Used deep copy for grid state to enable backtracking

**Language-specific patterns:**
- Wren's class syntax: `construct new()` for constructors
- Property accessors: `property { _property }` and `property=(value) { _property = value }`
- Bitwise operations: `&`, `|`, `~`, `<<` for candidate manipulation
- List operations: `List.filled()`, `.add()`, `.removeAt()` for dynamic collections

## Issues Encountered

**Issue 1: Initial iteration count mismatch (82 vs 43 expected)**
- Problem: DLX was generating double the expected iterations
- Root cause: Missing clue pre-covering step - was skipping clue rows instead of covering them
- Resolution: Added `coverClues()` method to cover constraint columns for pre-filled cells after matrix build

**Issue 2: runMe.sh not parsing iterations correctly**
- Problem: Metrics showed 0 iterations despite correct output
- Root cause: Used `run_solver()` function instead of setting SOLVER_BINARY correctly
- Resolution: Set `SOLVER_BINARY="wren_cli dlx.wren"` (full command) and removed custom run_solver function

**Issue 3: DLX solution showing garbage values**
- Problem: Solution grid contained incorrect digits
- Root cause: Converting rowId to grid position arithmetically instead of using stored metadata
- Resolution: Used _rowInfo map to properly decode rowId → (row, col, digit) mapping

## Next Step

Ready for 17-04-PLAN.md (Haxe DLX + CP implementation)
