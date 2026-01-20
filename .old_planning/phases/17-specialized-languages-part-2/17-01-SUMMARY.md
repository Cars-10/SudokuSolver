# Phase 17 Plan 1: V Algorithms (DLX + CP) Summary

Successfully implemented DLX and CP algorithms in V language, demonstrating V's modern systems programming capabilities with struct-based pointers and efficient bitwise operations.

## Accomplishments

- Implemented Dancing Links Algorithm X (DLX) in V with circular doubly-linked node structures
- Implemented Constraint Propagation (CP) solver in V with bitset-based candidate tracking
- Both implementations verified with correct iteration counts (DLX: 43, CP: 67 for Matrix 1)
- Leveraged V's struct pointers for mutable circular linked structures
- Used V's global variables with -enable-globals flag for iteration counting
- Handled V's fixed-size array constraints vs dynamic arrays

## Files Created/Modified

- `Algorithms/DLX/V/dlx.v` - DLX implementation with struct-based circular nodes (460 lines)
  - Struct DlxNode with mutable reference fields
  - cover_column/uncover_column operations for Algorithm X
  - Knuth's S heuristic (minimum column size selection)
  - Pre-cover given clues before search
  - Exact cover matrix with 324 columns
- `Algorithms/DLX/V/runMe.sh` - Build and benchmark script with -enable-globals flag
- `Algorithms/CP/V/cp.v` - CP implementation with bitset candidates (460 lines)
  - Bitset-based candidate tracking using int (bits 1-9)
  - Singleton elimination and hidden singles detection
  - MRV (Minimum Remaining Values) heuristic
  - Backtracking search with grid state restoration
- `Algorithms/CP/V/runMe.sh` - Build and benchmark script with -enable-globals flag

## Technical Challenges Resolved

1. **Pointer comparisons**: V doesn't support `!voidptr(x) == voidptr(y)` syntax. Changed to `voidptr(x) != voidptr(y)`.

2. **Mutable primitive parameters**: V doesn't allow `mut` on primitive types in function parameters. Changed bitwise helper functions to return new values instead of mutating in-place.

3. **Array initialization**: V requires references in arrays to use `init:` or be inside `unsafe`, or use `cap:` with append. Used `cap:` with `<<` append operator.

4. **Fixed vs dynamic arrays**: V distinguishes between `[9][9]int` (fixed) and `[][]int` (dynamic). Created separate print functions for each type.

5. **Global variables**: V requires `-enable-globals` flag for global variables. Added flag to compilation in runMe.sh.

6. **DLX clue covering**: Initially got 82 iterations (double expected). Root cause: forgot to pre-cover given clues before search. Added cover_clues() function to mark clues as selected in the exact cover matrix.

## Decisions Made

- Used V's struct with reference fields (`&DlxNode`) for circular linked structures in DLX
- Used fixed-size arrays `[9][9]int` for grid values in CP struct
- Used global variables for iteration counters (matches pattern from other languages)
- Pre-covered clues in DLX before search (following Dart/Swift pattern)
- Used MRV heuristic in CP for efficient search ordering

## Performance Notes

- DLX V: 314ms for Matrix 1 (43 iterations)
- CP V: 319ms for Matrix 1 (67 iterations)
- Both implementations compile with `-prod` flag for optimized builds
- Performance comparable to other modern compiled languages

## Next Step

Ready for 17-02-PLAN.md (Vala implementation of DLX and CP algorithms)
