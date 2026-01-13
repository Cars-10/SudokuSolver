# Phase 12 Plan 1: Erlang Algorithms Summary

**Implemented DLX and CP algorithms in Erlang with verified iteration counts (DLX: 43, CP: 67)**

## Accomplishments

- **DLX Implementation**: Ported Dancing Links algorithm from C to Erlang using ETS table for mutable state
  - Circular doubly-linked lists with tuple-based node structure stored in ETS
  - Column selection heuristic (minimum size) following Knuth's Algorithm X
  - Exact cover matrix with 324 constraint columns for Sudoku
  - Pre-covering of given clues before search
  - Verified with Matrix 1: exactly 43 iterations

- **CP Implementation**: Ported Constraint Propagation algorithm from C to Erlang using mutable arrays
  - Array module for grid state and candidate bitsets
  - Bitset operations (bits 1-9) for efficient candidate tracking using band, bor, bnot, bsl
  - Singleton elimination and hidden singles propagation strategies (rows, columns, boxes)
  - MRV (Minimum Remaining Values) heuristic for cell selection
  - Backtracking with functional state passing (arrays are copied on modification)
  - Verified with Matrix 1: exactly 67 iterations

## Files Created/Modified

- `Algorithms/DLX/Erlang/dlx.erl` - DLX solver using ETS table for circular linked lists
- `Algorithms/DLX/Erlang/runMe.sh` - Build script with escript execution
- `Algorithms/CP/Erlang/cp.erl` - CP solver using arrays and bitsets
- `Algorithms/CP/Erlang/runMe.sh` - Build script with escript execution

## Decisions Made

- **ETS table for DLX**: Used ETS (Erlang Term Storage) instead of process dictionary for DLX node storage, providing efficient mutable state management for the complex circular linked list structure. Each node is stored as {NodeId, {Left, Right, Up, Down, Column, Size, RowId, ColId}}.
- **Tuple-based node structure**: Represented DLX nodes as 8-element tuples for compact storage and fast field access via `element/2` and `setelement/3`.
- **Array module for CP**: Used Erlang's array module for grid and candidates, which provides O(1) access and efficient copying for backtracking.
- **Process dictionary for counters**: Used process dictionary for iteration counters (simple integers) while using ETS/arrays for complex data structures.
- **Guard expression fixes**: Replaced guard-incompatible function calls (has_candidate, get_grid) with case expressions to comply with Erlang's guard restrictions.
- **Bitset operations**: Used band, bor, bnot, bsl for bitwise operations on candidate sets, matching C implementation exactly.

## Issues Encountered

- **Initial DLX infinite loop**: First implementation using process dictionary had issues with row ID tracking for clue covering. Fixed by scanning all row_info entries to find matching clues instead of assuming sequential row IDs.
- **Guard expression errors**: Erlang guards only allow certain BIFs, not custom functions. Converted if/guard expressions to case expressions for has_candidate, get_grid calls.
- **Column selection logic**: Initially returned `none` when scanning completed, which should return the best column found. Fixed to return `Best` instead of `none` when scan reaches root again.

## Next Step

Ready for parallel execution with other Phase 12 plans. Erlang algorithms complete and verified with exact iteration counts matching reference implementations.
