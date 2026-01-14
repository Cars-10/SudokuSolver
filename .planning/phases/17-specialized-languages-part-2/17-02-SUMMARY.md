# Phase 17 Plan 2: Vala Algorithms (DLX + CP) Summary

Implemented DLX and CP algorithms in Vala, leveraging GObject-based class system and native compilation while achieving correct iteration counts.

## Accomplishments

- **DLX Implementation**: Circular doubly-linked list structure using Vala classes with reference semantics, pre-covering of given clues for correct iteration counting (43 iterations)
- **CP Implementation**: Bitset-based constraint propagation with singleton elimination and hidden singles strategies, proper MRV heuristic (67 iterations)
- Both implementations compile to efficient native code via GObject, demonstrating Vala's high-level syntax with C-level performance

## Files Created/Modified

- `Algorithms/DLX/Vala/dlx.vala` - DLX implementation with class-based circular nodes (476 lines)
- `Algorithms/DLX/Vala/runMe.sh` - Build and benchmark script with glib-2.0 and gio-2.0 packages
- `Algorithms/CP/Vala/cp.vala` - CP implementation with bitset candidates (522 lines)
- `Algorithms/CP/Vala/runMe.sh` - Build and benchmark script with glib-2.0 and gio-2.0 packages

## Decisions Made

1. **Global variable initialization**: Moved array initializations from global scope to main() function to comply with Vala's requirement for non-constant field initializers
2. **Pre-covering clues in DLX**: Added `cover_clues()` function to pre-cover constraint columns for given clues before search, matching reference implementation pattern from Swift/Dart
3. **CP iteration counting**: Followed Swift pattern of not manually assigning initial clues; instead relied on `propagate()` to naturally count iterations during constraint propagation
4. **Package dependencies**: Included both glib-2.0 and gio-2.0 packages for file I/O operations

## Issues Encountered

1. **Initial DLX iteration mismatch (82 vs 43)**: Fixed by implementing `cover_clues()` to pre-cover given clue columns before starting DLX search
2. **Initial CP iteration mismatch (42 vs 67)**: Fixed by removing manual initial clue assignment loop; `propagate()` naturally handles initial constraints through the grid initialization
3. **Compilation errors on global arrays**: Resolved by deferring array initialization to main() function entry point

## Next Step

Ready for 17-03-PLAN.md (Wren implementation)
