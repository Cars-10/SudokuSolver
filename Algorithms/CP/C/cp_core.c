#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cp.h"

// ============================================================================
// GLOBAL ITERATION COUNTER
// ============================================================================
long long cp_iterations = 0;

// ============================================================================
// INITIALIZATION
// ============================================================================

void init_grid(CPGrid *grid, int puzzle[9][9]) {
    // TODO: Implement in Plan 2
    // Initialize candidates to all possible (1-9) for empty cells
    // Set single value for given clues
    memset(grid, 0, sizeof(CPGrid));
}

// ============================================================================
// CONSTRAINT PROPAGATION
// ============================================================================

int eliminate(CPGrid *grid, int row, int col, int digit) {
    // TODO: Implement in Plan 2
    // Remove digit from candidates[row][col]
    // Return 0 if contradiction (no candidates left)
    return 1;
}

int assign(CPGrid *grid, int row, int col, int digit) {
    // TODO: Implement in Plan 2
    // Assign value and propagate constraints to peers
    // Return 0 if contradiction occurs during propagation
    return 1;
}

int propagate(CPGrid *grid) {
    // TODO: Implement in Plan 2
    // Apply singleton elimination and hidden singles until fixpoint
    // Return 0 if contradiction detected
    return 1;
}

// ============================================================================
// SEARCH
// ============================================================================

int find_mrv_cell(CPGrid *grid, int *row, int *col) {
    // TODO: Implement in Plan 2
    // Find cell with minimum remaining values (MRV heuristic)
    // Return 0 if grid is complete (no empty cells)
    return 0;
}

int cp_search(CPGrid *grid, int *solution) {
    // TODO: Implement in Plan 2
    // Recursive backtracking with constraint propagation
    // Return 1 if solved, 0 if unsolvable
    return 0;
}
