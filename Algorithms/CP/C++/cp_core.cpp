#include <cstdio>
#include <cstdlib>
#include <cstring>
#include "cp.h"

// ============================================================================
// GLOBAL ITERATION COUNTER
// ============================================================================
long long cp_iterations = 0;

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

// Count number of candidates in a bitset
int count_candidates(CandidateSet cs) {
    return COUNT_CANDIDATES(cs);
}

// Get first candidate digit from bitset (1-9)
int get_first_candidate(CandidateSet cs) {
    for (int digit = 1; digit <= 9; digit++) {
        if (HAS_CANDIDATE(cs, digit)) {
            return digit;
        }
    }
    return 0;
}

// Get all 20 peers for a cell (row, col, box) - returns coordinates interleaved
void get_peers(int row, int col, int peers[20][2]) {
    int idx = 0;

    // Same row (9 cells minus self = 8)
    for (int c = 0; c < 9; c++) {
        if (c != col) {
            peers[idx][0] = row;
            peers[idx][1] = c;
            idx++;
        }
    }

    // Same column (9 cells minus self = 8)
    for (int r = 0; r < 9; r++) {
        if (r != row) {
            peers[idx][0] = r;
            peers[idx][1] = col;
            idx++;
        }
    }

    // Same 3x3 box (9 cells minus self minus already counted = 4)
    int box_row = (row / 3) * 3;
    int box_col = (col / 3) * 3;
    for (int r = box_row; r < box_row + 3; r++) {
        for (int c = box_col; c < box_col + 3; c++) {
            if (r != row && c != col) {
                peers[idx][0] = r;
                peers[idx][1] = c;
                idx++;
            }
        }
    }
}

// ============================================================================
// INITIALIZATION
// ============================================================================

void init_grid(CPGrid *grid, int puzzle[9][9]) {
    for (int row = 0; row < 9; row++) {
        for (int col = 0; col < 9; col++) {
            if (puzzle[row][col] == 0) {
                // Empty cell: set all candidates 1-9 (bits 1-9 set)
                grid->values[row][col] = 0;
                grid->candidates[row][col] = 0x3FE;  // Binary: 0011 1111 1110 (bits 1-9)
            } else {
                // Given clue: set single value
                int digit = puzzle[row][col];
                grid->values[row][col] = digit;
                grid->candidates[row][col] = (1 << digit);
            }
        }
    }
}

// ============================================================================
// CONSTRAINT PROPAGATION
// ============================================================================

int eliminate(CPGrid *grid, int row, int col, int digit) {
    // Check if digit is already eliminated
    if (!HAS_CANDIDATE(grid->candidates[row][col], digit)) {
        return 1;  // Already eliminated, no change
    }

    // Remove digit from candidates
    REMOVE_CANDIDATE(grid->candidates[row][col], digit);

    // Check for contradiction (no candidates left)
    int remaining = count_candidates(grid->candidates[row][col]);
    if (remaining == 0) {
        return 0;  // Contradiction
    }

    // If only one candidate left, assign it (singleton elimination)
    if (remaining == 1 && grid->values[row][col] == 0) {
        int last_digit = get_first_candidate(grid->candidates[row][col]);
        if (!assign(grid, row, col, last_digit)) {
            return 0;  // Assignment caused contradiction
        }
    }

    return 1;
}

int assign(CPGrid *grid, int row, int col, int digit) {
    // Increment iteration counter (this is our benchmark metric)
    cp_iterations++;

    // Set value
    grid->values[row][col] = digit;
    grid->candidates[row][col] = (1 << digit);

    // Eliminate digit from all peers
    int peers[20][2];
    get_peers(row, col, peers);

    for (int i = 0; i < 20; i++) {
        int peer_row = peers[i][0];
        int peer_col = peers[i][1];

        if (!eliminate(grid, peer_row, peer_col, digit)) {
            return 0;  // Contradiction in peer elimination
        }
    }

    return 1;
}

int propagate(CPGrid *grid) {
    int changed = 1;

    while (changed) {
        changed = 0;

        // Strategy 1: Singleton elimination
        // If a cell has only one candidate, assign it
        for (int row = 0; row < 9; row++) {
            for (int col = 0; col < 9; col++) {
                if (grid->values[row][col] == 0) {
                    int num_candidates = count_candidates(grid->candidates[row][col]);
                    if (num_candidates == 0) {
                        return 0;  // Contradiction
                    }
                    if (num_candidates == 1) {
                        int digit = get_first_candidate(grid->candidates[row][col]);
                        if (!assign(grid, row, col, digit)) {
                            return 0;  // Assignment caused contradiction
                        }
                        changed = 1;
                    }
                }
            }
        }

        // Strategy 2: Hidden singles
        // For each unit (row, col, box), if a digit appears in only one cell, assign it

        // Check rows
        for (int row = 0; row < 9; row++) {
            for (int digit = 1; digit <= 9; digit++) {
                int count = 0;
                int last_col = -1;
                for (int col = 0; col < 9; col++) {
                    if (grid->values[row][col] == digit) {
                        count = 0;  // Already assigned
                        break;
                    }
                    if (HAS_CANDIDATE(grid->candidates[row][col], digit)) {
                        count++;
                        last_col = col;
                    }
                }
                if (count == 1) {
                    if (!assign(grid, row, last_col, digit)) {
                        return 0;
                    }
                    changed = 1;
                } else if (count == 0) {
                    // Check if digit is already assigned in this row
                    int found = 0;
                    for (int col = 0; col < 9; col++) {
                        if (grid->values[row][col] == digit) {
                            found = 1;
                            break;
                        }
                    }
                    if (!found) {
                        return 0;  // Digit cannot be placed anywhere in row
                    }
                }
            }
        }

        // Check columns
        for (int col = 0; col < 9; col++) {
            for (int digit = 1; digit <= 9; digit++) {
                int count = 0;
                int last_row = -1;
                for (int row = 0; row < 9; row++) {
                    if (grid->values[row][col] == digit) {
                        count = 0;  // Already assigned
                        break;
                    }
                    if (HAS_CANDIDATE(grid->candidates[row][col], digit)) {
                        count++;
                        last_row = row;
                    }
                }
                if (count == 1) {
                    if (!assign(grid, last_row, col, digit)) {
                        return 0;
                    }
                    changed = 1;
                } else if (count == 0) {
                    // Check if digit is already assigned in this column
                    int found = 0;
                    for (int row = 0; row < 9; row++) {
                        if (grid->values[row][col] == digit) {
                            found = 1;
                            break;
                        }
                    }
                    if (!found) {
                        return 0;  // Digit cannot be placed anywhere in column
                    }
                }
            }
        }

        // Check boxes
        for (int box = 0; box < 9; box++) {
            int box_row = (box / 3) * 3;
            int box_col = (box % 3) * 3;

            for (int digit = 1; digit <= 9; digit++) {
                int count = 0;
                int last_r = -1, last_c = -1;

                for (int r = box_row; r < box_row + 3; r++) {
                    for (int c = box_col; c < box_col + 3; c++) {
                        if (grid->values[r][c] == digit) {
                            count = 0;  // Already assigned
                            goto next_box_digit;
                        }
                        if (HAS_CANDIDATE(grid->candidates[r][c], digit)) {
                            count++;
                            last_r = r;
                            last_c = c;
                        }
                    }
                }

                if (count == 1) {
                    if (!assign(grid, last_r, last_c, digit)) {
                        return 0;
                    }
                    changed = 1;
                } else if (count == 0) {
                    // Check if digit is already assigned in this box
                    int found = 0;
                    for (int r = box_row; r < box_row + 3; r++) {
                        for (int c = box_col; c < box_col + 3; c++) {
                            if (grid->values[r][c] == digit) {
                                found = 1;
                                goto found_box_digit;
                            }
                        }
                    }
                    found_box_digit:
                    if (!found) {
                        return 0;  // Digit cannot be placed anywhere in box
                    }
                }

                next_box_digit:
                continue;
            }
        }
    }

    return 1;  // Success - reached fixpoint
}

// ============================================================================
// SEARCH
// ============================================================================

int find_mrv_cell(CPGrid *grid, int *row, int *col) {
    int min_candidates = 10;  // More than 9, so any cell will be smaller
    int found = 0;

    for (int r = 0; r < 9; r++) {
        for (int c = 0; c < 9; c++) {
            if (grid->values[r][c] == 0) {
                int num_candidates = count_candidates(grid->candidates[r][c]);
                if (num_candidates < min_candidates) {
                    min_candidates = num_candidates;
                    *row = r;
                    *col = c;
                    found = 1;
                }
            }
        }
    }

    return found;  // 0 if no empty cells (grid complete), 1 if cell found
}

int cp_search(CPGrid *grid, int *solution) {
    // Base case: check if grid is complete
    int mrv_row, mrv_col;
    if (!find_mrv_cell(grid, &mrv_row, &mrv_col)) {
        // No empty cells - grid is complete, extract solution
        for (int r = 0; r < 9; r++) {
            for (int c = 0; c < 9; c++) {
                solution[r * 9 + c] = grid->values[r][c];
            }
        }
        return 1;  // Solved
    }

    // Recursive case: try each candidate for the MRV cell
    CandidateSet candidates = grid->candidates[mrv_row][mrv_col];

    for (int digit = 1; digit <= 9; digit++) {
        if (HAS_CANDIDATE(candidates, digit)) {
            // Save grid state for backtracking
            CPGrid grid_copy;
            std::memcpy(&grid_copy, grid, sizeof(CPGrid));

            // Try assigning this digit
            if (assign(grid, mrv_row, mrv_col, digit)) {
                // Assignment succeeded, propagate constraints
                if (propagate(grid)) {
                    // Propagation succeeded, recurse
                    if (cp_search(grid, solution)) {
                        return 1;  // Found solution
                    }
                }
            }

            // Failed - restore grid state and try next candidate
            std::memcpy(grid, &grid_copy, sizeof(CPGrid));
        }
    }

    // All candidates exhausted - dead end
    return 0;
}
