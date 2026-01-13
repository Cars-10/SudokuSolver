#import "cp.h"
#import <stdio.h>
#import <stdlib.h>
#import <string.h>

// ============================================================================
// GLOBAL ITERATION COUNTER
// ============================================================================
long long cpIterations = 0;

// ============================================================================
// OBJECTIVE-C CLASS IMPLEMENTATION
// ============================================================================

@implementation CPGrid

- (instancetype)init {
    self = [super init];
    if (self) {
        memset(values, 0, sizeof(values));
        memset(candidates, 0, sizeof(candidates));
    }
    return self;
}

- (void)copyFrom:(CPGrid*)other {
    memcpy(values, other->values, sizeof(values));
    memcpy(candidates, other->candidates, sizeof(candidates));
}

@end

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

// Count number of candidates in a bitset
int countCandidates(CandidateSet cs) {
    return COUNT_CANDIDATES(cs);
}

// Get first candidate digit from bitset (1-9)
int getFirstCandidate(CandidateSet cs) {
    for (int digit = 1; digit <= 9; digit++) {
        if (HAS_CANDIDATE(cs, digit)) {
            return digit;
        }
    }
    return 0;
}

// Get all 20 peers for a cell (row, col, box) - returns coordinates interleaved
void getPeers(int row, int col, int peers[20][2]) {
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
    int boxRow = (row / 3) * 3;
    int boxCol = (col / 3) * 3;
    for (int r = boxRow; r < boxRow + 3; r++) {
        for (int c = boxCol; c < boxCol + 3; c++) {
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

void initGrid(CPGrid *grid, int puzzle[9][9]) {
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
    int remaining = countCandidates(grid->candidates[row][col]);
    if (remaining == 0) {
        return 0;  // Contradiction
    }

    // If only one candidate left, assign it (singleton elimination)
    if (remaining == 1 && grid->values[row][col] == 0) {
        int lastDigit = getFirstCandidate(grid->candidates[row][col]);
        if (!assign(grid, row, col, lastDigit)) {
            return 0;  // Assignment caused contradiction
        }
    }

    return 1;
}

int assign(CPGrid *grid, int row, int col, int digit) {
    // Increment iteration counter (this is our benchmark metric)
    cpIterations++;

    // Set value
    grid->values[row][col] = digit;
    grid->candidates[row][col] = (1 << digit);

    // Eliminate digit from all peers
    int peers[20][2];
    getPeers(row, col, peers);

    for (int i = 0; i < 20; i++) {
        int peerRow = peers[i][0];
        int peerCol = peers[i][1];

        if (!eliminate(grid, peerRow, peerCol, digit)) {
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
                    int numCandidates = countCandidates(grid->candidates[row][col]);
                    if (numCandidates == 0) {
                        return 0;  // Contradiction
                    }
                    if (numCandidates == 1) {
                        int digit = getFirstCandidate(grid->candidates[row][col]);
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
                int lastCol = -1;
                for (int col = 0; col < 9; col++) {
                    if (grid->values[row][col] == digit) {
                        count = 0;  // Already assigned
                        break;
                    }
                    if (HAS_CANDIDATE(grid->candidates[row][col], digit)) {
                        count++;
                        lastCol = col;
                    }
                }
                if (count == 1) {
                    if (!assign(grid, row, lastCol, digit)) {
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
                int lastRow = -1;
                for (int row = 0; row < 9; row++) {
                    if (grid->values[row][col] == digit) {
                        count = 0;  // Already assigned
                        break;
                    }
                    if (HAS_CANDIDATE(grid->candidates[row][col], digit)) {
                        count++;
                        lastRow = row;
                    }
                }
                if (count == 1) {
                    if (!assign(grid, lastRow, col, digit)) {
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
            int boxRow = (box / 3) * 3;
            int boxCol = (box % 3) * 3;

            for (int digit = 1; digit <= 9; digit++) {
                int count = 0;
                int lastR = -1, lastC = -1;

                for (int r = boxRow; r < boxRow + 3; r++) {
                    for (int c = boxCol; c < boxCol + 3; c++) {
                        if (grid->values[r][c] == digit) {
                            count = 0;  // Already assigned
                            goto nextBoxDigit;
                        }
                        if (HAS_CANDIDATE(grid->candidates[r][c], digit)) {
                            count++;
                            lastR = r;
                            lastC = c;
                        }
                    }
                }

                if (count == 1) {
                    if (!assign(grid, lastR, lastC, digit)) {
                        return 0;
                    }
                    changed = 1;
                } else if (count == 0) {
                    // Check if digit is already assigned in this box
                    int found = 0;
                    for (int r = boxRow; r < boxRow + 3; r++) {
                        for (int c = boxCol; c < boxCol + 3; c++) {
                            if (grid->values[r][c] == digit) {
                                found = 1;
                                goto foundBoxDigit;
                            }
                        }
                    }
                    foundBoxDigit:
                    if (!found) {
                        return 0;  // Digit cannot be placed anywhere in box
                    }
                }

                nextBoxDigit:
                continue;
            }
        }
    }

    return 1;  // Success - reached fixpoint
}

// ============================================================================
// SEARCH
// ============================================================================

int findMrvCell(CPGrid *grid, int *row, int *col) {
    int minCandidates = 10;  // More than 9, so any cell will be smaller
    int found = 0;

    for (int r = 0; r < 9; r++) {
        for (int c = 0; c < 9; c++) {
            if (grid->values[r][c] == 0) {
                int numCandidates = countCandidates(grid->candidates[r][c]);
                if (numCandidates < minCandidates) {
                    minCandidates = numCandidates;
                    *row = r;
                    *col = c;
                    found = 1;
                }
            }
        }
    }

    return found;  // 0 if no empty cells (grid complete), 1 if cell found
}

int cpSearch(CPGrid *grid, int *solution) {
    // Base case: check if grid is complete
    int mrvRow, mrvCol;
    if (!findMrvCell(grid, &mrvRow, &mrvCol)) {
        // No empty cells - grid is complete, extract solution
        for (int r = 0; r < 9; r++) {
            for (int c = 0; c < 9; c++) {
                solution[r * 9 + c] = grid->values[r][c];
            }
        }
        return 1;  // Solved
    }

    // Recursive case: try each candidate for the MRV cell
    CandidateSet candidates = grid->candidates[mrvRow][mrvCol];

    for (int digit = 1; digit <= 9; digit++) {
        if (HAS_CANDIDATE(candidates, digit)) {
            // Save grid state for backtracking
            CPGrid *gridCopy = [[CPGrid alloc] init];
            [gridCopy copyFrom:grid];

            // Try assigning this digit
            if (assign(grid, mrvRow, mrvCol, digit)) {
                // Assignment succeeded, propagate constraints
                if (propagate(grid)) {
                    // Propagation succeeded, recurse
                    if (cpSearch(grid, solution)) {
                        return 1;  // Found solution
                    }
                }
            }

            // Failed - restore grid state and try next candidate
            [grid copyFrom:gridCopy];
        }
    }

    // All candidates exhausted - dead end
    return 0;
}
