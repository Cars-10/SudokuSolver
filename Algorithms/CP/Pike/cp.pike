#!/usr/bin/env pike
/*
 * Constraint Propagation (CP) Sudoku Solver - Pike Implementation
 * Mechanical translation from Python reference to preserve algorithm correctness.
 *
 * Algorithm: Constraint propagation with MRV (Minimum Remaining Values) heuristic
 * - Uses bitsets to track candidate values (bits 1-9)
 * - Propagates constraints: singleton elimination, hidden singles
 * - Search with MRV cell selection for efficiency
 */

// Global iteration counter
int cp_iterations = 0;

// Grid structure with assigned values and candidate tracking
class CPGrid {
    array(array(int)) values;      // Assigned values (0 = empty)
    array(array(int)) candidates;  // Possible values per cell (bitset)

    void create() {
        values = allocate(9);
        candidates = allocate(9);
        for (int i = 0; i < 9; i++) {
            values[i] = allocate(9);
            candidates[i] = allocate(9);
        }
    }

    // Deep copy method
    CPGrid copy() {
        CPGrid grid_copy = CPGrid();
        for (int r = 0; r < 9; r++) {
            grid_copy->values[r] = values[r][..];
            grid_copy->candidates[r] = candidates[r][..];
        }
        return grid_copy;
    }
}

// Check if digit is a candidate in the bitset
int has_candidate(int candidate_set, int digit) {
    return (candidate_set & (1 << digit)) != 0;
}

// Add digit to the candidate bitset
int add_candidate(int candidate_set, int digit) {
    return candidate_set | (1 << digit);
}

// Remove digit from the candidate bitset
int remove_candidate(int candidate_set, int digit) {
    return candidate_set & ~(1 << digit);
}

// Count number of candidates in a bitset
int count_candidates(int candidate_set) {
    int count = 0;
    for (int digit = 1; digit <= 9; digit++) {
        if (has_candidate(candidate_set, digit)) {
            count++;
        }
    }
    return count;
}

// Get first candidate digit from bitset (1-9)
int get_first_candidate(int candidate_set) {
    for (int digit = 1; digit <= 9; digit++) {
        if (has_candidate(candidate_set, digit)) {
            return digit;
        }
    }
    return 0;
}

// Get all 20 peers for a cell (row, col, box) - returns array of [r,c] arrays
array(array(int)) get_peers(int row, int col) {
    array(array(int)) peers = ({ });

    // Same row (9 cells minus self = 8)
    for (int c = 0; c < 9; c++) {
        if (c != col) {
            peers += ({ ({ row, c }) });
        }
    }

    // Same column (9 cells minus self = 8)
    for (int r = 0; r < 9; r++) {
        if (r != row) {
            peers += ({ ({ r, col }) });
        }
    }

    // Same 3x3 box (9 cells minus self minus already counted = 4)
    int box_row = (row / 3) * 3;
    int box_col = (col / 3) * 3;
    for (int r = box_row; r < box_row + 3; r++) {
        for (int c = box_col; c < box_col + 3; c++) {
            if (r != row && c != col) {
                peers += ({ ({ r, c }) });
            }
        }
    }

    return peers;
}

// Initialize grid from puzzle
void init_grid(CPGrid grid, array(array(int)) puzzle) {
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

// Forward declaration
int assign(CPGrid grid, int row, int col, int digit);

// Eliminate digit from candidates at (row, col)
// Returns 0 if contradiction detected, 1 otherwise
int eliminate(CPGrid grid, int row, int col, int digit) {
    // Check if digit is already eliminated
    if (!has_candidate(grid->candidates[row][col], digit)) {
        return 1;  // Already eliminated, no change
    }

    // Remove digit from candidates
    grid->candidates[row][col] = remove_candidate(grid->candidates[row][col], digit);

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

// Assign digit to cell at (row, col)
// Returns 0 if contradiction detected, 1 otherwise
int assign(CPGrid grid, int row, int col, int digit) {
    // Increment iteration counter (this is our benchmark metric)
    cp_iterations++;

    // Set value
    grid->values[row][col] = digit;
    grid->candidates[row][col] = (1 << digit);

    // Eliminate digit from all peers
    array(array(int)) peers = get_peers(row, col);
    foreach (peers, array(int) peer) {
        int peer_row = peer[0];
        int peer_col = peer[1];
        if (!eliminate(grid, peer_row, peer_col, digit)) {
            return 0;  // Contradiction in peer elimination
        }
    }

    return 1;
}

// Apply constraint propagation until quiescence
// Returns 0 if contradiction detected, 1 otherwise
int propagate(CPGrid grid) {
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
                // Check if digit is already assigned in this row
                int already_assigned = 0;
                for (int col = 0; col < 9; col++) {
                    if (grid->values[row][col] == digit) {
                        already_assigned = 1;
                        break;
                    }
                    if (has_candidate(grid->candidates[row][col], digit)) {
                        count++;
                        last_col = col;
                    }
                }

                if (already_assigned) {
                    continue;
                }

                if (count == 1) {
                    if (!assign(grid, row, last_col, digit)) {
                        return 0;
                    }
                    changed = 1;
                } else if (count == 0) {
                    return 0;  // Digit cannot be placed anywhere in row
                }
            }
        }

        // Check columns
        for (int col = 0; col < 9; col++) {
            for (int digit = 1; digit <= 9; digit++) {
                int count = 0;
                int last_row = -1;
                // Check if digit is already assigned in this column
                int already_assigned = 0;
                for (int row = 0; row < 9; row++) {
                    if (grid->values[row][col] == digit) {
                        already_assigned = 1;
                        break;
                    }
                    if (has_candidate(grid->candidates[row][col], digit)) {
                        count++;
                        last_row = row;
                    }
                }

                if (already_assigned) {
                    continue;
                }

                if (count == 1) {
                    if (!assign(grid, last_row, col, digit)) {
                        return 0;
                    }
                    changed = 1;
                } else if (count == 0) {
                    return 0;  // Digit cannot be placed anywhere in column
                }
            }
        }

        // Check boxes
        for (int box = 0; box < 9; box++) {
            int box_row = (box / 3) * 3;
            int box_col = (box % 3) * 3;

            for (int digit = 1; digit <= 9; digit++) {
                int count = 0;
                int last_r = -1;
                int last_c = -1;

                // Check if digit is already assigned in this box
                int already_assigned = 0;
                for (int r = box_row; r < box_row + 3; r++) {
                    for (int c = box_col; c < box_col + 3; c++) {
                        if (grid->values[r][c] == digit) {
                            already_assigned = 1;
                            break;
                        }
                        if (has_candidate(grid->candidates[r][c], digit)) {
                            count++;
                            last_r = r;
                            last_c = c;
                        }
                    }
                    if (already_assigned) {
                        break;
                    }
                }

                if (already_assigned) {
                    continue;
                }

                if (count == 1) {
                    if (!assign(grid, last_r, last_c, digit)) {
                        return 0;
                    }
                    changed = 1;
                } else if (count == 0) {
                    return 0;  // Digit cannot be placed anywhere in box
                }
            }
        }
    }

    return 1;  // Success - reached fixpoint
}

// Find empty cell with Minimum Remaining Values (fewest candidates)
// Returns array [row, col] if found, 0 if no empty cells
array(int)|int find_mrv_cell(CPGrid grid) {
    int min_candidates = 10;  // More than 9, so any cell will be smaller
    int found = 0;
    int mrv_row = -1;
    int mrv_col = -1;

    for (int r = 0; r < 9; r++) {
        for (int c = 0; c < 9; c++) {
            if (grid->values[r][c] == 0) {
                int num_candidates = count_candidates(grid->candidates[r][c]);
                if (num_candidates < min_candidates) {
                    min_candidates = num_candidates;
                    mrv_row = r;
                    mrv_col = c;
                    found = 1;
                }
            }
        }
    }

    if (found) {
        return ({ mrv_row, mrv_col });
    }
    return 0;
}

// Forward declaration
int cp_search(CPGrid grid, array(int) solution);

// Search with constraint propagation
// Returns 1 if solution found, 0 otherwise
int cp_search(CPGrid grid, array(int) solution) {
    // Base case: check if grid is complete
    array(int)|int mrv_cell = find_mrv_cell(grid);
    if (!mrv_cell) {
        // No empty cells - grid is complete, extract solution
        for (int r = 0; r < 9; r++) {
            for (int c = 0; c < 9; c++) {
                solution[r * 9 + c] = grid->values[r][c];
            }
        }
        return 1;
    }

    int mrv_row = mrv_cell[0];
    int mrv_col = mrv_cell[1];

    // Recursive case: try each candidate for the MRV cell
    int candidates = grid->candidates[mrv_row][mrv_col];

    for (int digit = 1; digit <= 9; digit++) {
        if (has_candidate(candidates, digit)) {
            // Save grid state for backtracking
            CPGrid grid_copy = grid->copy();

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
            grid->values = grid_copy->values;
            grid->candidates = grid_copy->candidates;
        }
    }

    // All candidates exhausted - dead end
    return 0;
}

// Print puzzle
void print_puzzle(array(array(int)) puzzle) {
    write("\nPuzzle:\n");
    for (int r = 0; r < 9; r++) {
        string line = "";
        for (int c = 0; c < 9; c++) {
            line += sprintf("%d ", puzzle[r][c]);
        }
        write(line + "\n");
    }
}

// Read matrix file
array(array(int)) read_matrix_file(string filename) {
    array(array(int)) puzzle = allocate(9);
    for (int i = 0; i < 9; i++) {
        puzzle[i] = allocate(9);
    }

    // Normalize path for output (convert absolute to relative)
    string display_path = filename;
    if (has_prefix(filename, "/app/Matrices/")) {
        display_path = "../" + filename[5..];  // Skip "/app/" to get "Matrices/..."
    }
    write(display_path + "\n");

    string content = Stdio.read_file(filename);
    if (!content) {
        throw(({ "Could not read file: " + filename }));
    }

    array(string) lines = content / "\n";
    int line_count = 0;

    foreach (lines, string line) {
        // Skip comments and empty lines
        line = String.trim_all_whites(line);
        if (line == "" || has_prefix(line, "#")) {
            continue;
        }

        // Parse 9 integers from line
        array(string) parts = line / " ";
        array(int) values = ({ });
        foreach (parts, string part) {
            part = String.trim_all_whites(part);
            if (part != "") {
                values += ({ (int)part });
            }
        }

        if (sizeof(values) == 9 && line_count < 9) {
            puzzle[line_count] = values;
            string output_line = "";
            for (int i = 0; i < 9; i++) {
                output_line += sprintf("%d ", values[i]);
            }
            write(output_line + "\n");
            line_count++;
        }
    }

    if (line_count != 9) {
        throw(({ sprintf("Expected 9 lines, got %d", line_count) }));
    }

    return puzzle;
}

// Main program
int main(int argc, array(string) argv) {
    int start = gethrtime();

    if (argc != 2) {
        werror("Usage: %s <matrix_file>\n", argv[0]);
        return 1;
    }

    // Read puzzle from file
    array(array(int)) puzzle;
    mixed err = catch {
        puzzle = read_matrix_file(argv[1]);
        print_puzzle(puzzle);

        // Initialize CP grid
        CPGrid grid = CPGrid();
        init_grid(grid, puzzle);

        // Apply initial propagation
        cp_iterations = 0;
        if (!propagate(grid)) {
            write("\nNo solution found (contradiction during initial propagation)\n\n");
            float elapsed = (gethrtime() - start) / 1e9;
            write(sprintf("Seconds to process %.3f\n", elapsed));
            return 0;
        }

        // Run search
        array(int) solution = allocate(81);
        int solved = cp_search(grid, solution);

        if (solved) {
            // Convert solution array back to 2D for printing
            array(array(int)) solution_grid = allocate(9);
            for (int i = 0; i < 9; i++) {
                solution_grid[i] = allocate(9);
            }
            for (int r = 0; r < 9; r++) {
                for (int c = 0; c < 9; c++) {
                    solution_grid[r][c] = solution[r * 9 + c];
                }
            }

            print_puzzle(solution_grid);
            write(sprintf("\nSolved in Iterations=%d\n\n", cp_iterations));
        } else {
            write("\nNo solution found\n\n");
        }
    };

    if (err) {
        werror("Error: %s\n", describe_error(err));
        return 1;
    }

    float elapsed = (gethrtime() - start) / 1e9;
    write(sprintf("Seconds to process %.3f\n", elapsed));

    return 0;
}
