// CP (Constraint Propagation) Sudoku Solver in Vala
// Bitset-based constraint propagation with backtracking

using GLib;

// Global state (initialized in main)
public int64 cp_iterations = 0;
public int[,] grid_values;
public int[,] grid_candidates;

// Bitwise operations for candidate sets
bool has_candidate(int set, int digit) {
    return (set & (1 << digit)) != 0;
}

int count_candidates(int set) {
    int count = 0;
    for (int digit = 1; digit <= 9; digit++) {
        if (has_candidate(set, digit)) {
            count++;
        }
    }
    return count;
}

int get_first_candidate(int set) {
    for (int digit = 1; digit <= 9; digit++) {
        if (has_candidate(set, digit)) {
            return digit;
        }
    }
    return 0;
}

// Get all 20 peers for a cell (row, col, box)
int[,] get_peers(int row, int col) {
    int[,] peers = new int[20, 2];
    int idx = 0;

    // Same row (9 cells minus self = 8)
    for (int c = 0; c < 9; c++) {
        if (c != col) {
            peers[idx, 0] = row;
            peers[idx, 1] = c;
            idx++;
        }
    }

    // Same column (9 cells minus self = 8)
    for (int r = 0; r < 9; r++) {
        if (r != row) {
            peers[idx, 0] = r;
            peers[idx, 1] = col;
            idx++;
        }
    }

    // Same 3x3 box (9 cells minus self minus already counted = 4)
    int box_row = (row / 3) * 3;
    int box_col = (col / 3) * 3;
    for (int r = box_row; r < box_row + 3; r++) {
        for (int c = box_col; c < box_col + 3; c++) {
            if (r != row && c != col) {
                peers[idx, 0] = r;
                peers[idx, 1] = c;
                idx++;
            }
        }
    }

    return peers;
}

// Initialize grid from puzzle
void init_grid(int[,] puzzle) {
    for (int row = 0; row < 9; row++) {
        for (int col = 0; col < 9; col++) {
            if (puzzle[row, col] == 0) {
                // Empty cell: set all candidates 1-9 (bits 1-9 set)
                grid_values[row, col] = 0;
                grid_candidates[row, col] = 0x3FE;  // Binary: 0011 1111 1110 (bits 1-9)
            } else {
                // Given clue: set single value
                int digit = puzzle[row, col];
                grid_values[row, col] = digit;
                grid_candidates[row, col] = (1 << digit);
            }
        }
    }
}

// Eliminate a digit from a cell's candidates
bool eliminate(int row, int col, int digit) {
    // Check if digit is already eliminated
    if (!has_candidate(grid_candidates[row, col], digit)) {
        return true;  // Already eliminated, no change
    }

    // Remove digit from candidates
    grid_candidates[row, col] &= ~(1 << digit);

    // Check for contradiction (no candidates left)
    int remaining = count_candidates(grid_candidates[row, col]);
    if (remaining == 0) {
        return false;  // Contradiction
    }

    // If only one candidate left, assign it (singleton elimination)
    if (remaining == 1 && grid_values[row, col] == 0) {
        int last_digit = get_first_candidate(grid_candidates[row, col]);
        if (!assign(row, col, last_digit)) {
            return false;  // Assignment caused contradiction
        }
    }

    return true;
}

// Assign a digit to a cell
bool assign(int row, int col, int digit) {
    // Increment iteration counter (this is our benchmark metric)
    cp_iterations++;

    // Set value
    grid_values[row, col] = digit;
    grid_candidates[row, col] = (1 << digit);

    // Eliminate digit from all peers
    int[,] peers = get_peers(row, col);

    for (int i = 0; i < 20; i++) {
        int peer_row = peers[i, 0];
        int peer_col = peers[i, 1];

        if (!eliminate(peer_row, peer_col, digit)) {
            return false;  // Contradiction in peer elimination
        }
    }

    return true;
}

// Propagate constraints
bool propagate() {
    bool changed = true;

    while (changed) {
        changed = false;

        // Strategy 1: Singleton elimination
        // If a cell has only one candidate, assign it
        for (int row = 0; row < 9; row++) {
            for (int col = 0; col < 9; col++) {
                if (grid_values[row, col] == 0) {
                    int num_candidates = count_candidates(grid_candidates[row, col]);
                    if (num_candidates == 0) {
                        return false;  // Contradiction
                    }
                    if (num_candidates == 1) {
                        int digit = get_first_candidate(grid_candidates[row, col]);
                        if (!assign(row, col, digit)) {
                            return false;  // Assignment caused contradiction
                        }
                        changed = true;
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
                    if (grid_values[row, col] == digit) {
                        count = 0;  // Already assigned
                        break;
                    }
                    if (has_candidate(grid_candidates[row, col], digit)) {
                        count++;
                        last_col = col;
                    }
                }
                if (count == 1) {
                    if (!assign(row, last_col, digit)) {
                        return false;
                    }
                    changed = true;
                } else if (count == 0) {
                    // Check if digit is already assigned in this row
                    bool found = false;
                    for (int col = 0; col < 9; col++) {
                        if (grid_values[row, col] == digit) {
                            found = true;
                            break;
                        }
                    }
                    if (!found) {
                        return false;  // Digit cannot be placed anywhere in row
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
                    if (grid_values[row, col] == digit) {
                        count = 0;  // Already assigned
                        break;
                    }
                    if (has_candidate(grid_candidates[row, col], digit)) {
                        count++;
                        last_row = row;
                    }
                }
                if (count == 1) {
                    if (!assign(last_row, col, digit)) {
                        return false;
                    }
                    changed = true;
                } else if (count == 0) {
                    // Check if digit is already assigned in this column
                    bool found = false;
                    for (int row = 0; row < 9; row++) {
                        if (grid_values[row, col] == digit) {
                            found = true;
                            break;
                        }
                    }
                    if (!found) {
                        return false;  // Digit cannot be placed anywhere in column
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
                bool skip = false;

                for (int r = box_row; r < box_row + 3; r++) {
                    for (int c = box_col; c < box_col + 3; c++) {
                        if (grid_values[r, c] == digit) {
                            count = 0;  // Already assigned
                            skip = true;
                            break;
                        }
                        if (has_candidate(grid_candidates[r, c], digit)) {
                            count++;
                            last_r = r;
                            last_c = c;
                        }
                    }
                    if (skip) break;
                }

                if (!skip) {
                    if (count == 1) {
                        if (!assign(last_r, last_c, digit)) {
                            return false;
                        }
                        changed = true;
                    } else if (count == 0) {
                        // Check if digit is already assigned in this box
                        bool found = false;
                        for (int r = box_row; r < box_row + 3; r++) {
                            for (int c = box_col; c < box_col + 3; c++) {
                                if (grid_values[r, c] == digit) {
                                    found = true;
                                    break;
                                }
                            }
                            if (found) break;
                        }
                        if (!found) {
                            return false;  // Digit cannot be placed anywhere in box
                        }
                    }
                }
            }
        }
    }

    return true;  // Success - reached fixpoint
}

// Find cell with minimum remaining values (MRV heuristic)
bool find_mrv_cell(out int best_row, out int best_col) {
    int min_candidates = 10;  // More than 9, so any cell will be smaller
    bool found = false;
    best_row = -1;
    best_col = -1;

    for (int r = 0; r < 9; r++) {
        for (int c = 0; c < 9; c++) {
            if (grid_values[r, c] == 0) {
                int num_candidates = count_candidates(grid_candidates[r, c]);
                if (num_candidates < min_candidates) {
                    min_candidates = num_candidates;
                    best_row = r;
                    best_col = c;
                    found = true;
                }
            }
        }
    }

    return found;  // false if no empty cells (grid complete)
}

// Copy grid state
void copy_grid_state(int[,] values_dest, int[,] candidates_dest) {
    for (int r = 0; r < 9; r++) {
        for (int c = 0; c < 9; c++) {
            values_dest[r, c] = grid_values[r, c];
            candidates_dest[r, c] = grid_candidates[r, c];
        }
    }
}

// Restore grid state
void restore_grid_state(int[,] values_src, int[,] candidates_src) {
    for (int r = 0; r < 9; r++) {
        for (int c = 0; c < 9; c++) {
            grid_values[r, c] = values_src[r, c];
            grid_candidates[r, c] = candidates_src[r, c];
        }
    }
}

// Search with backtracking
bool cp_search() {
    // Base case: check if grid is complete
    int mrv_row, mrv_col;
    if (!find_mrv_cell(out mrv_row, out mrv_col)) {
        // No empty cells - grid is complete
        return true;
    }

    // Recursive case: try each candidate for the MRV cell
    int candidates = grid_candidates[mrv_row, mrv_col];

    for (int digit = 1; digit <= 9; digit++) {
        if (has_candidate(candidates, digit)) {
            // Save grid state for backtracking
            int[,] grid_copy_values = new int[9, 9];
            int[,] grid_copy_candidates = new int[9, 9];
            copy_grid_state(grid_copy_values, grid_copy_candidates);

            // Try assigning this digit
            if (assign(mrv_row, mrv_col, digit)) {
                // Assignment succeeded, propagate constraints
                if (propagate()) {
                    // Propagation succeeded, recurse
                    if (cp_search()) {
                        return true;  // Found solution
                    }
                }
            }

            // Failed - restore grid state and try next candidate
            restore_grid_state(grid_copy_values, grid_copy_candidates);
        }
    }

    // All candidates exhausted - dead end
    return false;
}

// Print puzzle
void print_puzzle(int[,] grid) {
    stdout.printf("\nPuzzle:\n");
    for (int row = 0; row < 9; row++) {
        for (int col = 0; col < 9; col++) {
            stdout.printf("%d ", grid[row, col]);
        }
        stdout.printf("\n");
    }
}

// Read matrix file
bool read_matrix_file(string filename, int[,] puzzle) {
    // Normalize path for output
    if (filename.has_prefix("/app/Matrices/")) {
        string display_path = filename.substring(5);
        stdout.printf("../%s\n", display_path);
    } else {
        stdout.printf("%s\n", filename);
    }

    try {
        var file = File.new_for_path(filename);
        var dis = new DataInputStream(file.read());
        string? line;
        int line_count = 0;

        while ((line = dis.read_line(null)) != null) {
            string trimmed = line.strip();
            if (trimmed.length == 0 || trimmed.has_prefix("#")) {
                continue;
            }

            string[] parts = trimmed.split_set(" \t");
            int[] values = new int[9];
            int val_count = 0;

            foreach (string part in parts) {
                if (part.length > 0 && val_count < 9) {
                    values[val_count] = int.parse(part);
                    val_count++;
                }
            }

            if (val_count >= 9 && line_count < 9) {
                for (int i = 0; i < 9; i++) {
                    puzzle[line_count, i] = values[i];
                    stdout.printf("%d ", values[i]);
                }
                stdout.printf("\n");
                line_count++;
            }

            if (line_count >= 9) break;
        }

        return line_count == 9;
    } catch (Error e) {
        stderr.printf("Error reading file: %s\n", e.message);
        return false;
    }
}

void main(string[] args) {
    // Initialize global arrays
    grid_values = new int[9, 9];
    grid_candidates = new int[9, 9];

    var start_time = GLib.get_monotonic_time();

    for (int i = 1; i < args.length; i++) {
        if (!args[i].has_suffix(".matrix")) {
            continue;
        }

        int[,] puzzle = new int[9, 9];

        if (!read_matrix_file(args[i], puzzle)) {
            stderr.printf("Error reading %s\n", args[i]);
            continue;
        }

        print_puzzle(puzzle);

        // Initialize CP structures
        init_grid(puzzle);

        // Apply initial propagation
        cp_iterations = 0;
        if (propagate() && cp_search()) {
            print_puzzle(grid_values);
            stdout.printf("\nSolved in Iterations=%lld\n\n", cp_iterations);
        } else {
            stdout.printf("No solution found\n");
        }
    }

    var elapsed = (GLib.get_monotonic_time() - start_time) / 1000000.0;
    stdout.printf("Seconds to process %.3f\n", elapsed);
}
