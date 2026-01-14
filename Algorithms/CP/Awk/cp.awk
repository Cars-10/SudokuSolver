#!/usr/bin/awk -f
# Constraint Propagation Sudoku Solver in AWK
# Uses bitsets for candidate tracking and constraint propagation
# Follows C reference exactly for iteration counting

BEGIN {
    # Global iteration counter (incremented only in assign())
    cp_iterations = 0

    # Bitwise constants (AWK doesn't support hex literals, so use decimal)
    FULL_CANDIDATES = 1022  # 0x3FE in hex, binary: 0011 1111 1110 (bits 1-9)

    # Check if AWK supports bitwise operations
    # gawk has and(), or(), etc. - standard AWK may not
}

# ============================================================================
# BITWISE HELPER FUNCTIONS (Manual Implementation)
# ============================================================================

# Manual bitwise operations for standard AWK (without gawk's and/or/xor)
# We'll use powers of 2 and modulo arithmetic

function power_of_2(n,    result, i) {
    # Calculate 2^n
    result = 1
    for (i = 0; i < n; i++) {
        result *= 2
    }
    return result
}

function has_bit(bitset, bit) {
    # Check if bit is set (bit 1-9)
    # bit N means position 1<<N, so we check if (bitset / 2^N) % 2 == 1
    pow = power_of_2(bit)
    return int(bitset / pow) % 2 == 1
}

function set_bit(bitset, bit,    pow) {
    # Set bit (bit 1-9)
    pow = power_of_2(bit)
    if (!has_bit(bitset, bit)) {
        bitset += pow
    }
    return bitset
}

function clear_bit(bitset, bit,    pow) {
    # Clear bit (bit 1-9)
    pow = power_of_2(bit)
    if (has_bit(bitset, bit)) {
        bitset -= pow
    }
    return bitset
}

function count_bits(bitset,    count, bit) {
    # Count number of set bits
    count = 0
    for (bit = 1; bit <= 9; bit++) {
        if (has_bit(bitset, bit)) {
            count++
        }
    }
    return count
}

function get_first_bit(bitset,    bit) {
    # Get first set bit (1-9), or 0 if none
    for (bit = 1; bit <= 9; bit++) {
        if (has_bit(bitset, bit)) {
            return bit
        }
    }
    return 0
}

# ============================================================================
# PEER CALCULATION
# ============================================================================

function get_peers(row, col,    r, c, box_row, box_col, idx) {
    # Calculate 20 peers for a cell (same row, col, box)
    # Store as flat array: peers[0] = row0, peers[1] = col0, etc.
    idx = 0

    # Same row (8 cells)
    for (c = 0; c < 9; c++) {
        if (c != col) {
            peers[idx++] = row
            peers[idx++] = c
        }
    }

    # Same column (8 cells)
    for (r = 0; r < 9; r++) {
        if (r != row) {
            peers[idx++] = r
            peers[idx++] = col
        }
    }

    # Same 3x3 box (4 cells not already counted)
    box_row = int(row / 3) * 3
    box_col = int(col / 3) * 3
    for (r = box_row; r < box_row + 3; r++) {
        for (c = box_col; c < box_col + 3; c++) {
            if (r != row && c != col) {
                peers[idx++] = r
                peers[idx++] = c
            }
        }
    }

    return idx  # Total count (should be 40: 20 pairs)
}

# ============================================================================
# CONSTRAINT PROPAGATION
# ============================================================================

function eliminate(row, col, digit,    remaining, last_digit) {
    # If cell already has a value, don't eliminate from it
    # (This shouldn't happen in correct propagation, but adding as safety check)
    if (grid_values[row, col] != 0) {
        # If trying to eliminate the cell's own value, that's a contradiction
        if (grid_values[row, col] == digit) {
            return 0  # Contradiction: trying to eliminate a cell's assigned value
        }
        # Otherwise, digit is not the cell's value, so "elimination" is a no-op
        return 1
    }

    # Check if digit is already eliminated
    if (!has_bit(grid_candidates[row, col], digit)) {
        return 1  # Already eliminated
    }

    # Remove digit from candidates
    grid_candidates[row, col] = clear_bit(grid_candidates[row, col], digit)

    # Check for contradiction
    remaining = count_bits(grid_candidates[row, col])
    if (remaining == 0) {
        return 0  # Contradiction
    }

    # Singleton elimination: if only one candidate left, assign it
    if (remaining == 1) {
        last_digit = get_first_bit(grid_candidates[row, col])
        if (!assign(row, col, last_digit)) {
            return 0
        }
    }

    return 1
}

function assign(row, col, digit,    peer_count, i, peer_row, peer_col) {
    # Increment iteration counter (benchmark metric)
    cp_iterations++


    # Set value
    grid_values[row, col] = digit
    grid_candidates[row, col] = power_of_2(digit)

    # Eliminate digit from all peers
    peer_count = get_peers(row, col)

    for (i = 0; i < peer_count; i += 2) {
        peer_row = peers[i]
        peer_col = peers[i + 1]

        if (!eliminate(peer_row, peer_col, digit)) {
            return 0  # Contradiction
        }
    }

    return 1
}

function propagate(    changed, row, col, num_candidates, digit, count, last_row, last_col, box, box_row, box_col, r, c, found) {
    # Propagate constraints until fixpoint
    changed = 1

    while (changed) {
        changed = 0

        # Strategy 1: Singleton elimination
        # ONLY process cells that don't have values yet
        for (row = 0; row < 9; row++) {
            for (col = 0; col < 9; col++) {
                if (grid_values[row, col] == 0) {
                    num_candidates = count_bits(grid_candidates[row, col])
                    if (num_candidates == 0) {
                        return 0  # Contradiction
                    }
                    if (num_candidates == 1) {
                        digit = get_first_bit(grid_candidates[row, col])
                        if (!assign(row, col, digit)) {
                            return 0
                        }
                        changed = 1
                    }
                }
            }
        }

        # Strategy 2: Hidden singles in rows
        for (row = 0; row < 9; row++) {
            for (digit = 1; digit <= 9; digit++) {
                # First check if digit is already assigned in this row
                already_assigned = 0
                for (col = 0; col < 9; col++) {
                    if (grid_values[row, col] == digit) {
                        already_assigned = 1
                        break
                    }
                }
                if (already_assigned) {
                    continue  # Skip to next digit
                }

                # Count how many cells can have this digit
                count = 0
                last_col = -1
                for (col = 0; col < 9; col++) {
                    if (has_bit(grid_candidates[row, col], digit)) {
                        count++
                        last_col = col
                    }
                }

                if (count == 1) {
                    # Hidden single found
                    if (!assign(row, last_col, digit)) {
                        return 0
                    }
                    changed = 1
                } else if (count == 0) {
                    # Digit cannot be placed anywhere - contradiction
                    return 0
                }
            }
        }

        # Strategy 3: Hidden singles in columns
        for (col = 0; col < 9; col++) {
            for (digit = 1; digit <= 9; digit++) {
                # First check if digit is already assigned in this column
                already_assigned = 0
                for (row = 0; row < 9; row++) {
                    if (grid_values[row, col] == digit) {
                        already_assigned = 1
                        break
                    }
                }
                if (already_assigned) {
                    continue  # Skip to next digit
                }

                # Count how many cells can have this digit
                count = 0
                last_row = -1
                for (row = 0; row < 9; row++) {
                    if (has_bit(grid_candidates[row, col], digit)) {
                        count++
                        last_row = row
                    }
                }

                if (count == 1) {
                    # Hidden single found
                    if (!assign(last_row, col, digit)) {
                        return 0
                    }
                    changed = 1
                } else if (count == 0) {
                    # Digit cannot be placed anywhere - contradiction
                    printf "ERROR: Col %d, digit %d has count=0 (contradiction)\n", col, digit > "/dev/stderr"
                    return 0
                }
            }
        }

        # Strategy 4: Hidden singles in boxes
        for (box = 0; box < 9; box++) {
            box_row = int(box / 3) * 3
            box_col = (box % 3) * 3

            for (digit = 1; digit <= 9; digit++) {
                # First check if digit is already assigned in this box
                already_assigned = 0
                for (r = box_row; r < box_row + 3; r++) {
                    for (c = box_col; c < box_col + 3; c++) {
                        if (grid_values[r, c] == digit) {
                            already_assigned = 1
                            r = box_row + 3  # Break outer loop
                            break
                        }
                    }
                }
                if (already_assigned) {
                    continue  # Skip to next digit
                }

                # Count how many cells can have this digit
                count = 0
                last_row = -1
                last_col = -1
                for (r = box_row; r < box_row + 3; r++) {
                    for (c = box_col; c < box_col + 3; c++) {
                        if (has_bit(grid_candidates[r, c], digit)) {
                            count++
                            last_row = r
                            last_col = c
                        }
                    }
                }

                if (count == 1) {
                    # Hidden single found
                    if (!assign(last_row, last_col, digit)) {
                        return 0
                    }
                    changed = 1
                } else if (count == 0) {
                    # Digit cannot be placed anywhere - contradiction
                    printf "ERROR: Box %d, digit %d has count=0 (contradiction)\n", box, digit > "/dev/stderr"
                    return 0
                }
            }
        }
    }

    return 1  # Success
}

# ============================================================================
# SEARCH
# ============================================================================

function find_mrv_cell(    min_candidates, found, row, col, num_candidates) {
    # Find cell with minimum remaining values (MRV heuristic)
    min_candidates = 10
    found = 0
    mrv_row = -1
    mrv_col = -1

    for (row = 0; row < 9; row++) {
        for (col = 0; col < 9; col++) {
            if (grid_values[row, col] == 0) {
                num_candidates = count_bits(grid_candidates[row, col])
                if (num_candidates < min_candidates) {
                    min_candidates = num_candidates
                    mrv_row = row
                    mrv_col = col
                    found = 1
                }
            }
        }
    }

    return found
}

function cp_search(    found, candidates, digit) {
    # Check if grid is complete
    found = find_mrv_cell()
    if (!found) {
        # No empty cells - solved
        return 1
    }

    # Try each candidate for MRV cell
    candidates = grid_candidates[mrv_row, mrv_col]

    for (digit = 1; digit <= 9; digit++) {
        if (has_bit(candidates, digit)) {
            # Save grid state
            save_grid()

            # Try assigning digit
            if (assign(mrv_row, mrv_col, digit)) {
                # Propagate constraints
                if (propagate()) {
                    # Recurse
                    if (cp_search()) {
                        return 1
                    }
                }
            }

            # Backtrack
            restore_grid()
        }
    }

    return 0  # Dead end
}

# ============================================================================
# GRID STATE MANAGEMENT
# ============================================================================

function save_grid(    row, col) {
    # Save grid state for backtracking
    for (row = 0; row < 9; row++) {
        for (col = 0; col < 9; col++) {
            saved_values[row, col] = grid_values[row, col]
            saved_candidates[row, col] = grid_candidates[row, col]
        }
    }
}

function restore_grid(    row, col) {
    # Restore grid state after backtracking
    for (row = 0; row < 9; row++) {
        for (col = 0; col < 9; col++) {
            grid_values[row, col] = saved_values[row, col]
            grid_candidates[row, col] = saved_candidates[row, col]
        }
    }
}

function init_grid(puzzle,    row, col, digit) {
    # Initialize grid with puzzle
    for (row = 0; row < 9; row++) {
        for (col = 0; col < 9; col++) {
            if (puzzle[row, col] == 0) {
                grid_values[row, col] = 0
                grid_candidates[row, col] = FULL_CANDIDATES
            } else {
                digit = puzzle[row, col]
                grid_values[row, col] = digit
                grid_candidates[row, col] = power_of_2(digit)
            }
        }
    }
}

# ============================================================================
# I/O FUNCTIONS
# ============================================================================

function print_puzzle(label,    row, col) {
    printf "\n%s:\n", label
    for (row = 0; row < 9; row++) {
        for (col = 0; col < 9; col++) {
            printf "%d ", grid_values[row, col]
        }
        printf "\n"
    }
}

function read_matrix(filename, puzzle,    line, row, col, n, parts, i) {
    row = 0

    # Print filename (normalize /app/Matrices to ../Matrices)
    if (index(filename, "/app/Matrices/") == 1) {
        printf "../%s\n", substr(filename, 6)
    } else {
        print filename
    }

    while ((getline line < filename) > 0) {
        # Skip comments and empty lines
        if (line ~ /^#/ || length(line) == 0) continue

        # Parse line
        n = split(line, parts, " ")
        col = 0
        for (i = 1; i <= n && col < 9; i++) {
            if (parts[i] != "") {
                puzzle[row, col] = parts[i] + 0
                printf "%d ", puzzle[row, col]
                col++
            }
        }
        printf "\n"
        row++
        if (row >= 9) break
    }
    close(filename)
    return 1
}

function solve_sudoku(puzzle,    solved) {
    # Initialize grid (clues already set with single-digit candidates)
    init_grid(puzzle)

    # Display initial puzzle
    print_puzzle("Puzzle")

    # Reset iteration counter
    cp_iterations = 0

    # Apply initial propagation (propagate will call assign for singletons/hidden singles)
    if (!propagate()) {
        printf "\nNo solution found (contradiction during initial propagation)\n"
        return 0
    }

    # Search for solution
    solved = cp_search()

    if (solved) {
        # Display solution
        print_puzzle("Puzzle")
        printf "\nSolved in Iterations=%d\n\n", cp_iterations
        return 1
    } else {
        printf "\nNo solution found\n"
        return 0
    }
}

# ============================================================================
# MAIN
# ============================================================================

END {
    if (ARGC < 2) {
        print "Usage: awk -f cp.awk <matrix_file>" > "/dev/stderr"
        exit 1
    }

    filename = ARGV[1]
    if (read_matrix(filename, puzzle)) {
        # Get timing from external command
        cmd = "date +%s.%N"
        cmd | getline start_time
        close(cmd)

        solve_sudoku(puzzle)

        cmd = "date +%s.%N"
        cmd | getline end_time
        close(cmd)

        printf "Seconds to process %.3f\n", end_time - start_time
    }
}
