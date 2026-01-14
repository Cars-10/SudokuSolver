#!/usr/bin/awk -f
# DLX (Dancing Links) Sudoku Solver in AWK
# Algorithm X implementation using circular doubly-linked lists
# Follows C reference exactly for iteration counting

BEGIN {
    # Global iteration counter
    dlx_iterations = 0

    # Node structure simulation using associative arrays
    # Each node has: left, right, up, down, column, row_id
    # Root node is at index 0
    # Column headers are at indices 1-324
    # Row nodes follow after

    MAX_NODES = 3000  # Pre-allocate for performance
    next_node = 0
}

# ============================================================================
# NODE ALLOCATION
# ============================================================================

function alloc_node() {
    return next_node++
}

# ============================================================================
# COLUMN OPERATIONS
# ============================================================================

function cover_column(c,    col_node, row_node, right_node) {
    col_node = c

    # Remove column header from header list
    node_right[node_left[col_node]] = node_right[col_node]
    node_left[node_right[col_node]] = node_left[col_node]

    # For each row in this column
    row_node = node_down[col_node]
    while (row_node != col_node) {
        # For each node in this row (excluding column)
        right_node = node_right[row_node]
        while (right_node != row_node) {
            # Remove node from its column
            node_down[node_up[right_node]] = node_down[right_node]
            node_up[node_down[right_node]] = node_up[right_node]
            column_size[node_column[right_node]]--
            right_node = node_right[right_node]
        }
        row_node = node_down[row_node]
    }
}

function uncover_column(c,    col_node, row_node, left_node) {
    col_node = c

    # For each row in this column (in reverse order)
    row_node = node_up[col_node]
    while (row_node != col_node) {
        # For each node in this row (in reverse order)
        left_node = node_left[row_node]
        while (left_node != row_node) {
            # Restore node to its column
            column_size[node_column[left_node]]++
            node_down[node_up[left_node]] = left_node
            node_up[node_down[left_node]] = left_node
            left_node = node_left[left_node]
        }
        row_node = node_up[row_node]
    }

    # Restore column header to header list
    node_right[node_left[col_node]] = col_node
    node_left[node_right[col_node]] = col_node
}

function choose_column(root,    best, min_size, col_node, col) {
    best = -1
    min_size = 999999

    col_node = node_right[root]
    while (col_node != root) {
        if (column_size[col_node] < min_size) {
            min_size = column_size[col_node]
            best = col_node
        }
        col_node = node_right[col_node]
    }

    return best
}

# ============================================================================
# DLX SEARCH
# ============================================================================

function dlx_search(root, k,    col, row_node, right_node, left_node) {
    dlx_iterations++

    # If matrix is empty, solution found
    if (node_right[root] == root) {
        return 1
    }

    # Choose column with minimum size
    col = choose_column(root)

    # If column has no rows, no solution
    if (column_size[col] == 0) {
        return 0
    }

    # Cover this column
    cover_column(col)

    # Try each row in this column
    row_node = node_down[col]
    while (row_node != col) {
        # Add row to solution
        solution[k] = node_row_id[row_node]

        # Cover all other columns in this row
        right_node = node_right[row_node]
        while (right_node != row_node) {
            cover_column(node_column[right_node])
            right_node = node_right[right_node]
        }

        # Recurse
        if (dlx_search(root, k + 1)) {
            return 1
        }

        # Backtrack: uncover columns
        left_node = node_left[row_node]
        while (left_node != row_node) {
            uncover_column(node_column[left_node])
            left_node = node_left[left_node]
        }

        row_node = node_down[row_node]
    }

    # Uncover column
    uncover_column(col)

    return 0
}

# ============================================================================
# MATRIX CONSTRUCTION
# ============================================================================

function build_dlx_matrix(root) {
    # Root node at index 0
    root = 0
    node_left[root] = root
    node_right[root] = root
    node_up[root] = root
    node_down[root] = root
    node_column[root] = root
    node_row_id[root] = -1
    column_size[root] = 0
    next_node = 1

    # Create 324 column headers (constraints)
    # 81 cells + 81 rows + 81 cols + 81 boxes = 324 constraints
    for (i = 0; i < 324; i++) {
        col = alloc_node()

        # Link into header list
        node_left[col] = node_left[root]
        node_right[col] = root
        node_right[node_left[root]] = col
        node_left[root] = col

        # Column points to itself vertically
        node_up[col] = col
        node_down[col] = col

        node_column[col] = col
        node_row_id[col] = -1
        column_size[col] = 0
    }

    # Create rows (729 possibilities: 9 rows × 9 cols × 9 digits)
    for (row = 0; row < 9; row++) {
        for (col = 0; col < 9; col++) {
            for (digit = 1; digit <= 9; digit++) {
                add_dlx_row(root, row, col, digit)
            }
        }
    }

    return root
}

function add_dlx_row(root, row, col, digit,    box, c1, c2, c3, c4, n1, n2, n3, n4, col_header) {
    # Calculate constraint column indices
    box = int(row / 3) * 3 + int(col / 3)

    # Constraint columns:
    # c1: cell constraint (position must be filled)
    # c2: row constraint (digit in row)
    # c3: col constraint (digit in column)
    # c4: box constraint (digit in box)
    c1 = row * 9 + col + 1
    c2 = 81 + row * 9 + (digit - 1) + 1
    c3 = 162 + col * 9 + (digit - 1) + 1
    c4 = 243 + box * 9 + (digit - 1) + 1

    # Create 4 nodes for this row
    n1 = alloc_node()
    n2 = alloc_node()
    n3 = alloc_node()
    n4 = alloc_node()

    # Link nodes horizontally (circular)
    node_left[n1] = n4
    node_right[n1] = n2
    node_left[n2] = n1
    node_right[n2] = n3
    node_left[n3] = n2
    node_right[n3] = n4
    node_left[n4] = n3
    node_right[n4] = n1

    # Link into column c1
    col_header = c1
    node_column[n1] = col_header
    node_row_id[n1] = row * 81 + col * 9 + (digit - 1)
    node_up[n1] = node_up[col_header]
    node_down[n1] = col_header
    node_down[node_up[col_header]] = n1
    node_up[col_header] = n1
    column_size[col_header]++

    # Link into column c2
    col_header = c2
    node_column[n2] = col_header
    node_row_id[n2] = row * 81 + col * 9 + (digit - 1)
    node_up[n2] = node_up[col_header]
    node_down[n2] = col_header
    node_down[node_up[col_header]] = n2
    node_up[col_header] = n2
    column_size[col_header]++

    # Link into column c3
    col_header = c3
    node_column[n3] = col_header
    node_row_id[n3] = row * 81 + col * 9 + (digit - 1)
    node_up[n3] = node_up[col_header]
    node_down[n3] = col_header
    node_down[node_up[col_header]] = n3
    node_up[col_header] = n3
    column_size[col_header]++

    # Link into column c4
    col_header = c4
    node_column[n4] = col_header
    node_row_id[n4] = row * 81 + col * 9 + (digit - 1)
    node_up[n4] = node_up[col_header]
    node_down[n4] = col_header
    node_down[node_up[col_header]] = n4
    node_up[col_header] = n4
    column_size[col_header]++
}

function cover_clues(root, puzzle,    row, col, digit, box, c1, c2, c3, c4, col_node) {
    # Cover constraints for given clues
    for (row = 0; row < 9; row++) {
        for (col = 0; col < 9; col++) {
            digit = puzzle[row, col]
            if (digit > 0) {
                box = int(row / 3) * 3 + int(col / 3)

                # Calculate constraint columns
                c1 = row * 9 + col + 1
                c2 = 81 + row * 9 + (digit - 1) + 1
                c3 = 162 + col * 9 + (digit - 1) + 1
                c4 = 243 + box * 9 + (digit - 1) + 1

                # Cover each constraint column
                cover_column(c1)
                cover_column(c2)
                cover_column(c3)
                cover_column(c4)
            }
        }
    }
}

# ============================================================================
# SOLUTION EXTRACTION
# ============================================================================

function extract_solution(sol_array, solution_grid,    i, row_id, row, col, digit) {
    # Initialize solution grid
    for (i = 0; i < 81; i++) {
        solution_grid[i] = 0
    }

    # Extract from solution array
    for (i = 0; solution[i] != ""; i++) {
        row_id = solution[i]
        if (row_id >= 0) {
            digit = (row_id % 9) + 1
            col = int((row_id / 9) % 9)
            row = int(row_id / 81)
            solution_grid[row * 9 + col] = digit
        }
    }
}

# ============================================================================
# I/O FUNCTIONS
# ============================================================================

function print_puzzle(puzzle, label) {
    printf "\n%s:\n", label
    for (r = 0; r < 9; r++) {
        for (c = 0; c < 9; c++) {
            printf "%d ", puzzle[r, c]
        }
        printf "\n"
    }
}

function read_matrix(filename, puzzle,    line, row, col, n, parts) {
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

function solve_sudoku(puzzle,    root, solved, solution_grid) {
    # Build DLX matrix
    root = build_dlx_matrix(root)

    # Cover clues
    cover_clues(root, puzzle)

    # Display initial puzzle
    print_puzzle(puzzle, "Puzzle")

    # Initialize solution array
    for (i = 0; i < 81; i++) {
        solution[i] = ""
    }

    # Reset iteration counter
    dlx_iterations = 0

    # Search for solution
    solved = dlx_search(root, 0)

    if (solved) {
        # Extract and merge with clues
        extract_solution(solution, solution_grid)

        # Merge clues into solution
        for (r = 0; r < 9; r++) {
            for (c = 0; c < 9; c++) {
                if (puzzle[r, c] > 0) {
                    solution_grid[r * 9 + c] = puzzle[r, c]
                }
            }
        }

        # Display solution
        printf "\nPuzzle:\n"
        for (r = 0; r < 9; r++) {
            for (c = 0; c < 9; c++) {
                printf "%d ", solution_grid[r * 9 + c]
            }
            printf "\n"
        }

        printf "\nSolved in Iterations=%d\n\n", dlx_iterations
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
        print "Usage: awk -f dlx.awk <matrix_file>" > "/dev/stderr"
        exit 1
    }

    filename = ARGV[1]
    if (read_matrix(filename, puzzle)) {
        # Get timing from external command (systime() not available in all AWK versions)
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
