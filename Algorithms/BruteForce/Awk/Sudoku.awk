#!/usr/bin/awk -f
# Sudoku Solver in AWK
# Brute-force backtracking algorithm matching C reference exactly

BEGIN {
    iterations = 0
}

function print_puzzle(    r, c) {
    printf "\nPuzzle:\n"
    for (r = 0; r < 9; r++) {
        for (c = 0; c < 9; c++) {
            printf "%d ", puzzle[r, c]
        }
        printf "\n"
    }
}

function is_valid(row, col, val,    i, box_row, box_col, br, bc) {
    # Check row
    for (i = 0; i < 9; i++) {
        if (puzzle[row, i] == val) return 0
    }

    # Check column
    for (i = 0; i < 9; i++) {
        if (puzzle[i, col] == val) return 0
    }

    # Check 3x3 box
    box_row = int(row / 3) * 3
    box_col = int(col / 3) * 3
    for (br = 0; br < 3; br++) {
        for (bc = 0; bc < 3; bc++) {
            if (puzzle[box_row + br, box_col + bc] == val) return 0
        }
    }

    return 1
}

function solve(    r, c, row, col, val) {
    # Find first empty cell (row-major order)
    row = -1
    col = -1
    for (r = 0; r < 9; r++) {
        for (c = 0; c < 9; c++) {
            if (puzzle[r, c] == 0) {
                row = r
                col = c
                break
            }
        }
        if (row != -1) break
    }

    # If no empty cell, puzzle is solved
    if (row == -1) {
        print_puzzle()
        printf "\nSolved in Iterations=%d\n\n", iterations
        return 1
    }

    # Try values 1-9 in order
    for (val = 1; val <= 9; val++) {
        iterations++  # Count EVERY attempt

        if (is_valid(row, col, val)) {
            puzzle[row, col] = val

            if (solve()) {
                return 1
            }

            puzzle[row, col] = 0  # Backtrack
        }
    }

    return 0
}

function read_matrix(filename,    line, row, col, parts, i, display_path) {
    row = 0

    # Print filename (normalize /app/Matrices to ../Matrices)
    if (index(filename, "/app/Matrices/") == 1) {
        display_path = "../" substr(filename, 6)  # Skip "/app/"
        print display_path
    } else {
        print filename
    }

    while ((getline line < filename) > 0) {
        # Skip comments and empty lines
        if (line ~ /^#/ || length(line) == 0) continue

        # Split line into parts
        split(line, parts, " ")
        col = 0
        for (i = 1; i <= length(parts) && col < 9; i++) {
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

END {
    if (ARGC < 2) {
        print "Usage: gawk -f Sudoku.awk <matrix_file>" > "/dev/stderr"
        exit 1
    }

    for (i = 1; i < ARGC; i++) {
        filename = ARGV[i]
        if (filename ~ /\.matrix$/) {
            read_matrix(filename)
            print_puzzle()
            iterations = 0
            if (!solve()) {
                print "No solution found"
            }
        }
    }
}
