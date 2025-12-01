#!/usr/bin/awk -f

BEGIN {
    iterations = 0
}

function print_board(    i, j) {
    print "Puzzle:"
    for (i = 0; i < 9; i++) {
        for (j = 0; j < 9; j++) {
            printf "%d ", puzzle[i, j]
        }
        print ""
    }
}

function is_possible(row, col, num,    i, j, start_row, start_col) {
    for (i = 0; i < 9; i++) {
        if (puzzle[row, i] == num || puzzle[i, col] == num) {
            return 0
        }
    }

    start_row = int(row / 3) * 3
    start_col = int(col / 3) * 3
    for (i = 0; i < 3; i++) {
        for (j = 0; j < 3; j++) {
            if (puzzle[start_row + i, start_col + j] == num) {
                return 0
            }
        }
    }
    return 1
}

function solve(row, col,    next_row, next_col, num) {
    if (row == 9) {
        return 1
    }

    next_row = row
    next_col = col + 1
    if (next_col == 9) {
        next_row = row + 1
        next_col = 0
    }

    if (puzzle[row, col] != 0) {
        return solve(next_row, next_col)
    }

    for (num = 1; num <= 9; num++) {
        iterations++
        if (is_possible(row, col, num)) {
            puzzle[row, col] = num
            if (solve(next_row, next_col)) {
                return 1
            }
            puzzle[row, col] = 0
        }
    }
    return 0
}

function read_board(filename,    line, parts, row, col, i) {
    row = 0
    while ((getline line < filename) > 0) {
        if (line !~ /^#/ && length(line) > 0) {
            split(line, parts, " ")
            col = 0
            for (i = 1; i <= length(parts); i++) {
                if (parts[i] != "") {
                    if (col < 9) {
                        puzzle[row, col] = parts[i] + 0
                        col++
                    }
                }
            }
            row++
            if (row == 9) break
        }
    }
    close(filename)
    return 1
}

{
    # Main loop processes arguments
}

END {
    if (ARGC < 2) {
        print "Usage: awk -f Sudoku.awk <file1> <file2> ..."
        exit 1
    }

    for (i = 1; i < ARGC; i++) {
        filename = ARGV[i]
        print "\nProcessing " filename
        if (read_board(filename)) {
            print_board()
            iterations = 0
            if (solve(0, 0)) {
                print_board()
                print "\nSolved in Iterations=" iterations
            } else {
                print "No solution found"
            }
        }
    }
}
