#!/usr/bin/env Rscript
#
# Sudoku Solver - R Implementation
# Brute-force backtracking algorithm matching C reference exactly.
#
# Algorithm:
# - Row-major search for empty cells (top-to-bottom, left-to-right)
# - Try values 1-9 in ascending order
# - Count EVERY placement attempt (algorithm fingerprint)
#
# Note: R uses 1-based indexing - adjust box calculations accordingly

# Global puzzle grid [row, col] - 1-indexed
puzzle <- matrix(0L, nrow = 9, ncol = 9)
count <- 0L  # Iteration counter

print_puzzle <- function() {
    cat("\nPuzzle:\n")
    for (row in 1:9) {
        for (col in 1:9) {
            cat(puzzle[row, col], " ", sep = "")
        }
        cat("\n")
    }
}

read_matrix_file <- function(filename) {
    # Normalize path for output (match C format)
    display_path <- filename
    if (startsWith(filename, "/app/Matrices/")) {
        display_path <- paste0("../", substring(filename, 6))  # Skip "/app/" to get "Matrices/..."
    }
    cat(display_path, "\n", sep = "")

    lines <- readLines(filename, warn = FALSE)
    line_count <- 0

    for (line in lines) {
        # Skip comments and empty lines
        line <- trimws(line)
        if (nchar(line) == 0 || substr(line, 1, 1) == "#") {
            next
        }

        # Parse 9 integers from line
        values <- as.integer(strsplit(line, "\\s+")[[1]])
        if (length(values) == 9 && line_count < 9) {
            line_count <- line_count + 1
            for (col in 1:9) {
                puzzle[line_count, col] <<- values[col]
            }
            # Print with trailing space to match C format
            for (col in 1:9) {
                cat(values[col], " ", sep = "")
            }
            cat("\n")
        }
    }
}

is_valid <- function(row, col, val) {
    # Check row
    for (i in 1:9) {
        if (puzzle[row, i] == val) {
            return(FALSE)
        }
    }

    # Check column
    for (i in 1:9) {
        if (puzzle[i, col] == val) {
            return(FALSE)
        }
    }

    # Check 3x3 box
    # R is 1-indexed, so adjust box calculation
    # C: box_row = (row / 3) * 3 where row is 0-8
    # R: box_row = ((row - 1) %/% 3) * 3 + 1 where row is 1-9
    box_row <- ((row - 1) %/% 3) * 3 + 1
    box_col <- ((col - 1) %/% 3) * 3 + 1
    for (i in 0:2) {
        for (j in 0:2) {
            if (puzzle[box_row + i, box_col + j] == val) {
                return(FALSE)
            }
        }
    }

    return(TRUE)
}

# BRUTE-FORCE SOLVER
# Searches row-major order (top-to-bottom, left-to-right)
# Tries candidates 1-9 in ascending order
# Counts EVERY placement attempt (the algorithm fingerprint)
solve <- function() {
    # Find first empty cell (row-major order)
    found_row <- -1L
    found_col <- -1L
    for (r in 1:9) {
        for (c in 1:9) {
            if (puzzle[r, c] == 0L) {
                found_row <- r
                found_col <- c
                break
            }
        }
        if (found_row != -1L) {
            break
        }
    }

    # If no empty cell found, puzzle is solved
    if (found_row == -1L) {
        print_puzzle()
        cat("\nSolved in Iterations=", count, "\n\n", sep = "")
        return(TRUE)
    }

    # Try values 1-9 in order
    for (val in 1:9) {
        count <<- count + 1L  # COUNT EVERY ATTEMPT - this is the algorithm fingerprint

        if (is_valid(found_row, found_col, val)) {
            puzzle[found_row, found_col] <<- val  # Place value

            if (solve()) {
                return(TRUE)
            }

            puzzle[found_row, found_col] <<- 0L  # Backtrack
        }
    }

    return(FALSE)
}

main <- function() {
    start_time <- Sys.time()

    # Process each .matrix file from command line
    args <- commandArgs(trailingOnly = TRUE)
    for (arg in args) {
        if (grepl("\\.matrix$", arg)) {
            # Reset puzzle
            puzzle <<- matrix(0L, nrow = 9, ncol = 9)

            read_matrix_file(arg)
            print_puzzle()
            count <<- 0L
            solve()
        }
    }

    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    cat(sprintf("Seconds to process %.3f\n", elapsed))
}

main()
