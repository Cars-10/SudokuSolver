#!/usr/bin/env Rscript
#
# Constraint Propagation (CP) Sudoku Solver - R Implementation
# Mechanical translation from C reference to preserve algorithm correctness.
#
# Algorithm: Backtracking search with constraint propagation
# - Uses bitsets for candidate tracking (integers with bit operations)
# - Propagates constraints using singleton and hidden single strategies
# - MRV (Minimum Remaining Values) heuristic for cell selection
#
# Note: R uses 1-based indexing - adjusted in array accesses

# Global iteration counter
cp_iterations <- 0L

# Sudoku puzzle grid [row, col] - 1-indexed
puzzle <- matrix(0L, nrow = 9, ncol = 9)

# CPGrid structure (as R list with matrices)
new_cp_grid <- function() {
    list(
        values = matrix(0L, nrow = 9, ncol = 9),          # Assigned values (0 = empty)
        candidates = matrix(0L, nrow = 9, ncol = 9)       # Possible values per cell (bitset)
    )
}

# ============================================================================
# BITSET HELPER FUNCTIONS
# ============================================================================

has_candidate <- function(set, digit) {
    return(bitwAnd(set, bitwShiftL(1L, digit)) != 0)
}

add_candidate <- function(set, digit) {
    return(bitwOr(set, bitwShiftL(1L, digit)))
}

remove_candidate <- function(set, digit) {
    return(bitwAnd(set, bitwNot(bitwShiftL(1L, digit))))
}

# Count number of candidates in a bitset (popcount)
count_candidates <- function(set) {
    count <- 0L
    for (digit in 1:9) {
        if (has_candidate(set, digit)) {
            count <- count + 1L
        }
    }
    return(count)
}

# Get first candidate digit from bitset (1-9)
get_first_candidate <- function(set) {
    for (digit in 1:9) {
        if (has_candidate(set, digit)) {
            return(digit)
        }
    }
    return(0L)
}

# Get all 20 peers for a cell (row, col, box)
get_peers <- function(row, col) {
    peers <- matrix(0L, nrow = 20, ncol = 2)
    idx <- 1L

    # Same row (9 cells minus self = 8)
    for (c in 1:9) {
        if (c != col) {
            peers[idx, 1] <- row
            peers[idx, 2] <- c
            idx <- idx + 1L
        }
    }

    # Same column (9 cells minus self = 8)
    for (r in 1:9) {
        if (r != row) {
            peers[idx, 1] <- r
            peers[idx, 2] <- col
            idx <- idx + 1L
        }
    }

    # Same 3x3 box (9 cells minus self minus already counted = 4)
    box_row <- ((row - 1) %/% 3) * 3 + 1
    box_col <- ((col - 1) %/% 3) * 3 + 1
    for (r in box_row:(box_row + 2)) {
        for (c in box_col:(box_col + 2)) {
            if (r != row && c != col) {
                peers[idx, 1] <- r
                peers[idx, 2] <- c
                idx <- idx + 1L
            }
        }
    }

    return(peers)
}

# ============================================================================
# INITIALIZATION
# ============================================================================

init_grid <- function(grid, puz) {
    for (row in 1:9) {
        for (col in 1:9) {
            if (puz[row, col] == 0) {
                # Empty cell: set all candidates 1-9 (bits 1-9 set)
                grid$values[row, col] <- 0L
                grid$candidates[row, col] <- 0x3FEL  # Binary: 0011 1111 1110 (bits 1-9)
            } else {
                # Given clue: set single value
                digit <- puz[row, col]
                grid$values[row, col] <- digit
                grid$candidates[row, col] <- bitwShiftL(1L, digit)
            }
        }
    }
    return(grid)
}

# ============================================================================
# CONSTRAINT PROPAGATION
# ============================================================================

eliminate <- function(grid, row, col, digit) {
    # Check if digit is already eliminated
    if (!has_candidate(grid$candidates[row, col], digit)) {
        return(list(success = TRUE, grid = grid))  # Already eliminated, no change
    }

    # Remove digit from candidates
    grid$candidates[row, col] <- remove_candidate(grid$candidates[row, col], digit)

    # Check for contradiction (no candidates left)
    remaining <- count_candidates(grid$candidates[row, col])
    if (remaining == 0) {
        return(list(success = FALSE, grid = grid))  # Contradiction
    }

    # If only one candidate left, assign it (singleton elimination)
    if (remaining == 1 && grid$values[row, col] == 0) {
        last_digit <- get_first_candidate(grid$candidates[row, col])
        result <- assign(grid, row, col, last_digit)
        return(result)
    }

    return(list(success = TRUE, grid = grid))
}

assign <- function(grid, row, col, digit) {
    # Increment iteration counter (this is our benchmark metric)
    cp_iterations <<- cp_iterations + 1L

    # Set value
    grid$values[row, col] <- digit
    grid$candidates[row, col] <- bitwShiftL(1L, digit)

    # Eliminate digit from all peers
    peers <- get_peers(row, col)

    for (i in 1:20) {
        peer_row <- peers[i, 1]
        peer_col <- peers[i, 2]

        result <- eliminate(grid, peer_row, peer_col, digit)
        if (!result$success) {
            return(list(success = FALSE, grid = grid))  # Contradiction in peer elimination
        }
        grid <- result$grid
    }

    return(list(success = TRUE, grid = grid))
}

propagate <- function(grid) {
    changed <- TRUE

    while (changed) {
        changed <- FALSE

        # Strategy 1: Singleton elimination
        # If a cell has only one candidate, assign it
        for (row in 1:9) {
            for (col in 1:9) {
                if (grid$values[row, col] == 0) {
                    num_candidates <- count_candidates(grid$candidates[row, col])
                    if (num_candidates == 0) {
                        return(list(success = FALSE, grid = grid))  # Contradiction
                    }
                    if (num_candidates == 1) {
                        digit <- get_first_candidate(grid$candidates[row, col])
                        result <- assign(grid, row, col, digit)
                        if (!result$success) {
                            return(list(success = FALSE, grid = grid))  # Assignment caused contradiction
                        }
                        grid <- result$grid
                        changed <- TRUE
                    }
                }
            }
        }

        # Strategy 2: Hidden singles
        # For each unit (row, col, box), if a digit appears in only one cell, assign it

        # Check rows
        for (row in 1:9) {
            for (digit in 1:9) {
                count <- 0L
                last_col <- -1L
                already_assigned <- FALSE

                for (col in 1:9) {
                    if (grid$values[row, col] == digit) {
                        already_assigned <- TRUE
                        break
                    }
                    if (has_candidate(grid$candidates[row, col], digit)) {
                        count <- count + 1L
                        last_col <- col
                    }
                }

                if (!already_assigned) {
                    if (count == 1) {
                        result <- assign(grid, row, last_col, digit)
                        if (!result$success) {
                            return(list(success = FALSE, grid = grid))
                        }
                        grid <- result$grid
                        changed <- TRUE
                    } else if (count == 0) {
                        return(list(success = FALSE, grid = grid))  # Digit cannot be placed anywhere in row
                    }
                }
            }
        }

        # Check columns
        for (col in 1:9) {
            for (digit in 1:9) {
                count <- 0L
                last_row <- -1L
                already_assigned <- FALSE

                for (row in 1:9) {
                    if (grid$values[row, col] == digit) {
                        already_assigned <- TRUE
                        break
                    }
                    if (has_candidate(grid$candidates[row, col], digit)) {
                        count <- count + 1L
                        last_row <- row
                    }
                }

                if (!already_assigned) {
                    if (count == 1) {
                        result <- assign(grid, last_row, col, digit)
                        if (!result$success) {
                            return(list(success = FALSE, grid = grid))
                        }
                        grid <- result$grid
                        changed <- TRUE
                    } else if (count == 0) {
                        return(list(success = FALSE, grid = grid))  # Digit cannot be placed anywhere in column
                    }
                }
            }
        }

        # Check boxes
        for (box in 0:8) {
            box_row <- (box %/% 3) * 3 + 1
            box_col <- (box %% 3) * 3 + 1

            for (digit in 1:9) {
                count <- 0L
                last_r <- -1L
                last_c <- -1L
                already_assigned <- FALSE

                for (r in box_row:(box_row + 2)) {
                    for (c in box_col:(box_col + 2)) {
                        if (grid$values[r, c] == digit) {
                            already_assigned <- TRUE
                            break
                        }
                        if (has_candidate(grid$candidates[r, c], digit)) {
                            count <- count + 1L
                            last_r <- r
                            last_c <- c
                        }
                    }
                    if (already_assigned) break
                }

                if (!already_assigned) {
                    if (count == 1) {
                        result <- assign(grid, last_r, last_c, digit)
                        if (!result$success) {
                            return(list(success = FALSE, grid = grid))
                        }
                        grid <- result$grid
                        changed <- TRUE
                    } else if (count == 0) {
                        return(list(success = FALSE, grid = grid))  # Digit cannot be placed anywhere in box
                    }
                }
            }
        }
    }

    return(list(success = TRUE, grid = grid))  # Success - reached fixpoint
}

# ============================================================================
# SEARCH
# ============================================================================

find_mrv_cell <- function(grid) {
    min_candidates <- 10L  # More than 9, so any cell will be smaller
    found_row <- -1L
    found_col <- -1L

    for (r in 1:9) {
        for (c in 1:9) {
            if (grid$values[r, c] == 0) {
                num_candidates <- count_candidates(grid$candidates[r, c])
                if (num_candidates < min_candidates) {
                    min_candidates <- num_candidates
                    found_row <- r
                    found_col <- c
                }
            }
        }
    }

    if (found_row == -1L) {
        return(NULL)  # No empty cells (grid complete)
    } else {
        return(list(row = found_row, col = found_col))
    }
}

cp_search <- function(grid) {
    # Base case: check if grid is complete
    mrv_cell <- find_mrv_cell(grid)
    if (is.null(mrv_cell)) {
        # No empty cells - grid is complete, return solution
        return(list(success = TRUE, grid = grid))
    }

    # Recursive case: try each candidate for the MRV cell
    mrv_row <- mrv_cell$row
    mrv_col <- mrv_cell$col
    candidates <- grid$candidates[mrv_row, mrv_col]

    for (digit in 1:9) {
        if (has_candidate(candidates, digit)) {
            # Save grid state for backtracking (deep copy)
            grid_copy <- list(
                values = matrix(grid$values, nrow = 9, ncol = 9),
                candidates = matrix(grid$candidates, nrow = 9, ncol = 9)
            )

            # Try assigning this digit
            result <- assign(grid, mrv_row, mrv_col, digit)
            if (result$success) {
                grid <- result$grid

                # Assignment succeeded, propagate constraints
                prop_result <- propagate(grid)
                if (prop_result$success) {
                    grid <- prop_result$grid

                    # Propagation succeeded, recurse
                    search_result <- cp_search(grid)
                    if (search_result$success) {
                        return(search_result)  # Found solution
                    }
                }
            }

            # Failed - restore grid state and try next candidate
            grid <- grid_copy
        }
    }

    # All candidates exhausted - dead end
    return(list(success = FALSE, grid = grid))
}

# Print puzzle
print_puzzle <- function(grid) {
    cat("\nPuzzle:\n")
    for (r in 1:9) {
        for (c in 1:9) {
            cat(grid[r, c], " ", sep = "")
        }
        cat("\n")
    }
}

# Read matrix file
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

# Main function
main <- function() {
    start_time <- Sys.time()

    # Process each .matrix file from command line
    args <- commandArgs(trailingOnly = TRUE)
    for (arg in args) {
        if (grepl("\\.matrix$", arg)) {
            # Reset puzzle
            puzzle <<- matrix(0L, nrow = 9, ncol = 9)

            read_matrix_file(arg)
            print_puzzle(puzzle)

            # Initialize grid
            grid <- new_cp_grid()
            grid <- init_grid(grid, puzzle)

            # Initial constraint propagation for given clues
            cp_iterations <<- 0L
            prop_result <- propagate(grid)

            if (prop_result$success) {
                grid <- prop_result$grid

                # Solve using CP search
                result <- cp_search(grid)

                if (result$success) {
                    print_puzzle(result$grid$values)
                    cat("\nSolved in Iterations=", cp_iterations, "\n\n", sep = "")
                } else {
                    cat("\nNo solution found\n")
                }
            } else {
                cat("\nNo solution found (initial propagation failed)\n")
            }
        }
    }

    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    cat(sprintf("Seconds to process %.3f\n", elapsed))
}

main()
