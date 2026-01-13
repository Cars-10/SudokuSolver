#!/usr/bin/env Rscript
#
# Dancing Links (DLX) Sudoku Solver - R Implementation
# Mechanical translation from C reference to preserve algorithm correctness.
#
# Algorithm: Knuth's Algorithm X with Dancing Links data structure
# - Exact cover problem: 324 constraints (row-col, row-num, col-num, box-num)
# - Each Sudoku placement satisfies exactly 4 constraints
# - Doubly-linked circular lists for efficient cover/uncover operations
#
# Note: R uses 1-based indexing - adjusted in array accesses
# Implementation note: Using environments for mutable node objects

# Global iteration counter (analogous to brute-force count)
dlx_iterations <- 0L

# Sudoku puzzle grid [row, col] - 1-indexed
puzzle <- matrix(0L, nrow = 9, ncol = 9)
solution_grid <- matrix(0L, nrow = 9, ncol = 9)

# Next available node ID
next_node_id <- 0L

# DLX Node structure (as R environment for mutability)
new_dlx_node <- function() {
    node_id <- next_node_id
    next_node_id <<- next_node_id + 1L

    node <- new.env(parent = emptyenv())
    node$id <- node_id
    node$left_id <- -1L
    node$right_id <- -1L
    node$up_id <- -1L
    node$down_id <- -1L
    node$column_id <- -1L
    node$row_id <- -1L
    return(node)
}

# DLX Column structure (as R environment)
new_dlx_column <- function(name = "") {
    col <- new.env(parent = emptyenv())
    col$node <- new_dlx_node()
    col$size <- 0L
    col$name <- name
    col$id <- col$node$id
    col$node$column_id <- col$id
    return(col)
}

# Row metadata to map DLX rows back to Sudoku (row, col, num)
new_row_info <- function(row = 0L, col = 0L, num = 0L) {
    list(row = row, col = col, num = num)
}

# Global node registry (map node_id to node object)
node_registry <- new.env(parent = emptyenv())

register_node <- function(node) {
    node_registry[[as.character(node$id)]] <- node
}

get_node <- function(node_id) {
    if (node_id < 0) return(NULL)
    return(node_registry[[as.character(node_id)]])
}

# DLX matrix structures (global)
root <- NULL
columns <- list()      # 324 constraint columns (1-indexed)
row_info <- list()     # 729 possible rows (9x9x9)
row_starts <- list()   # ID of first node in each row

# Calculate constraint column indices (1-indexed for R)
get_position_col <- function(r, c) {
    return((r - 1) * 9 + c)
}

get_row_col <- function(r, n) {
    return(81 + (r - 1) * 9 + n)
}

get_col_col <- function(c, n) {
    return(162 + (c - 1) * 9 + n)
}

get_box_col <- function(r, c, n) {
    box <- ((r - 1) %/% 3) * 3 + ((c - 1) %/% 3)
    return(243 + box * 9 + n)
}

# Initialize DLX matrix structure
init_dlx_matrix <- function() {
    # Reset globals
    next_node_id <<- 0L
    node_registry <<- new.env(parent = emptyenv())

    # Allocate root column
    root <<- new_dlx_column("root")
    register_node(root$node)
    root$node$left_id <<- root$node$id
    root$node$right_id <<- root$node$id
    root$node$up_id <<- root$node$id
    root$node$down_id <<- root$node$id

    # Allocate 324 column headers (1-indexed)
    columns <<- vector("list", 324)
    for (i in 1:324) {
        columns[[i]] <<- new_dlx_column(paste0("C", i))
        register_node(columns[[i]]$node)

        # Initialize as circular list
        columns[[i]]$node$up_id <<- columns[[i]]$node$id
        columns[[i]]$node$down_id <<- columns[[i]]$node$id

        # Link into header list (at end, just before root)
        left_id <- root$node$left_id
        columns[[i]]$node$left_id <<- left_id
        columns[[i]]$node$right_id <<- root$node$id
        node_registry[[as.character(left_id)]]$right_id <<- columns[[i]]$node$id
        root$node$left_id <<- columns[[i]]$node$id
    }

    # Initialize row info
    row_info <<- vector("list", 729)
    row_starts <<- integer(729)  # Store node IDs
}

# Add a node to the DLX matrix
add_node <- function(col, row_id) {
    node <- new_dlx_node()
    register_node(node)
    node$column_id <- col$id
    node$row_id <- row_id

    # Insert at end of column's circular list
    up_node <- get_node(col$node$up_id)
    node$down_id <- col$node$id
    node$up_id <- up_node$id
    up_node$down_id <- node$id
    col$node$up_id <- node$id
    col$size <- col$size + 1L

    return(node)
}

# Build a DLX row for Sudoku cell (r,c) with value n
build_dlx_row <- function(r, c, n, row_id) {
    # Store row metadata
    row_info[[row_id]] <<- new_row_info(r, c, n)

    # Create nodes for the 4 constraints
    n1 <- add_node(columns[[get_position_col(r, c)]], row_id)
    n2 <- add_node(columns[[get_row_col(r, n)]], row_id)
    n3 <- add_node(columns[[get_col_col(c, n)]], row_id)
    n4 <- add_node(columns[[get_box_col(r, c, n)]], row_id)

    # Link nodes horizontally in circular list
    n1$right_id <- n2$id
    n2$right_id <- n3$id
    n3$right_id <- n4$id
    n4$right_id <- n1$id

    n1$left_id <- n4$id
    n2$left_id <- n1$id
    n3$left_id <- n2$id
    n4$left_id <- n3$id

    # Store first node ID for this row
    row_starts[[row_id]] <<- n1$id
}

# Build the complete DLX matrix from the puzzle
build_dlx_matrix_from_puzzle <- function() {
    row_id <- 1L  # 1-indexed in R

    for (r in 1:9) {
        for (c in 1:9) {
            if (puzzle[r, c] != 0) {
                # Cell has a clue - create only one row for that value
                build_dlx_row(r, c, puzzle[r, c], row_id)
                row_id <- row_id + 1L
            } else {
                # Cell is empty - create rows for all possible values
                for (n in 1:9) {
                    build_dlx_row(r, c, n, row_id)
                    row_id <- row_id + 1L
                }
            }
        }
    }
}

# Cover a column in the DLX matrix
dlx_cover_column <- function(col) {
    col_node <- col$node

    # Remove column header from the header list
    right_node <- get_node(col_node$right_id)
    left_node <- get_node(col_node$left_id)
    right_node$left_id <- left_node$id
    left_node$right_id <- right_node$id

    # For each row in this column
    row_node_id <- col_node$down_id
    while (row_node_id != col_node$id) {
        row_node <- get_node(row_node_id)

        # For each node in this row (excluding the column itself)
        right_node_id <- row_node$right_id
        while (right_node_id != row_node_id) {
            right_node <- get_node(right_node_id)

            # Remove this node from its column
            down_node <- get_node(right_node$down_id)
            up_node <- get_node(right_node$up_id)
            down_node$up_id <- up_node$id
            up_node$down_id <- down_node$id

            col_obj <- columns[[right_node$column_id]]
            col_obj$size <- col_obj$size - 1L

            right_node_id <- right_node$right_id
        }
        row_node_id <- row_node$down_id
    }
}

# Uncover a column (exact reverse of cover)
dlx_uncover_column <- function(col) {
    col_node <- col$node

    # For each row in this column (in reverse order)
    row_node_id <- col_node$up_id
    while (row_node_id != col_node$id) {
        row_node <- get_node(row_node_id)

        # For each node in this row (in reverse order)
        left_node_id <- row_node$left_id
        while (left_node_id != row_node_id) {
            left_node <- get_node(left_node_id)

            # Restore this node to its column
            col_obj <- columns[[left_node$column_id]]
            col_obj$size <- col_obj$size + 1L

            down_node <- get_node(left_node$down_id)
            up_node <- get_node(left_node$up_id)
            down_node$up_id <- left_node$id
            up_node$down_id <- left_node$id

            left_node_id <- left_node$left_id
        }
        row_node_id <- row_node$up_id
    }

    # Restore column header to the header list
    right_node <- get_node(col_node$right_id)
    left_node <- get_node(col_node$left_id)
    right_node$left_id <- col_node$id
    left_node$right_id <- col_node$id
}

# Choose column with minimum size (Knuth's S heuristic)
choose_column <- function(root_col) {
    root_node <- root_col$node
    best <- NULL
    min_size <- .Machine$integer.max

    col_node_id <- root_node$right_id
    while (col_node_id != root_node$id) {
        col_node <- get_node(col_node_id)
        col <- columns[[col_node$column_id]]
        if (col$size < min_size) {
            min_size <- col$size
            best <- col
        }
        col_node_id <- col_node$right_id
    }

    return(best)
}

# DLX Search - Algorithm X with Dancing Links
# Returns TRUE if solution found, FALSE otherwise
# solution[] stores the row indices of the solution
dlx_search <- function(root_col, k, solution) {
    # Count every search call (analogous to brute-force iterations)
    dlx_iterations <<- dlx_iterations + 1L

    root_node <- root_col$node

    # If matrix is empty, we found a solution
    if (root_node$right_id == root_node$id) {
        return(TRUE)
    }

    # Choose column with minimum size
    col <- choose_column(root_col)

    # If column has no rows, no solution possible
    if (is.null(col) || col$size == 0) {
        return(FALSE)
    }

    # Cover this column
    dlx_cover_column(col)

    # Try each row in this column
    row_node_id <- col$node$down_id
    while (row_node_id != col$node$id) {
        row_node <- get_node(row_node_id)

        # Add row to partial solution
        solution[k] <- row_node$row_id

        # Cover all other columns in this row
        right_node_id <- row_node$right_id
        while (right_node_id != row_node_id) {
            right_node <- get_node(right_node_id)
            dlx_cover_column(columns[[right_node$column_id]])
            right_node_id <- right_node$right_id
        }

        # Recurse
        if (dlx_search(root_col, k + 1L, solution)) {
            return(TRUE)  # Solution found
        }

        # Backtrack: uncover all columns in this row
        left_node_id <- row_node$left_id
        while (left_node_id != row_node_id) {
            left_node <- get_node(left_node_id)
            dlx_uncover_column(columns[[left_node$column_id]])
            left_node_id <- left_node$left_id
        }

        row_node_id <- row_node$down_id
    }

    # Uncover column
    dlx_uncover_column(col)

    return(FALSE)  # No solution found
}

# Cover given clues (pre-selected rows)
cover_clues <- function() {
    for (r in 1:9) {
        for (c in 1:9) {
            if (puzzle[r, c] != 0) {
                n <- puzzle[r, c]

                # Find the row for this clue
                for (row_id in 1:length(row_starts)) {
                    if (row_starts[[row_id]] > 0 &&
                        !is.null(row_info[[row_id]]) &&
                        row_info[[row_id]]$row == r &&
                        row_info[[row_id]]$col == c &&
                        row_info[[row_id]]$num == n) {

                        # Cover all columns in this row
                        node_id <- row_starts[[row_id]]
                        curr_id <- node_id
                        repeat {
                            curr <- get_node(curr_id)
                            dlx_cover_column(columns[[curr$column_id]])
                            curr_id <- curr$right_id
                            if (curr_id == node_id) break
                        }
                        break
                    }
                }
            }
        }
    }
}

# Extract solution from DLX and populate solution_grid
extract_solution <- function(solution, solution_len) {
    # Initialize solution grid - start with the original puzzle (includes clues)
    solution_grid <<- puzzle

    # Each solution entry is a row_id
    for (i in 1:solution_len) {
        row_id <- solution[i]
        if (row_id > 0 && row_id <= length(row_info) && !is.null(row_info[[row_id]])) {
            info <- row_info[[row_id]]
            solution_grid[info$row, info$col] <<- info$num
        }
    }
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

            # Initialize DLX matrix
            init_dlx_matrix()

            # Build matrix from puzzle
            build_dlx_matrix_from_puzzle()

            # Cover pre-filled clues
            cover_clues()

            # Solve using DLX
            dlx_iterations <<- 0L
            solution <- integer(81)
            result <- dlx_search(root, 1L, solution)

            if (result) {
                extract_solution(solution, 81)
                print_puzzle(solution_grid)
                cat("\nSolved in Iterations=", dlx_iterations, "\n\n", sep = "")
            } else {
                cat("\nNo solution found\n")
            }
        }
    }

    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    cat(sprintf("Seconds to process %.3f\n", elapsed))
}

main()
