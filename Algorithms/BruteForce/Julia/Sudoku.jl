#!/usr/bin/env julia
#=
Sudoku Solver - Julia Implementation
Brute-force backtracking algorithm matching C reference exactly.

Algorithm:
- Row-major search for empty cells (top-to-bottom, left-to-right)
- Try values 1-9 in ascending order
- Count EVERY placement attempt (algorithm fingerprint)

Note: Julia uses 1-based indexing - adjust box calculations accordingly
=#

using Printf

# Global puzzle grid [row, col] - 1-indexed
puzzle = zeros(Int, 9, 9)
count = 0  # Iteration counter

function print_puzzle()
    println("\nPuzzle:")
    for row in 1:9
        for col in 1:9
            print(puzzle[row, col], " ")
        end
        println()
    end
end

function read_matrix_file(filename::String)
    global puzzle

    # Normalize path for output (match C format)
    display_path = filename
    if startswith(filename, "/app/Matrices/")
        display_path = "../" * filename[6:end]  # Skip "/app/" to get "Matrices/..."
    end
    println(display_path)

    line_count = 0
    open(filename, "r") do file
        for line in eachline(file)
            # Skip comments and empty lines
            stripped = strip(line)
            if isempty(stripped) || startswith(stripped, '#')
                continue
            end

            # Parse 9 integers from line
            values = parse.(Int, split(stripped))
            if length(values) == 9 && line_count < 9
                line_count += 1
                for col in 1:9
                    puzzle[line_count, col] = values[col]
                end
                # Print with trailing space to match C format
                for col in 1:9
                    print(values[col], " ")
                end
                println()
            end
        end
    end
end

function is_valid(row::Int, col::Int, val::Int)::Bool
    # Check row
    for i in 1:9
        if puzzle[row, i] == val
            return false
        end
    end

    # Check column
    for i in 1:9
        if puzzle[i, col] == val
            return false
        end
    end

    # Check 3x3 box
    # Julia is 1-indexed, so adjust box calculation
    # C: box_row = (row / 3) * 3 where row is 0-8
    # Julia: box_row = div(row - 1, 3) * 3 + 1 where row is 1-9
    box_row = div(row - 1, 3) * 3 + 1
    box_col = div(col - 1, 3) * 3 + 1
    for i in 0:2
        for j in 0:2
            if puzzle[box_row + i, box_col + j] == val
                return false
            end
        end
    end

    return true
end

#=
BRUTE-FORCE SOLVER
Searches row-major order (top-to-bottom, left-to-right)
Tries candidates 1-9 in ascending order
Counts EVERY placement attempt (the algorithm fingerprint)
=#
function solve()::Bool
    global count

    # Find first empty cell (row-major order)
    found_row, found_col = -1, -1
    for r in 1:9
        for c in 1:9
            if puzzle[r, c] == 0
                found_row, found_col = r, c
                @goto found_empty
            end
        end
    end

    @label found_empty

    # If no empty cell found, puzzle is solved
    if found_row == -1
        print_puzzle()
        println("\nSolved in Iterations=$count\n")
        return true
    end

    # Try values 1-9 in order
    for val in 1:9
        count += 1  # COUNT EVERY ATTEMPT - this is the algorithm fingerprint

        if is_valid(found_row, found_col, val)
            puzzle[found_row, found_col] = val  # Place value

            if solve()
                return true
            end

            puzzle[found_row, found_col] = 0  # Backtrack
        end
    end

    return false
end

function main()
    global puzzle, count

    start_time = time()

    # Process each .matrix file from command line
    for arg in ARGS
        if endswith(arg, ".matrix")
            # Reset puzzle
            puzzle = zeros(Int, 9, 9)

            read_matrix_file(arg)
            print_puzzle()
            count = 0
            solve()
        end
    end

    elapsed = time() - start_time
    @printf("Seconds to process %.3f\n", elapsed)
end

main()
