#!/usr/bin/env julia
#=
Sudoku Solver - Constraint Propagation (CP) Implementation in Julia
Constraint propagation with backtracking search.

Algorithm:
- Use bitsets to track candidate values for each cell
- Apply constraint propagation (singleton elimination + hidden singles)
- Use MRV (Minimum Remaining Values) heuristic for search
- Backtrack when contradictions occur
- Count iterations for benchmarking

Note: Julia uses 1-based indexing
=#

using Printf

# ============================================================================
# DATA STRUCTURES
# ============================================================================

# Grid structure with assigned values and candidate tracking
mutable struct CPGrid
    values::Matrix{Int}          # Assigned values (0 = empty), 9x9
    candidates::Matrix{Int}      # Possible values per cell (bitset), 9x9

    function CPGrid()
        new(zeros(Int, 9, 9), zeros(Int, 9, 9))
    end
end

# ============================================================================
# GLOBAL STATE
# ============================================================================

# Global iteration counter (algorithm fingerprint)
const cp_iterations = Ref{Int}(0)

# ============================================================================
# BITSET OPERATIONS
# ============================================================================

# Check if digit is in candidate set
has_candidate(set::Int, digit::Int)::Bool = (set & (1 << digit)) != 0

# Add digit to candidate set
add_candidate(set::Int, digit::Int)::Int = set | (1 << digit)

# Remove digit from candidate set
remove_candidate(set::Int, digit::Int)::Int = set & ~(1 << digit)

# Count number of candidates in bitset
count_candidates(set::Int)::Int = count_ones(set)

# Get first candidate digit from bitset (1-9)
function get_first_candidate(set::Int)::Int
    for digit in 1:9
        if has_candidate(set, digit)
            return digit
        end
    end
    return 0
end

# ============================================================================
# INITIALIZATION
# ============================================================================

function init_grid!(grid::CPGrid, puzzle::Matrix{Int})
    for row in 1:9
        for col in 1:9
            if puzzle[row, col] == 0
                # Empty cell: set all candidates 1-9 (bits 1-9 set)
                grid.values[row, col] = 0
                grid.candidates[row, col] = 0x3FE  # Binary: 0011 1111 1110 (bits 1-9)
            else
                # Given clue: set single value
                digit = puzzle[row, col]
                grid.values[row, col] = digit
                grid.candidates[row, col] = (1 << digit)
            end
        end
    end
end

# ============================================================================
# PEER CALCULATION
# ============================================================================

# Get all 20 peers for a cell (row, col, box) - returns array of (row, col) tuples
function get_peers(row::Int, col::Int)::Vector{Tuple{Int, Int}}
    peers = Tuple{Int, Int}[]

    # Same row (9 cells minus self = 8)
    for c in 1:9
        if c != col
            push!(peers, (row, c))
        end
    end

    # Same column (9 cells minus self = 8)
    for r in 1:9
        if r != row
            push!(peers, (r, col))
        end
    end

    # Same 3x3 box (9 cells minus self minus already counted = 4)
    box_row = div(row - 1, 3) * 3 + 1
    box_col = div(col - 1, 3) * 3 + 1
    for r in box_row:(box_row + 2)
        for c in box_col:(box_col + 2)
            if r != row && c != col
                push!(peers, (r, c))
            end
        end
    end

    return peers
end

# ============================================================================
# CONSTRAINT PROPAGATION
# ============================================================================

function eliminate!(grid::CPGrid, row::Int, col::Int, digit::Int)::Bool
    # Check if digit is already eliminated
    if !has_candidate(grid.candidates[row, col], digit)
        return true  # Already eliminated, no change
    end

    # Remove digit from candidates
    grid.candidates[row, col] = remove_candidate(grid.candidates[row, col], digit)

    # Check for contradiction (no candidates left)
    remaining = count_candidates(grid.candidates[row, col])
    if remaining == 0
        return false  # Contradiction
    end

    # If only one candidate left, assign it (singleton elimination)
    if remaining == 1 && grid.values[row, col] == 0
        last_digit = get_first_candidate(grid.candidates[row, col])
        if !assign!(grid, row, col, last_digit)
            return false  # Assignment caused contradiction
        end
    end

    return true
end

function assign!(grid::CPGrid, row::Int, col::Int, digit::Int)::Bool
    # Increment iteration counter (this is our benchmark metric)
    cp_iterations[] += 1

    # Set value
    grid.values[row, col] = digit
    grid.candidates[row, col] = (1 << digit)

    # Eliminate digit from all peers
    peers = get_peers(row, col)

    for (peer_row, peer_col) in peers
        if !eliminate!(grid, peer_row, peer_col, digit)
            return false  # Contradiction in peer elimination
        end
    end

    return true
end

function propagate!(grid::CPGrid)::Bool
    changed = true

    while changed
        changed = false

        # Strategy 1: Singleton elimination
        # If a cell has only one candidate, assign it
        for row in 1:9
            for col in 1:9
                if grid.values[row, col] == 0
                    num_candidates = count_candidates(grid.candidates[row, col])
                    if num_candidates == 0
                        return false  # Contradiction
                    end
                    if num_candidates == 1
                        digit = get_first_candidate(grid.candidates[row, col])
                        if !assign!(grid, row, col, digit)
                            return false  # Assignment caused contradiction
                        end
                        changed = true
                    end
                end
            end
        end

        # Strategy 2: Hidden singles
        # For each unit (row, col, box), if a digit appears in only one cell, assign it

        # Check rows
        for row in 1:9
            for digit in 1:9
                count = 0
                last_col = -1
                found_assigned = false

                for col in 1:9
                    if grid.values[row, col] == digit
                        found_assigned = true
                        break
                    end
                    if has_candidate(grid.candidates[row, col], digit)
                        count += 1
                        last_col = col
                    end
                end

                if !found_assigned
                    if count == 1
                        if !assign!(grid, row, last_col, digit)
                            return false
                        end
                        changed = true
                    elseif count == 0
                        return false  # Digit cannot be placed anywhere in row
                    end
                end
            end
        end

        # Check columns
        for col in 1:9
            for digit in 1:9
                count = 0
                last_row = -1
                found_assigned = false

                for row in 1:9
                    if grid.values[row, col] == digit
                        found_assigned = true
                        break
                    end
                    if has_candidate(grid.candidates[row, col], digit)
                        count += 1
                        last_row = row
                    end
                end

                if !found_assigned
                    if count == 1
                        if !assign!(grid, last_row, col, digit)
                            return false
                        end
                        changed = true
                    elseif count == 0
                        return false  # Digit cannot be placed anywhere in column
                    end
                end
            end
        end

        # Check boxes
        for box in 0:8
            box_row = div(box, 3) * 3 + 1
            box_col = (box % 3) * 3 + 1

            for digit in 1:9
                count = 0
                last_r = -1
                last_c = -1
                found_assigned = false

                for r in box_row:(box_row + 2)
                    for c in box_col:(box_col + 2)
                        if grid.values[r, c] == digit
                            found_assigned = true
                            @goto next_box_digit
                        end
                        if has_candidate(grid.candidates[r, c], digit)
                            count += 1
                            last_r = r
                            last_c = c
                        end
                    end
                end

                @label next_box_digit

                if !found_assigned
                    if count == 1
                        if !assign!(grid, last_r, last_c, digit)
                            return false
                        end
                        changed = true
                    elseif count == 0
                        return false  # Digit cannot be placed anywhere in box
                    end
                end
            end
        end
    end

    return true  # Success - reached fixpoint
end

# ============================================================================
# SEARCH
# ============================================================================

function find_mrv_cell(grid::CPGrid)::Union{Tuple{Int, Int}, Nothing}
    min_candidates = 10  # More than 9, so any cell will be smaller
    best_row = -1
    best_col = -1

    for r in 1:9
        for c in 1:9
            if grid.values[r, c] == 0
                num_candidates = count_candidates(grid.candidates[r, c])
                if num_candidates < min_candidates
                    min_candidates = num_candidates
                    best_row = r
                    best_col = c
                end
            end
        end
    end

    if best_row == -1
        return nothing  # No empty cells (grid complete)
    else
        return (best_row, best_col)
    end
end

function cp_search(grid::CPGrid, solution::Vector{Int})::Bool
    # Base case: check if grid is complete
    mrv_cell = find_mrv_cell(grid)
    if mrv_cell === nothing
        # No empty cells - grid is complete, extract solution
        for r in 1:9
            for c in 1:9
                solution[(r - 1) * 9 + c] = grid.values[r, c]
            end
        end
        return true  # Solved
    end

    # Recursive case: try each candidate for the MRV cell
    mrv_row, mrv_col = mrv_cell
    candidates = grid.candidates[mrv_row, mrv_col]

    for digit in 1:9
        if has_candidate(candidates, digit)
            # Save grid state for backtracking
            grid_copy = deepcopy(grid)

            # Try assigning this digit
            if assign!(grid, mrv_row, mrv_col, digit)
                # Assignment succeeded, propagate constraints
                if propagate!(grid)
                    # Propagation succeeded, recurse
                    if cp_search(grid, solution)
                        return true  # Found solution
                    end
                end
            end

            # Failed - restore grid state and try next candidate
            grid.values = grid_copy.values
            grid.candidates = grid_copy.candidates
        end
    end

    # All candidates exhausted - dead end
    return false
end

# ============================================================================
# PUZZLE I/O
# ============================================================================

function print_puzzle(grid::Matrix{Int})
    println("\nPuzzle:")
    for r in 1:9
        for c in 1:9
            print(grid[r, c], " ")
        end
        println()
    end
end

function read_matrix_file(filename::String)::Matrix{Int}
    # Normalize path for output (match C format)
    display_path = filename
    if startswith(filename, "/app/Matrices/")
        display_path = "../" * filename[6:end]  # Skip "/app/" to get "Matrices/..."
    end
    println(display_path)

    puzzle = zeros(Int, 9, 9)
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

    return puzzle
end

# ============================================================================
# MAIN ENTRYPOINT
# ============================================================================

function main()
    start_time = time()

    if length(ARGS) == 0
        println("Usage: julia cp.jl <matrix_file>")
        return
    end

    # Read puzzle from file
    puzzle = read_matrix_file(ARGS[1])

    # Print initial puzzle
    print_puzzle(puzzle)

    # Initialize CP grid
    grid = CPGrid()
    init_grid!(grid, puzzle)

    # Apply initial propagation
    if !propagate!(grid)
        println("\nNo solution found (contradiction during initial propagation)\n")
        elapsed = time() - start_time
        @printf("Seconds to process %.3f\n", elapsed)
        return
    end

    # Run search
    solution = zeros(Int, 81)
    solved = cp_search(grid, solution)

    if solved
        # Convert solution array back to 2D for printing
        solution_grid = zeros(Int, 9, 9)
        for r in 1:9
            for c in 1:9
                solution_grid[r, c] = solution[(r - 1) * 9 + c]
            end
        end

        print_puzzle(solution_grid)
        println("\nSolved in Iterations=$(cp_iterations[])\n")
    else
        println("\nNo solution found\n")
    end

    elapsed = time() - start_time
    @printf("Seconds to process %.3f\n", elapsed)
end

main()
