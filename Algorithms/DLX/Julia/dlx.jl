#!/usr/bin/env julia
#=
Sudoku Solver - Dancing Links (DLX) Implementation in Julia
Exact cover problem solved with Algorithm X using Dancing Links.

Algorithm X (Knuth):
- Choose column with minimum size
- Cover that column
- For each row in the column, cover all columns in that row and recurse
- If recursion succeeds, done; otherwise uncover and try next row
- Count iterations for benchmarking

Note: Julia uses 1-based indexing
=#

using Printf

# ============================================================================
# DATA STRUCTURES
# ============================================================================

# Forward declaration using abstract types to handle circular reference
abstract type DlxElement end

# Mutable struct for DLX nodes in circular doubly-linked list
mutable struct DlxNode <: DlxElement
    left::Union{DlxElement, Nothing}
    right::Union{DlxElement, Nothing}
    up::Union{DlxElement, Nothing}
    down::Union{DlxElement, Nothing}
    column::Union{DlxElement, Nothing}  # Will be a DlxColumn
    row_id::Int

    DlxNode() = new(nothing, nothing, nothing, nothing, nothing, -1)
end

# Column header inherits behavior from DlxNode
mutable struct DlxColumn <: DlxElement
    node::DlxNode
    size::Int
    name::String

    function DlxColumn(name::String)
        col = new(DlxNode(), 0, name)
        # Make node circular
        col.node.left = col.node
        col.node.right = col.node
        col.node.up = col.node
        col.node.down = col.node
        col.node.column = col
        return col
    end
end

# ============================================================================
# GLOBAL STATE
# ============================================================================

# Global iteration counter (algorithm fingerprint)
const dlx_iterations = Ref{Int}(0)

# ============================================================================
# DLX CORE OPERATIONS
# ============================================================================

# Cover a column in the DLX matrix
# Remove column from header list and remove all rows in the column's list from other column lists
function cover_column!(c::DlxColumn)
    col_node = c.node

    # Remove column header from the header list
    col_node.right.left = col_node.left
    col_node.left.right = col_node.right

    # For each row in this column
    row_node = col_node.down
    while row_node !== col_node
        # For each node in this row (excluding the column itself)
        right_node = row_node.right
        while right_node !== row_node
            # Remove this node from its column
            right_node.down.up = right_node.up
            right_node.up.down = right_node.down
            right_node.column.size -= 1
            right_node = right_node.right
        end
        row_node = row_node.down
    end
end

# Uncover a column (exact reverse of cover)
function uncover_column!(c::DlxColumn)
    col_node = c.node

    # For each row in this column (in reverse order)
    row_node = col_node.up
    while row_node !== col_node
        # For each node in this row (in reverse order)
        left_node = row_node.left
        while left_node !== row_node
            # Restore this node to its column
            left_node.column.size += 1
            left_node.down.up = left_node
            left_node.up.down = left_node
            left_node = left_node.left
        end
        row_node = row_node.up
    end

    # Restore column header to the header list
    col_node.right.left = col_node
    col_node.left.right = col_node
end

# Choose column with minimum size (Knuth's S heuristic)
function choose_column(root::DlxColumn)::Union{DlxColumn, Nothing}
    root_node = root.node
    best = nothing
    min_size = typemax(Int)

    col_node = root_node.right
    while col_node !== root_node
        col = col_node.column
        if col.size < min_size
            min_size = col.size
            best = col
        end
        col_node = col_node.right
    end

    return best
end

# DLX Search - Algorithm X with Dancing Links
# Returns true if solution found, false otherwise
# solution[] stores the row indices of the solution
function dlx_search!(root::DlxColumn, k::Int, solution::Vector{Int})::Bool
    dlx_iterations[] += 1  # Count every search call

    root_node = root.node

    # If matrix is empty, we found a solution
    if root_node.right === root_node
        return true
    end

    # Choose column with minimum size
    col = choose_column(root)

    # If column has no rows, no solution possible
    if col === nothing || col.size == 0
        return false
    end

    # Cover this column
    cover_column!(col)

    # Try each row in this column
    row_node = col.node.down
    while row_node !== col.node
        # Add row to partial solution
        solution[k] = row_node.row_id  # Store row ID

        # Cover all other columns in this row
        right_node = row_node.right
        while right_node !== row_node
            cover_column!(right_node.column)
            right_node = right_node.right
        end

        # Recurse
        if dlx_search!(root, k + 1, solution)
            return true  # Solution found
        end

        # Backtrack: uncover all columns in this row
        left_node = row_node.left
        while left_node !== row_node
            uncover_column!(left_node.column)
            left_node = left_node.left
        end

        row_node = row_node.down
    end

    # Uncover column
    uncover_column!(col)

    return false  # No solution found
end

# ============================================================================
# SUDOKU EXACT COVER CONSTRUCTION
# ============================================================================

# Row metadata to map DLX rows back to Sudoku (row, col, num)
struct RowInfo
    row::Int
    col::Int
    num::Int
end

# Calculate constraint column indices (0-indexed for consistency with C)
# For a Sudoku cell (r,c) with value n (1-9):
# - Position constraint: (r,c) must be filled -> column index: r*9 + c
# - Row constraint: row r must have number n -> column index: 81 + r*9 + (n-1)
# - Column constraint: column c must have number n -> column index: 162 + c*9 + (n-1)
# - Box constraint: box b must have number n -> column index: 243 + b*9 + (n-1)
#   where b = (r/3)*3 + (c/3)

function get_position_col(r::Int, c::Int)::Int
    return r * 9 + c
end

function get_row_col(r::Int, n::Int)::Int
    return 81 + r * 9 + (n - 1)
end

function get_col_col(c::Int, n::Int)::Int
    return 162 + c * 9 + (n - 1)
end

function get_box_col(r::Int, c::Int, n::Int)::Int
    box = div(r, 3) * 3 + div(c, 3)
    return 243 + box * 9 + (n - 1)
end

# Initialize DLX matrix structure
function init_dlx_matrix()
    # Allocate root column
    root = DlxColumn("root")

    # Allocate 324 column headers
    columns = [DlxColumn("C$i") for i in 0:323]

    # Link columns into header list
    for col in columns
        col.node.left = root.node.left
        col.node.right = root.node
        root.node.left.right = col.node
        root.node.left = col.node
    end

    return root, columns
end

# Add a node to the DLX matrix
function add_node!(col::DlxColumn, row_id::Int, nodes::Vector{DlxNode})::DlxNode
    node = DlxNode()
    push!(nodes, node)
    node.column = col
    node.row_id = row_id

    # Insert at end of column's circular list
    node.down = col.node
    node.up = col.node.up
    col.node.up.down = node
    col.node.up = node
    col.size += 1

    return node
end

# Build a DLX row for Sudoku cell (r,c) with value n (0-indexed)
function build_dlx_row!(r::Int, c::Int, n::Int, row_id::Int,
                        columns::Vector{DlxColumn}, nodes::Vector{DlxNode},
                        row_info::Vector{RowInfo}, row_starts::Vector{Union{DlxNode, Nothing}})
    # Store row metadata
    push!(row_info, RowInfo(r, c, n))

    # Create nodes for the 4 constraints (add 1 for Julia 1-indexing of columns array)
    n1 = add_node!(columns[get_position_col(r, c) + 1], row_id, nodes)
    n2 = add_node!(columns[get_row_col(r, n) + 1], row_id, nodes)
    n3 = add_node!(columns[get_col_col(c, n) + 1], row_id, nodes)
    n4 = add_node!(columns[get_box_col(r, c, n) + 1], row_id, nodes)

    # Link nodes horizontally in circular list
    n1.right = n2
    n2.right = n3
    n3.right = n4
    n4.right = n1

    n1.left = n4
    n2.left = n1
    n3.left = n2
    n4.left = n3

    # Store first node for this row
    push!(row_starts, n1)
end

# Build the complete DLX matrix from the puzzle
function build_dlx_matrix_from_puzzle(puzzle::Matrix{Int})
    root, columns = init_dlx_matrix()
    nodes = DlxNode[]
    row_info = RowInfo[]
    row_starts = Union{DlxNode, Nothing}[]

    row_id = 0

    for r in 0:8
        for c in 0:8
            if puzzle[r+1, c+1] != 0  # Convert to 1-indexed for Julia
                # Cell has a clue - create only one row for that value
                build_dlx_row!(r, c, puzzle[r+1, c+1], row_id, columns, nodes, row_info, row_starts)
                row_id += 1
            else
                # Cell is empty - create rows for all possible values
                for n in 1:9
                    build_dlx_row!(r, c, n, row_id, columns, nodes, row_info, row_starts)
                    row_id += 1
                end
            end
        end
    end

    return root, columns, row_info, row_starts
end

# Cover given clues (pre-selected rows)
function cover_clues!(puzzle::Matrix{Int}, root::DlxColumn, row_info::Vector{RowInfo},
                      row_starts::Vector{Union{DlxNode, Nothing}})
    for r in 0:8
        for c in 0:8
            if puzzle[r+1, c+1] != 0  # Convert to 1-indexed
                n = puzzle[r+1, c+1]

                # Find the row for this clue
                for (idx, info) in enumerate(row_info)
                    if info.row == r && info.col == c && info.num == n
                        # Cover all columns in this row
                        node = row_starts[idx]
                        if node !== nothing
                            curr = node
                            while true
                                cover_column!(curr.column)
                                curr = curr.right
                                if curr === node
                                    break
                                end
                            end
                        end
                        break
                    end
                end
            end
        end
    end
end

# Extract solution from DLX and populate solution_grid
function extract_solution(solution::Vector{Int}, puzzle::Matrix{Int},
                         row_info::Vector{RowInfo})::Matrix{Int}
    # Initialize solution grid - start with the original puzzle (includes clues)
    solution_grid = copy(puzzle)

    # Each solution entry is a row_id
    for row_id in solution
        if row_id >= 0 && row_id < length(row_info)
            info = row_info[row_id + 1]  # Convert to 1-indexed
            solution_grid[info.row + 1, info.col + 1] = info.num  # Convert to 1-indexed
        end
    end

    return solution_grid
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

    # Process each .matrix file from command line
    for arg in ARGS
        if endswith(arg, ".matrix")
            # Read puzzle
            puzzle = read_matrix_file(arg)

            print_puzzle(puzzle)

            # Initialize DLX matrix
            root, columns, row_info, row_starts = build_dlx_matrix_from_puzzle(puzzle)

            # Cover pre-filled clues
            cover_clues!(puzzle, root, row_info, row_starts)

            # Solve using DLX
            dlx_iterations[] = 0
            solution = zeros(Int, 81)
            result = dlx_search!(root, 1, solution)  # Julia uses 1-based indexing

            if result
                solution_grid = extract_solution(solution, puzzle, row_info)
                print_puzzle(solution_grid)
                println("\nSolved in Iterations=$(dlx_iterations[])\n")
            else
                println("\nNo solution found\n")
            end
        end
    end

    elapsed = time() - start_time
    @printf("Seconds to process %.3f\n", elapsed)
end

main()
