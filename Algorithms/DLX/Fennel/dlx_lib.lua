-- Dancing Links (DLX) Algorithm X Sudoku Solver Library
-- Lua helper for Fennel implementation
-- Based on C reference

local M = {}

-- Global iteration counter
M.dlx_iterations = 0

-- ============================================================================
-- DATA STRUCTURES (using Lua tables)
-- ============================================================================

-- DlxNode structure
local function new_node()
    return {
        up = nil,
        down = nil,
        left = nil,
        right = nil,
        column = nil,
        row_id = -1
    }
end

-- DlxColumn structure (inherits from node)
local function new_column(name)
    local col = new_node()
    col.size = 0
    col.name = name or ""
    return col
end

-- ============================================================================
-- DLX CORE OPERATIONS
-- ============================================================================

-- Cover a column (remove from matrix)
local function cover_column(c)
    local col_node = c

    -- Remove column header from header list
    col_node.right.left = col_node.left
    col_node.left.right = col_node.right

    -- For each row in this column
    local row_node = col_node.down
    while row_node ~= col_node do
        -- For each node in this row
        local right_node = row_node.right
        while right_node ~= row_node do
            -- Remove this node from its column
            right_node.down.up = right_node.up
            right_node.up.down = right_node.down
            right_node.column.size = right_node.column.size - 1
            right_node = right_node.right
        end
        row_node = row_node.down
    end
end

-- Uncover a column (exact reverse of cover)
local function uncover_column(c)
    local col_node = c

    -- For each row in this column (in reverse order)
    local row_node = col_node.up
    while row_node ~= col_node do
        -- For each node in this row (in reverse order)
        local left_node = row_node.left
        while left_node ~= row_node do
            -- Restore this node to its column
            left_node.column.size = left_node.column.size + 1
            left_node.down.up = left_node
            left_node.up.down = left_node
            left_node = left_node.left
        end
        row_node = row_node.up
    end

    -- Restore column header to header list
    col_node.right.left = col_node
    col_node.left.right = col_node
end

-- Choose column with minimum size (Knuth's S heuristic)
local function choose_column(root)
    local best = nil
    local min_size = math.huge

    local col_node = root.right
    while col_node ~= root do
        if col_node.size < min_size then
            min_size = col_node.size
            best = col_node
        end
        col_node = col_node.right
    end

    return best
end

-- DLX Search - Algorithm X with Dancing Links
local function dlx_search(root, k, solution)
    M.dlx_iterations = M.dlx_iterations + 1  -- Count every search call

    -- If matrix is empty, we found a solution
    if root.right == root then
        return true
    end

    -- Choose column with minimum size
    local col = choose_column(root)

    -- If column has no rows, no solution possible
    if col.size == 0 then
        return false
    end

    -- Cover this column
    cover_column(col)

    -- Try each row in this column
    local row_node = col.down
    while row_node ~= col do
        -- Add row to partial solution
        solution[k] = row_node.row_id

        -- Cover all other columns in this row
        local right_node = row_node.right
        while right_node ~= row_node do
            cover_column(right_node.column)
            right_node = right_node.right
        end

        -- Recurse
        if dlx_search(root, k + 1, solution) then
            return true  -- Solution found
        end

        -- Backtrack: uncover all columns in this row
        local left_node = row_node.left
        while left_node ~= row_node do
            uncover_column(left_node.column)
            left_node = left_node.left
        end

        row_node = row_node.down
    end

    -- Uncover column
    uncover_column(col)

    return false  -- No solution found
end

-- ============================================================================
-- SUDOKU-SPECIFIC FUNCTIONS
-- ============================================================================

-- Calculate constraint column indices
local function get_position_col(r, c)
    return r * 9 + c
end

local function get_row_col(r, n)
    return 81 + r * 9 + (n - 1)
end

local function get_col_col(c, n)
    return 162 + c * 9 + (n - 1)
end

local function get_box_col(r, c, n)
    local box = math.floor(r / 3) * 3 + math.floor(c / 3)
    return 243 + box * 9 + (n - 1)
end

-- Initialize DLX matrix structure
local function init_dlx_matrix()
    -- Create root column
    local root = new_column("root")
    root.left = root
    root.right = root
    root.up = root
    root.down = root
    root.column = root
    root.row_id = -1

    -- Create 324 column headers
    local columns = {}
    for i = 0, 323 do
        local col = new_column("C" .. i)
        col.up = col
        col.down = col
        col.column = col
        col.row_id = -1

        -- Link into header list (add to left of root)
        col.left = root.left
        col.right = root
        root.left.right = col
        root.left = col

        columns[i] = col
    end

    return root, columns
end

-- Add a node to the DLX matrix
local function add_node(col, row_id)
    local node = new_node()
    node.column = col
    node.row_id = row_id

    -- Insert at end of column's circular list
    node.down = col
    node.up = col.up
    col.up.down = node
    col.up = node
    col.size = col.size + 1

    return node
end

-- Build a DLX row for Sudoku cell (r,c) with value n
local function build_dlx_row(r, c, n, row_id, columns, row_info)
    -- Store row metadata (using 0-based indexing for consistency)
    row_info[row_id] = {row = r, col = c, num = n}

    -- Create nodes for the 4 constraints
    local n1 = add_node(columns[get_position_col(r, c)], row_id)
    local n2 = add_node(columns[get_row_col(r, n)], row_id)
    local n3 = add_node(columns[get_col_col(c, n)], row_id)
    local n4 = add_node(columns[get_box_col(r, c, n)], row_id)

    -- Link nodes horizontally in circular list
    n1.right = n2
    n2.right = n3
    n3.right = n4
    n4.right = n1

    n1.left = n4
    n2.left = n1
    n3.left = n2
    n4.left = n3

    return n1
end

-- Build the complete DLX matrix from puzzle
local function build_dlx_matrix_from_puzzle(puzzle, root, columns)
    local row_id = 0
    local row_info = {}
    local row_starts = {}

    for r = 0, 8 do
        for c = 0, 8 do
            if puzzle[r][c] ~= 0 then
                -- Cell has a clue - create only one row for that value
                local first_node = build_dlx_row(r, c, puzzle[r][c], row_id, columns, row_info)
                row_starts[row_id] = first_node
                row_id = row_id + 1
            else
                -- Cell is empty - create rows for all possible values
                for n = 1, 9 do
                    local first_node = build_dlx_row(r, c, n, row_id, columns, row_info)
                    row_starts[row_id] = first_node
                    row_id = row_id + 1
                end
            end
        end
    end

    return row_info, row_starts
end

-- Cover given clues (pre-selected rows)
local function cover_clues(puzzle, row_info, row_starts)
    for r = 0, 8 do
        for c = 0, 8 do
            if puzzle[r][c] ~= 0 then
                local n = puzzle[r][c]

                -- Find the row for this clue
                for row_id = 0, 728 do
                    if row_starts[row_id] and row_info[row_id] and
                       row_info[row_id].row == r and
                       row_info[row_id].col == c and
                       row_info[row_id].num == n then

                        -- Cover all columns in this row
                        local node = row_starts[row_id]
                        local curr = node
                        repeat
                            cover_column(curr.column)
                            curr = curr.right
                        until curr == node
                        break
                    end
                end
            end
        end
    end
end

-- Extract solution from DLX
local function extract_solution(solution, solution_len, row_info, puzzle)
    -- Start with original puzzle (includes clues)
    local solution_grid = {}
    for r = 0, 8 do
        solution_grid[r] = {}
        for c = 0, 8 do
            solution_grid[r][c] = puzzle[r][c]
        end
    end

    -- Apply solution rows
    for i = 0, solution_len - 1 do
        local row_id = solution[i]
        if row_id and row_id >= 0 and row_id < 729 and row_info[row_id] then
            local info = row_info[row_id]
            solution_grid[info.row][info.col] = info.num
        end
    end

    return solution_grid
end

-- ============================================================================
-- I/O FUNCTIONS
-- ============================================================================

-- Print puzzle
local function print_puzzle(grid)
    io.write("\nPuzzle:\n")
    for r = 0, 8 do
        for c = 0, 8 do
            io.write(grid[r][c] .. " ")
        end
        io.write("\n")
    end
end

-- Read matrix file
local function read_matrix_file(filename)
    local file = io.open(filename, "r")
    if not file then
        io.stderr:write("Error opening file '" .. filename .. "'\n")
        return nil
    end

    -- Print filename (normalize path if needed)
    local display_path = filename
    if filename:match("^/app/Matrices/") then
        display_path = "../" .. filename:sub(6)  -- Skip "/app/"
    end
    -- print(display_path) -- handled by main loop print in reference, but let's check

    local puzzle = {}
    local line_count = 0

    for line in file:lines() do
        local trimmed = line:match("^%s*(.-)%s*$")
        if trimmed ~= "" and trimmed:sub(1, 1) ~= "#" then
            local values = {}
            for num in trimmed:gmatch("%d+") do
                table.insert(values, tonumber(num))
            end

            if #values == 9 then
                -- Store in puzzle (0-indexed for consistency with C)
                puzzle[line_count] = {}
                for i = 1, 9 do
                    puzzle[line_count][i - 1] = values[i]
                end

                -- Print raw row
                -- for i = 1, 9 do
                --     io.write(values[i] .. " ")
                -- end
                -- io.write("\n")

                line_count = line_count + 1
                if line_count >= 9 then
                    break
                end
            else
                io.stderr:write("Error: line does not contain 9 integers\n")
                file:close()
                return nil
            end
        end
    end

    file:close()

    if line_count ~= 9 then
        io.stderr:write("Error: file does not contain 9 rows\n")
        return nil
    end

    return puzzle
end

-- ============================================================================
-- MAIN EXPORT
-- ============================================================================

function M.main(args)
    local start_time = os.clock()

    -- Process each .matrix file from command line
    for i = 1, #args do
        local filename = args[i]
        if filename:match("%.matrix$") then
            
            -- Print display path (mirroring reference logic)
            local display_path = filename
            if filename:match("^/app/Matrices/") then
                display_path = "../" .. filename:sub(6)
            end
            print(display_path)

            -- Read puzzle
            local puzzle = read_matrix_file(filename)
            if not puzzle then
                io.stderr:write("Error reading " .. filename .. "\n")
            else
                -- Print initial puzzle (raw rows)
                for r=0,8 do
                    for c=0,8 do
                         io.write(puzzle[r][c] .. " ")
                    end
                    io.write("\n")
                end

                -- Print initial puzzle text
                print_puzzle(puzzle)

                -- Initialize DLX matrix
                local root, columns = init_dlx_matrix()

                -- Build matrix from puzzle
                local row_info, row_starts = build_dlx_matrix_from_puzzle(puzzle, root, columns)

                -- Cover pre-filled clues
                cover_clues(puzzle, row_info, row_starts)

                -- Solve using DLX
                M.dlx_iterations = 0
                local solution = {}
                local result = dlx_search(root, 0, solution)

                if result then
                    local solution_grid = extract_solution(solution, 81, row_info, puzzle)
                    print_puzzle(solution_grid)
                    io.write("\nSolved in Iterations=" .. M.dlx_iterations .. "\n") -- removed double newline to match others if needed, typically C has it... reference has \n\n.
                    io.write("\n") -- explicit extra newline
                else
                    io.write("\nNo solution found\n")
                end
            end
        end
    end

    local end_time = os.clock()
    print(string.format("Seconds to process %.3f", end_time - start_time))
end

return M
