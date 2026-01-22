-- Constraint Propagation (CP) Sudoku Solver Library
-- Lua helper for Fennel implementation
-- Based on C reference

local M = {}

-- Global iteration counter
M.cp_iterations = 0

-- ============================================================================
-- BITSET OPERATIONS
-- ============================================================================

-- Lua 5.2+ has bit32 library, but for compatibility we implement bitwise ops
local function bit_and(a, b)
    if bit32 then
        return bit32.band(a, b)
    end
    -- Manual implementation for Lua 5.1
    local result = 0
    local bit = 1
    while a > 0 or b > 0 do
        if (a % 2 == 1) and (b % 2 == 1) then
            result = result + bit
        end
        a = math.floor(a / 2)
        b = math.floor(b / 2)
        bit = bit * 2
    end
    return result
end

local function bit_or(a, b)
    if bit32 then
        return bit32.bor(a, b)
    end
    -- Manual implementation for Lua 5.1
    local result = 0
    local bit = 1
    while a > 0 or b > 0 do
        if (a % 2 == 1) or (b % 2 == 1) then
            result = result + bit
        end
        a = math.floor(a / 2)
        b = math.floor(b / 2)
        bit = bit * 2
    end
    return result
end

local function bit_not(a, bits)
    if bit32 then
        return bit32.band(bit32.bnot(a), (2^bits) - 1)
    end
    -- Manual implementation for Lua 5.1
    local result = 0
    local bit = 1
    for i = 0, bits - 1 do
        if a % 2 == 0 then
            result = result + bit
        end
        a = math.floor(a / 2)
        bit = bit * 2
    end
    return result
end

local function bit_lshift(a, n)
    if bit32 then
        return bit32.lshift(a, n)
    end
    return math.floor(a * (2 ^ n))
end

-- Candidate bitset operations
local function has_candidate(set, digit)
    return bit_and(set, bit_lshift(1, digit)) ~= 0
end

local function add_candidate(set, digit)
    return bit_or(set, bit_lshift(1, digit))
end

local function remove_candidate(set, digit)
    return bit_and(set, bit_not(bit_lshift(1, digit), 16))
end

local function count_candidates(set)
    -- Count set bits (popcount)
    local count = 0
    while set > 0 do
        if set % 2 == 1 then
            count = count + 1
        end
        set = math.floor(set / 2)
        set = math.floor(set) -- ensure integer
    end
    return count
end

local function get_first_candidate(cs)
    for digit = 1, 9 do
        if has_candidate(cs, digit) then
            return digit
        end
    end
    return 0
end

-- ============================================================================
-- GRID STRUCTURE AND HELPERS
-- ============================================================================

-- Create a new CP grid
local function new_grid()
    local grid = {
        values = {},
        candidates = {}
    }
    for r = 0, 8 do
        grid.values[r] = {}
        grid.candidates[r] = {}
        for c = 0, 8 do
            grid.values[r][c] = 0
            grid.candidates[r][c] = 0
        end
    end
    return grid
end

-- Deep copy a grid
local function copy_grid(grid)
    local new = new_grid()
    for r = 0, 8 do
        for c = 0, 8 do
            new.values[r][c] = grid.values[r][c]
            new.candidates[r][c] = grid.candidates[r][c]
        end
    end
    return new
end

-- Get all 20 peers for a cell (row, col, box)
local function get_peers(row, col)
    local peers = {}
    local idx = 1

    -- Same row (8 cells excluding self)
    for c = 0, 8 do
        if c ~= col then
            peers[idx] = {row, c}
            idx = idx + 1
        end
    end

    -- Same column (8 cells excluding self)
    for r = 0, 8 do
        if r ~= row then
            peers[idx] = {r, col}
            idx = idx + 1
        end
    end

    -- Same 3x3 box (4 cells excluding self and already counted)
    local box_row = math.floor(row / 3) * 3
    local box_col = math.floor(col / 3) * 3
    for r = box_row, box_row + 2 do
        for c = box_col, box_col + 2 do
            if r ~= row and c ~= col then
                peers[idx] = {r, c}
                idx = idx + 1
            end
        end
    end

    return peers
end

-- ============================================================================
-- INITIALIZATION
-- ============================================================================

local function init_grid(grid, puzzle)
    for row = 0, 8 do
        for col = 0, 8 do
            if puzzle[row][col] == 0 then
                -- Empty cell: set all candidates 1-9
                grid.values[row][col] = 0
                grid.candidates[row][col] = 0x3FE  -- Binary: 0011 1111 1110 (bits 1-9)
            else
                -- Given clue: set single value
                local digit = puzzle[row][col]
                grid.values[row][col] = digit
                grid.candidates[row][col] = bit_lshift(1, digit)
            end
        end
    end
end

-- ============================================================================
-- CONSTRAINT PROPAGATION
-- ============================================================================

local eliminate, assign  -- Forward declarations

eliminate = function(grid, row, col, digit)
    -- Check if digit is already eliminated
    if not has_candidate(grid.candidates[row][col], digit) then
        return true  -- Already eliminated, no change
    end

    -- Remove digit from candidates
    grid.candidates[row][col] = remove_candidate(grid.candidates[row][col], digit)

    -- Check for contradiction (no candidates left)
    local remaining = count_candidates(grid.candidates[row][col])
    if remaining == 0 then
        return false  -- Contradiction
    end

    -- If only one candidate left, assign it (singleton elimination)
    if remaining == 1 and grid.values[row][col] == 0 then
        local last_digit = get_first_candidate(grid.candidates[row][col])
        if not assign(grid, row, col, last_digit) then
            return false  -- Assignment caused contradiction
        end
    end

    return true
end

assign = function(grid, row, col, digit)
    -- Increment iteration counter (this is our benchmark metric)
    M.cp_iterations = M.cp_iterations + 1

    -- Set value
    grid.values[row][col] = digit
    grid.candidates[row][col] = bit_lshift(1, digit)

    -- Eliminate digit from all peers
    local peers = get_peers(row, col)

    for i = 1, #peers do
        local peer_row = peers[i][1]
        local peer_col = peers[i][2]

        if not eliminate(grid, peer_row, peer_col, digit) then
            return false  -- Contradiction in peer elimination
        end
    end

    return true
end

local function propagate(grid)
    local changed = true

    while changed do
        changed = false

        -- Strategy 1: Singleton elimination
        -- If a cell has only one candidate, assign it
        for row = 0, 8 do
            for col = 0, 8 do
                if grid.values[row][col] == 0 then
                    local num_candidates = count_candidates(grid.candidates[row][col])
                    if num_candidates == 0 then
                        return false  -- Contradiction
                    end
                    if num_candidates == 1 then
                        local digit = get_first_candidate(grid.candidates[row][col])
                        if not assign(grid, row, col, digit) then
                            return false  -- Assignment caused contradiction
                        end
                        changed = true
                    end
                end
            end
        end

        -- Strategy 2: Hidden singles
        -- For each unit (row, col, box), if a digit appears in only one cell, assign it

        -- Check rows
        for row = 0, 8 do
            for digit = 1, 9 do
                local count = 0
                local last_col = -1
                for col = 0, 8 do
                    if grid.values[row][col] == digit then
                        count = 0  -- Already assigned
                        break
                    end
                    if has_candidate(grid.candidates[row][col], digit) then
                        count = count + 1
                        last_col = col
                    end
                end
                if count == 1 then
                    if not assign(grid, row, last_col, digit) then
                        return false
                    end
                    changed = true
                elseif count == 0 then
                    -- Check if digit is already assigned in this row
                    local found = false
                    for col = 0, 8 do
                        if grid.values[row][col] == digit then
                            found = true
                            break
                        end
                    end
                    if not found then
                        return false  -- Digit cannot be placed anywhere in row
                    end
                end
            end
        end

        -- Check columns
        for col = 0, 8 do
            for digit = 1, 9 do
                local count = 0
                local last_row = -1
                for row = 0, 8 do
                    if grid.values[row][col] == digit then
                        count = 0  -- Already assigned
                        break
                    end
                    if has_candidate(grid.candidates[row][col], digit) then
                        count = count + 1
                        last_row = row
                    end
                end
                if count == 1 then
                    if not assign(grid, last_row, col, digit) then
                        return false
                    end
                    changed = true
                elseif count == 0 then
                    -- Check if digit is already assigned in this column
                    local found = false
                    for row = 0, 8 do
                        if grid.values[row][col] == digit then
                            found = true
                            break
                        end
                    end
                    if not found then
                        return false  -- Digit cannot be placed anywhere in column
                    end
                end
            end
        end

        -- Check boxes
        for box = 0, 8 do
            local box_row = math.floor(box / 3) * 3
            local box_col = (box % 3) * 3

            for digit = 1, 9 do
                local count = 0
                local last_r, last_c = -1, -1

                for r = box_row, box_row + 2 do
                    for c = box_col, box_col + 2 do
                        if grid.values[r][c] == digit then
                            count = 0  -- Already assigned
                            goto next_box_digit
                        end
                        if has_candidate(grid.candidates[r][c], digit) then
                            count = count + 1
                            last_r = r
                            last_c = c
                        end
                    end
                end

                if count == 1 then
                    if not assign(grid, last_r, last_c, digit) then
                        return false
                    end
                    changed = true
                elseif count == 0 then
                    -- Check if digit is already assigned in this box
                    local found = false
                    for r = box_row, box_row + 2 do
                        for c = box_col, box_col + 2 do
                            if grid.values[r][c] == digit then
                                found = true
                                goto found_box_digit
                            end
                        end
                    end
                    ::found_box_digit::
                    if not found then
                        return false  -- Digit cannot be placed anywhere in box
                    end
                end

                ::next_box_digit::
            end
        end
    end

    return true  -- Success - reached fixpoint
end

-- ============================================================================
-- SEARCH
-- ============================================================================

local function find_mrv_cell(grid)
    local min_candidates = 10  -- More than 9
    local found = false
    local best_row, best_col = -1, -1

    for r = 0, 8 do
        for c = 0, 8 do
            if grid.values[r][c] == 0 then
                local num_candidates = count_candidates(grid.candidates[r][c])
                if num_candidates < min_candidates then
                    min_candidates = num_candidates
                    best_row = r
                    best_col = c
                    found = true
                end
            end
        end
    end

    if found then
        return best_row, best_col
    else
        return nil, nil
    end
end

local function cp_search(grid, solution)
    -- Base case: check if grid is complete
    local mrv_row, mrv_col = find_mrv_cell(grid)
    if not mrv_row then
        -- No empty cells - grid is complete, extract solution
        for r = 0, 8 do
            for c = 0, 8 do
                solution[r * 9 + c] = grid.values[r][c]
            end
        end
        return true  -- Solved
    end

    -- Recursive case: try each candidate for the MRV cell
    local candidates = grid.candidates[mrv_row][mrv_col]

    for digit = 1, 9 do
        if has_candidate(candidates, digit) then
            -- Save grid state for backtracking
            local grid_copy = copy_grid(grid)

            -- Try assigning this digit
            if assign(grid, mrv_row, mrv_col, digit) then
                -- Assignment succeeded, propagate constraints
                if propagate(grid) then
                    -- Propagation succeeded, recurse
                    if cp_search(grid, solution) then
                        return true  -- Found solution
                    end
                end
            end

            -- Failed - restore grid state and try next candidate
            grid.values = grid_copy.values
            grid.candidates = grid_copy.candidates
        end
    end

    -- All candidates exhausted - dead end
    return false
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
    print(display_path)

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
                for i = 1, 9 do
                    io.write(values[i] .. " ")
                end
                io.write("\n")

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

    if #args < 1 then
        io.stderr:write("Usage: lua cp.lua <matrix_file>\n")
        return 1
    end

    local filename = args[1]

    -- Read puzzle from file
    local puzzle = read_matrix_file(filename)
    if not puzzle then
        io.stderr:write("Failed to read matrix file\n")
        return 1
    end

    -- Print initial puzzle
    print_puzzle(puzzle)

    -- Initialize CP grid
    local grid = new_grid()
    init_grid(grid, puzzle)

    -- Apply initial propagation
    if not propagate(grid) then
        io.write("\nNo solution found (contradiction during initial propagation)\n")
        local end_time = os.clock()
        print(string.format("Seconds to process %.3f", end_time - start_time))
        return 0
    end

    -- Run search
    M.cp_iterations = 0
    local solution = {}
    local solved = cp_search(grid, solution)

    if solved then
        -- Convert solution array back to 2D for printing
        local solution_grid = {}
        for r = 0, 8 do
            solution_grid[r] = {}
            for c = 0, 8 do
                solution_grid[r][c] = solution[r * 9 + c]
            end
        end

        print_puzzle(solution_grid)
        io.write("\nSolved in Iterations=" .. M.cp_iterations .. "\n\n")
    else
        io.write("\nNo solution found\n")
    end

    local end_time = os.clock()
    print(string.format("Seconds to process %.3f", end_time - start_time))

    return 0
end

return M
