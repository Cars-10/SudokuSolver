-- Sudoku Solver in Lua
-- Brute-force algorithm matching C reference exactly
-- Counts EVERY placement attempt (not just valid ones)

-- Global variables
local puzzle = {}  -- 9x9 grid (1-indexed in Lua)
local count = 0    -- Iteration counter

-- Initialize empty puzzle grid
local function initPuzzle()
    for i = 1, 9 do
        puzzle[i] = {}
        for j = 1, 9 do
            puzzle[i][j] = 0
        end
    end
end

-- Print puzzle in standard format
local function printPuzzle()
    io.write("\nPuzzle:\n")
    for row = 1, 9 do
        for col = 1, 9 do
            io.write(puzzle[row][col] .. " ")
        end
        io.write("\n")
    end
end

-- Read matrix file and populate puzzle
local function readMatrixFile(filename)
    local file = io.open(filename, "r")
    if not file then
        io.stderr:write("Error opening file '" .. filename .. "'\n")
        return false
    end

    -- Print filename
    print(filename)

    local lineCount = 0
    for line in file:lines() do
        -- Skip comments and empty lines
        local trimmed = line:match("^%s*(.-)%s*$")  -- trim whitespace
        if trimmed ~= "" and trimmed:sub(1, 1) ~= "#" then
            -- Parse 9 integers from line
            local values = {}
            for num in trimmed:gmatch("%d+") do
                table.insert(values, tonumber(num))
            end

            if #values == 9 then
                lineCount = lineCount + 1
                if lineCount <= 9 then
                    -- Store in puzzle (1-indexed)
                    for i = 1, 9 do
                        puzzle[lineCount][i] = values[i]
                    end
                    -- Print raw row as read
                    for i = 1, 9 do
                        io.write(values[i] .. " ")
                    end
                    io.write("\n")
                end
            else
                io.stderr:write("Error: line does not contain 9 integers\n")
                file:close()
                return false
            end
        end
    end

    file:close()
    return true
end

-- Check if placing val at (row, col) is valid
local function isValid(row, col, val)
    -- Check row
    for i = 1, 9 do
        if puzzle[row][i] == val then
            return false
        end
    end

    -- Check column
    for i = 1, 9 do
        if puzzle[i][col] == val then
            return false
        end
    end

    -- Check 3x3 box
    local boxRow = math.floor((row - 1) / 3) * 3 + 1
    local boxCol = math.floor((col - 1) / 3) * 3 + 1
    for i = 0, 2 do
        for j = 0, 2 do
            if puzzle[boxRow + i][boxCol + j] == val then
                return false
            end
        end
    end

    return true
end

-- BRUTE-FORCE SOLVER
-- Searches row-major order (top-to-bottom, left-to-right)
-- Tries candidates 1-9 in ascending order
-- Counts EVERY placement attempt (the algorithm fingerprint)
local function solve()
    -- Find first empty cell (row-major order)
    local emptyRow, emptyCol = nil, nil
    for r = 1, 9 do
        for c = 1, 9 do
            if puzzle[r][c] == 0 then
                emptyRow = r
                emptyCol = c
                goto found_empty
            end
        end
    end

    ::found_empty::

    -- If no empty cell found, puzzle is solved
    if not emptyRow then
        printPuzzle()
        io.write("\nSolved in Iterations=" .. count .. "\n\n")
        return true  -- Success
    end

    -- Try values 1-9 in order
    for val = 1, 9 do
        count = count + 1  -- COUNT EVERY ATTEMPT - this is the algorithm fingerprint

        if isValid(emptyRow, emptyCol, val) then
            puzzle[emptyRow][emptyCol] = val  -- Place value

            if solve() then
                return true  -- Solved
            end

            puzzle[emptyRow][emptyCol] = 0  -- Backtrack
        end
    end

    return false  -- No solution found
end

-- Main execution
local function main()
    local startTime = os.clock()

    -- Process each .matrix file from command line
    for i = 1, #arg do
        local filename = arg[i]
        if filename:match("%.matrix$") then
            initPuzzle()

            if readMatrixFile(filename) then
                printPuzzle()
                count = 0
                solve()
            else
                io.stderr:write("Error reading " .. filename .. "\n")
            end
        end
    end

    local endTime = os.clock()
    print(string.format("Seconds to process %.3f", endTime - startTime))
end

-- Run main
main()
