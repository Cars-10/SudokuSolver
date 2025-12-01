local puzzle = {}
local iterations = 0

function print_board()
    print("Puzzle:")
    for i = 1, 9 do
        local row_str = ""
        for j = 1, 9 do
            row_str = row_str .. puzzle[i][j] .. " "
        end
        print(row_str)
    end
end

function is_possible(row, col, num)
    for i = 1, 9 do
        if puzzle[row][i] == num then return false end
        if puzzle[i][col] == num then return false end
    end
    
    local start_row = math.floor((row - 1) / 3) * 3 + 1
    local start_col = math.floor((col - 1) / 3) * 3 + 1
    
    for i = 0, 2 do
        for j = 0, 2 do
            if puzzle[start_row + i][start_col + j] == num then return false end
        end
    end
    return true
end

function solve(row, col)
    if row == 10 then return true end
    
    local next_row = row
    local next_col = col + 1
    if next_col == 10 then
        next_row = row + 1
        next_col = 1
    end
    
    if puzzle[row][col] ~= 0 then
        return solve(next_row, next_col)
    end
    
    for num = 1, 9 do
        iterations = iterations + 1
        if is_possible(row, col, num) then
            puzzle[row][col] = num
            if solve(next_row, next_col) then return true end
            puzzle[row][col] = 0
        end
    end
    return false
end

function read_board(filename)
    puzzle = {}
    local file = io.open(filename, "r")
    if not file then
        print("Error reading file " .. filename)
        return false
    end
    
    local row = 1
    for line in file:lines() do
        if line:match("%S") and not line:match("^#") then
            puzzle[row] = {}
            local col = 1
            for num in line:gmatch("%S+") do
                if col <= 9 then
                    puzzle[row][col] = tonumber(num)
                    col = col + 1
                end
            end
            row = row + 1
            if row > 9 then break end
        end
    end
    file:close()
    return true
end

if #arg < 1 then
    print("Usage: lua Sudoku.lua <file1> <file2> ...")
    os.exit(1)
end

for _, filename in ipairs(arg) do
    print("\nProcessing " .. filename)
    if read_board(filename) then
        print_board()
        iterations = 0
        if solve(1, 1) then
            print_board()
            print("\nSolved in Iterations=" .. iterations)
        else
            print("No solution found")
        end
    end
end
