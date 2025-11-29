local puzzle = {}
local count = 0
local DEBUG = 0

for i = 0, 8 do
    puzzle[i] = {}
    for j = 0, 8 do
        puzzle[i][j] = 0
    end
end

function printPuzzle()
    print("\nPuzzle:")
    for j = 0, 8 do
        local line = ""
        for i = 0, 8 do
            line = line .. puzzle[j][i] .. " "
        end
        print(line)
    end
end

function readMatrixFile(filename)
    print(filename)
    local file = io.open(filename, "r")
    if not file then
        print("Error opening file: " .. filename)
        return
    end

    local row = 0
    for line in file:lines() do
        if not (string.sub(line, 1, 1) == "#") and string.len(line) > 0 then
            local col = 0
            for val in string.gmatch(line, "%S+") do
                puzzle[row][col] = tonumber(val)
                col = col + 1
            end
            if col == 9 then
                row = row + 1
                if row == 9 then break end
            end
        end
    end
    file:close()
end

function isPossible(y, x, val)
    for i = 0, 8 do
        if puzzle[i][x] == val then return false end
        if puzzle[y][i] == val then return false end
    end

    local x0 = math.floor(x / 3) * 3
    local y0 = math.floor(y / 3) * 3

    for i = 0, 2 do
        for j = 0, 2 do
            if puzzle[y0 + i][x0 + j] == val then return false end
        end
    end
    return true
end

function solve()
    for j = 0, 8 do
        for i = 0, 8 do
            if puzzle[j][i] == 0 then
                for val = 1, 9 do
                    count = count + 1
                    if isPossible(j, i, val) then
                        puzzle[j][i] = val
                        if solve() == 2 then return 2 end
                        puzzle[j][i] = 0
                    end
                end
                return 0
            end
        end
    end
    printPuzzle()
    print(string.format("\nSolved in Iterations=%d\n", count))
    return 2
end

local start = os.clock()

for i = 1, #arg do
    if string.sub(arg[i], -7) == ".matrix" then
        readMatrixFile(arg[i])
        printPuzzle()
        count = 0
        solve()
    end
end

local elapsed = os.clock() - start
print(string.format("Seconds to process %.3f", elapsed))
