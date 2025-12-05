/* Sudoku Solver in Rexx */
parse arg inputFile

if inputFile = "" then do
    say "Usage: rexx Sudoku.rexx <input_file>"
    exit 1
end

/* Read file */
board. = 0
row = 1
do while lines(inputFile) > 0
    line = linein(inputFile)
    if length(line) > 0 then do
        do col = 1 to 9
            char = substr(line, col, 1)
            if datatype(char, 'N') then board.row.col = char
            else board.row.col = 0
        end
        row = row + 1
    end
end
call lineout inputFile /* Close file */

iterations = 0
if solve() then do
    say "Solved in Iterations=" iterations
    call printBoard
end
else do
    say "No solution found."
end
exit 0

solve:
    procedure expose board. iterations
    
    /* Find empty cell */
    found = 0
    do r = 1 to 9
        do c = 1 to 9
            if board.r.c = 0 then do
                found = 1
                row = r
                col = c
                leave r
            end
        end
    end
    
    if found = 0 then return 1 /* Solved */
    
    do num = 1 to 9
        if isValid(row, col, num) then do
            board.row.col = num
            iterations = iterations + 1
            
            if solve() then return 1
            
            board.row.col = 0 /* Backtrack */
        end
    end
    
    return 0

isValid:
    procedure expose board.
    parse arg r, c, n
    
    /* Check row */
    do j = 1 to 9
        if board.r.j = n then return 0
    end
    
    /* Check column */
    do i = 1 to 9
        if board.i.c = n then return 0
    end
    
    /* Check 3x3 box */
    startRow = ((r - 1) % 3) * 3 + 1
    startCol = ((c - 1) % 3) * 3 + 1
    
    do i = 0 to 2
        do j = 0 to 2
            rr = startRow + i
            cc = startCol + j
            if board.rr.cc = n then return 0
        end
    end
    
    return 1

printBoard:
    procedure expose board.
    do r = 1 to 9
        line = ""
        do c = 1 to 9
            line = line || board.r.c
            if c < 9 then line = line || ""
        end
        say line
    end
    return
