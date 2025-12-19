-- Sudoku Solver in AppleScript
-- Exact match of C brute-force algorithm

global iterations
global board

on run argv
    if (count of argv) < 1 then
        return "Usage: osascript Sudoku.applescript <matrix_file>"
    end if
    
    set matrixFile to item 1 of argv
    set iterations to 0
    set board to {}
    repeat 81 times
        set end of board to 0
    end repeat
    
    if not readMatrix(matrixFile) then
        return "Error reading matrix file"
    end if
    
    -- Print filename to stdout
    do shell script "echo " & quoted form of matrixFile
    
    -- Print initial grid to stdout (match C reference)
    do shell script "echo \"\nPuzzle:\""
    repeat with r from 0 to 8
        set rowLine to ""
        repeat with c from 0 to 8
            set rowLine to rowLine & (item (r * 9 + c + 1) of board) & " "
        end repeat
        do shell script "echo " & quoted form of rowLine
    end repeat
    
    set iterations to 0
    if solve() then
        -- Print solved grid to stdout
        do shell script "echo \"\nPuzzle:\""
        repeat with r from 0 to 8
            set rowLine to ""
            repeat with c from 0 to 8
                set rowLine to rowLine & (item (r * 9 + c + 1) of board) & " "
            end repeat
            do shell script "echo " & quoted form of rowLine
        end repeat
        
        -- Print iterations to stdout
        do shell script "echo \"\nSolved in Iterations=" & iterations & "\""
    else
        do shell script "echo \"No solution found.\""
    end if
end run

on readMatrix(filename)
    try
        set fileContent to do shell script "cat " & quoted form of filename
        
        set allWords to words of fileContent
        set wordCount to 0
        repeat with w in allWords
            try
                set val to w as integer
                set wordCount to wordCount + 1
                set item wordCount of board to val
            end try
            if wordCount = 81 then exit repeat
        end repeat
        return true
    on error
        return false
    end try
end readMatrix

on isValid(r, c, val)
    -- Check row
    repeat with i from 0 to 8
        if (item (r * 9 + i + 1) of board) = val then return false
    end repeat
    
    -- Check col
    repeat with i from 0 to 8
        if (item (i * 9 + c + 1) of board) = val then return false
    end repeat
    
    -- Check box
    set br to (r div 3) * 3
    set bc to (c div 3) * 3
    repeat with i from 0 to 2
        repeat with j from 0 to 2
            if (item ((br + i) * 9 + (bc + j) + 1) of board) = val then return false
        end repeat
    end repeat
    
    return true
end isValid

on solve()
    set found to false
    set r to -1
    set c to -1
    
    -- Find first empty
    repeat with i from 0 to 80
        if (item (i + 1) of board) = 0 then
            set r to i div 9
            set c to i mod 9
            set found to true
            exit repeat
        end if
    end repeat
    
    if not found then return true
    
    repeat with val from 1 to 9
        set iterations to iterations + 1
        
        if isValid(r, c, val) then
            set item (r * 9 + c + 1) of board to val
            if solve() then return true
            set item (r * 9 + c + 1) of board to 0
        end if
    end repeat
    
    return false
end solve