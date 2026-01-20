-- Constraint Propagation Sudoku Solver in AppleScript
-- Collects output and returns it for standard benchmark capture

global cp_iterations, board, candidates, outputLog

on run argv
    if (count of argv) < 1 then
        return "Usage: osascript cp.applescript <matrix_file>"
    end if
    
    set matrixFile to item 1 of argv
    set outputLog to ""
    
    -- Pre-allocate global lists
    set board to ArrayOfSize(81, 0)
    set candidates to ArrayOfSize(81, 1022) -- 0x3FE
    set cp_iterations to 0
    
    solveSudoku(matrixFile)
    return outputLog
end run

on logOut(msg)
    set outputLog to outputLog & msg & return
end logOut

on ArrayOfSize(n, val)
    set lst to {}
    repeat n times
        set end of lst to val
    end repeat
    return lst
end ArrayOfSize

on solveSudoku(filename)
    -- Read puzzle
    set puzzle to {}
    repeat 81 times
        set end of puzzle to 0
    end repeat
    
    try
        set fileContent to do shell script "cat " & quoted form of filename
        set allWords to words of fileContent
        set wordCount to 0
        repeat with w in allWords
            try
                set val to w as integer
                set wordCount to wordCount + 1
                set item wordCount of puzzle to val
            end try
            if wordCount = 81 then exit repeat
        end repeat
    on error
        logOut("Error reading file")
        return false
    end try
    
    logOut(filename)
    logOut("")
    logOut("Puzzle:")
    printGrid(puzzle)
    
    -- Dirty init
    repeat with i from 1 to 81
        set val to item i of puzzle
        if val > 0 then
            set item i of board to val
            set item i of candidates to (2 ^ val)
        end if
    end repeat
    
    set cp_iterations to 0
    
    if propagate() then
        if cpSearch() then
            logOut("")
            logOut("Puzzle:")
            printGrid(board)
            logOut("")
            logOut("Solved in Iterations=" & cp_iterations)
            return true
        end if
    end if
    
    logOut("")
    logOut("No solution found")
    return false
end solveSudoku

on printGrid(grid)
    repeat with r from 0 to 8
        set rowLine to ""
        repeat with c from 0 to 8
            set rowLine to rowLine & (item (r * 9 + c + 1) of grid) & " "
        end repeat
        logOut(rowLine)
    end repeat
end printGrid

on hasCand(mask, digit)
    return ((mask div (2 ^ digit)) mod 2) = 1
end hasCand

on countCand(mask)
    set c to 0
    repeat with i from 1 to 9
        if ((mask div (2 ^ i)) mod 2) = 1 then set c to c + 1
    end repeat
    return c
end countCand

on removeCand(mask, digit)
    set p to 2 ^ digit
    if ((mask div p) mod 2) = 1 then
        return mask - p
    else
        return mask
    end if
end removeCand

on getFirstCand(mask)
    repeat with i from 1 to 9
        if ((mask div (2 ^ i)) mod 2) = 1 then return i
    end repeat
    return 0
end getFirstCand

on assign(r, c, val)
    set cp_iterations to cp_iterations + 1
    set idx to r * 9 + c + 1
    set item idx of board to val
    set item idx of candidates to (2 ^ val)
    
    -- Row
    repeat with k from 0 to 8
        if k is not c then
            if not eliminate(r, k, val) then return false
        end if
    end repeat
    -- Col
    repeat with k from 0 to 8
        if k is not r then
            if not eliminate(k, c, val) then return false
        end if
    end repeat
    -- Box
    set br to (r div 3) * 3
    set bc to (c div 3) * 3
    repeat with bi from 0 to 2
        repeat with bj from 0 to 2
            set nr to br + bi
            set nc to bc + bj
            if nr is not r or nc is not c then
                if nr is not r and nc is not c then
                    if not eliminate(nr, nc, val) then return false
                end if
            end if
        end repeat
    end repeat
    return true
end assign

on eliminate(r, c, val)
    set idx to r * 9 + c + 1
    set mask to item idx of candidates
    if not hasCand(mask, val) then return true
    set mask to removeCand(mask, val)
    set item idx of candidates to mask
    set rem to countCand(mask)
    if rem = 0 then
        if (item idx of board) = val then return true
        return false
    end if
    if rem = 1 and (item idx of board) = 0 then
        set v to getFirstCand(mask)
        if not assign(r, c, v) then return false
    end if
    return true
end eliminate

on propagate()
    set changed to true
    repeat while changed
        set changed to false
        repeat with i from 0 to 80
            if (item (i + 1) of board) = 0 then
                set rem to countCand(item (i + 1) of candidates)
                if rem = 0 then return false
                if rem = 1 then
                    if not assign(i div 9, i mod 9, getFirstCand(item (i + 1) of candidates)) then return false
                    set changed to true
                end if
            end if
        end repeat
        
        -- Rows
        repeat with row from 0 to 8
            repeat with val from 1 to 9
                set matchCount to 0
                set lastCol to -1
                repeat with col from 0 to 8
                    if (item (row * 9 + col + 1) of board) = val then
                        set matchCount to 0
                        exit repeat
                    end if
                    if hasCand(item (row * 9 + col + 1) of candidates, val) then
                        set matchCount to matchCount + 1
                        set lastCol to col
                    end if
                end repeat
                if matchCount = 1 then
                    if not assign(row, lastCol, val) then return false
                    set changed to true
                else if matchCount = 0 then
                    set found to false
                    repeat with k from 0 to 8
                        if (item (row * 9 + k + 1) of board) = val then
                            set found to true
                            exit repeat
                        end if
                    end repeat
                    if not found then return false
                end if
            end repeat
        end repeat
        -- Cols
        repeat with col from 0 to 8
            repeat with val from 1 to 9
                set matchCount to 0
                set lastRow to -1
                repeat with row from 0 to 8
                    if (item (row * 9 + col + 1) of board) = val then
                        set matchCount to 0
                        exit repeat
                    end if
                    if hasCand(item (row * 9 + col + 1) of candidates, val) then
                        set matchCount to matchCount + 1
                        set lastRow to row
                    end if
                end repeat
                if matchCount = 1 then
                    if not assign(lastRow, col, val) then return false
                    set changed to true
                else if matchCount = 0 then
                    set found to false
                    repeat with k from 0 to 8
                        if (item (k * 9 + col + 1) of board) = val then
                            set found to true
                            exit repeat
                        end if
                    end repeat
                    if not found then return false
                end if
            end repeat
        end repeat
        -- Boxes
        repeat with bIdx from 0 to 8
            set br to (bIdx div 3) * 3
            set bc to (bIdx mod 3) * 3
            repeat with val from 1 to 9
                set matchCount to 0
                set lr to -1
                set lc to -1
                repeat with bi from 0 to 2
                    repeat with bj from 0 to 2
                        set r to br + bi
                        set c to bc + bj
                        if (item (r * 9 + c + 1) of board) = val then
                            set matchCount to 0
                            exit repeat
                        end if
                        if hasCand(item (r * 9 + c + 1) of candidates, val) then
                            set matchCount to matchCount + 1
                            set lr to r
                            set lc to c
                        end if
                    end repeat
                    if matchCount = 0 then exit repeat
                end repeat
                if matchCount = 1 then
                    if not assign(lr, lc, val) then return false
                    set changed to true
                else if matchCount = 0 then
                    set found to false
                    repeat with bi from 0 to 2
                        repeat with bj from 0 to 2
                            if (item ((br + bi) * 9 + (bc + bj) + 1) of board) = val then
                                set found to true
                                exit repeat
                            end if
                        end repeat
                        if found then exit repeat
                    end repeat
                    if not found then return false
                end if
            end repeat
        end repeat
    end repeat
    return true
end propagate

on cpSearch()
    set minC to 10
    set mr to -1
    set mc to -1
    repeat with i from 0 to 80
        if (item (i + 1) of board) = 0 then
            set c to countCand(item (i + 1) of candidates)
            if c < minC then
                set minC to c
                set mr to i div 9
                set mc to i mod 9
            end if
        end if
    end repeat
    if mr = -1 then return true
    set mask to item (mr * 9 + mc + 1) of candidates
    repeat with val from 1 to 9
        if hasCand(mask, val) then
            set savedBoard to items of board
            set savedCands to items of candidates
            if assign(mr, mc, val) then
                if propagate() then
                    if cpSearch() then return true
                end if
            end if
            set board to savedBoard
            set candidates to savedCands
        end if
    end repeat
    return false
end cpSearch
