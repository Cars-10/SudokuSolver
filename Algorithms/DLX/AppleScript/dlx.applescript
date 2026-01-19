-- DLX Sudoku Solver in AppleScript
-- Collects output and returns it for standard benchmark capture

global h, s, l, r, u, d, c, o, nodes, updates, solution_found, outputLog

on run argv
    if (count of argv) < 1 then
        return "Usage: osascript dlx.applescript <matrix_file>"
    end if
    
    set matrixFile to item 1 of argv
    set outputLog to ""
    
    -- Pre-allocate global lists
    set maxNodes to 4000
    set h to ArrayOfSize(maxNodes, 0)
    set s to ArrayOfSize(maxNodes, 0)
    set l to ArrayOfSize(maxNodes, 0)
    set r to ArrayOfSize(maxNodes, 0)
    set u to ArrayOfSize(maxNodes, 0)
    set d to ArrayOfSize(maxNodes, 0)
    set c to ArrayOfSize(maxNodes, 0)
    set o to {}
    
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
    
    set updates to 0
    buildMatrix(puzzle)
    preCover(puzzle)
    set solution_found to false
    
    search(0)
    
    if not solution_found then
        logOut("")
        logOut("No solution found")
    end if
    return solution_found
end solveSudoku

on preCover(puzzle)
    repeat with rowIdx from 0 to 8
        repeat with colIdx from 0 to 8
            set val to item (rowIdx * 9 + colIdx + 1) of puzzle
            if val is not 0 then
                set target to (rowIdx * 9 + colIdx) * 10 + val
                -- Find node with this h
                repeat with n from 1 to nodes
                    if (item n of h) = target then
                        set idx to n - 1
                        -- Cover this row
                        cover(item (idx + 1) of c)
                        set j to item (idx + 1) of r
                        repeat while j is not idx
                            cover(item (j + 1) of c)
                            set j to item (j + 1) of r
                        end repeat
                        exit repeat
                    end if
                end repeat
            end if
        end repeat
    end repeat
end preCover

on printGrid(grid)
    repeat with row from 0 to 8
        set rowLine to ""
        repeat with col from 0 to 8
            set rowLine to rowLine & (item (row * 9 + col + 1) of grid) & " "
        end repeat
        logOut(rowLine)
    end repeat
end printGrid

on buildMatrix(puzzle)
    set numCols to 324
    set nodes to numCols + 1
    repeat with i from 0 to numCols
        set item (i + 1) of u to i
        set item (i + 1) of d to i
        set item (i + 1) of l to i - 1
        set item (i + 1) of r to i + 1
        set item (i + 1) of s to 0
    end repeat
    set item 1 of l to numCols
    set item (numCols + 1) of r to 0
    
    repeat with rowIdx from 0 to 8
        repeat with colIdx from 0 to 8
            set valIdx to rowIdx * 9 + colIdx + 1
            set val to item valIdx of puzzle
            if val = 0 then
                repeat with k from 1 to 9
                    addRow(rowIdx, colIdx, k)
                end repeat
            else
                addRow(rowIdx, colIdx, val)
            end if
        end repeat
    end repeat
end buildMatrix

on addRow(row, col, val)
    set c1 to (row * 9 + col) + 1
    set c2 to 81 + (row * 9 + val - 1) + 1
    set c3 to 162 + (col * 9 + val - 1) + 1
    set box to (row div 3) * 3 + (col div 3)
    set c4 to 243 + (box * 9 + val - 1) + 1
    
    set n1 to addNode(c1)
    set n2 to addNode(c2)
    set n3 to addNode(c3)
    set n4 to addNode(c4)
    
    set item (n1 + 1) of r to n2
    set item (n1 + 1) of l to n4
    set item (n2 + 1) of r to n3
    set item (n2 + 1) of l to n1
    set item (n3 + 1) of r to n4
    set item (n3 + 1) of l to n2
    set item (n4 + 1) of r to n1
    set item (n4 + 1) of l to n3
    
    set item (n1 + 1) of h to (row * 9 + col) * 10 + val
end addRow

on addNode(colIdx)
    set myIdx to nodes
    set nodes to nodes + 1
    set upIdx to item (colIdx + 1) of u
    set item (myIdx + 1) of u to upIdx
    set item (myIdx + 1) of d to colIdx
    set item (myIdx + 1) of c to colIdx
    set item (upIdx + 1) of d to myIdx
    set item (colIdx + 1) of u to myIdx
    set item (colIdx + 1) of s to (item (colIdx + 1) of s) + 1
    return myIdx
end addNode

on cover(colIdx)
    set r_node to item (colIdx + 1) of r
    set l_node to item (colIdx + 1) of l
    set item (r_node + 1) of l to l_node
    set item (l_node + 1) of r to r_node
    set i to item (colIdx + 1) of d
    repeat while i is not colIdx
        set j to item (i + 1) of r
        repeat while j is not i
            set d_node to item (j + 1) of d
            set u_node to item (j + 1) of u
            set item (d_node + 1) of u to u_node
            set item (u_node + 1) of d to d_node
            set c_node to item (j + 1) of c
            set item (c_node + 1) of s to (item (c_node + 1) of s) - 1
            set j to item (j + 1) of r
        end repeat
        set i to item (i + 1) of d
    end repeat
end cover

on uncover(colIdx)
    set i to item (colIdx + 1) of u
    repeat while i is not colIdx
        set j to item (i + 1) of l
        repeat while j is not i
            set c_node to item (j + 1) of c
            set item (c_node + 1) of s to (item (c_node + 1) of s) + 1
            set u_node to item (j + 1) of u
            set d_node to item (j + 1) of d
            set item (u_node + 1) of d to j
            set item (d_node + 1) of u to j
            set j to item (j + 1) of l
        end repeat
        set i to item (i + 1) of u
    end repeat
    set r_node to item (colIdx + 1) of r
    set l_node to item (colIdx + 1) of l
    set item (r_node + 1) of l to colIdx
    set item (l_node + 1) of r to colIdx
end uncover

on search(k)
    set updates to updates + 1
    if (item 1 of r) = 0 then
        printSolution()
        set solution_found to true
        return
    end if
    set minS to 999999
    set cIdx to -1
    set j to item 1 of r
    repeat while j is not 0
        set colSize to item (j + 1) of s
        if colSize < minS then
            set minS to colSize
            set cIdx to j
        end if
        set j to item (j + 1) of r
    end repeat
    
    if cIdx is -1 or minS is 0 then
        return
    end if

    cover(cIdx)
    set rIdx to item (cIdx + 1) of d
    repeat while rIdx is not cIdx
        set end of o to rIdx
        set j to item (rIdx + 1) of r
        repeat while j is not rIdx
            cover(item (j + 1) of c)
            set j to item (j + 1) of r
        end repeat
        search(k + 1)
        if solution_found then return
        set j to item (rIdx + 1) of l
        repeat while j is not rIdx
            uncover(item (j + 1) of c)
            set j to item (j + 1) of l
        end repeat
        if (count of o) > 0 then
            set o to items 1 thru -2 of o
        end if
        set rIdx to item (rIdx + 1) of d
    end repeat
    uncover(cIdx)
end search

on printSolution()
    set resultGrid to {}
    repeat 81 times
        set end of resultGrid to 0
    end repeat
    repeat with nodeIdx in o
        set rowVal to -1
        set n to nodeIdx
        repeat 4 times
            if (item (n + 1) of h) is not 0 then
                set rowVal to item (n + 1) of h
                exit repeat
            end if
            set n to item (n + 1) of r
        end repeat
        if rowVal is not -1 then
            set val to rowVal mod 10
            set pos to rowVal div 10
            set item (pos + 1) of resultGrid to val
        end if
    end repeat
    logOut("")
    logOut("Puzzle:")
    printGrid(resultGrid)
    logOut("")
    logOut("Solved in Iterations=" & updates)
end printSolution
