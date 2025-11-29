/* Rexx Sudoku Solver */
parse arg args
start = time('S') + time('U') / 1000000

call process_files args

end = time('S') + time('U') / 1000000
say "Seconds to process " format(end - start, , 3)
exit

process_files:
    parse arg filelist
    /* Regina Rexx doesn't expand wildcards automatically in arg, so we rely on shell expansion passed in */
    do i = 1 to words(filelist)
        file = word(filelist, i)
        if right(file, 7) = ".matrix" then do
            call process_file file
        end
    end
    return

process_file:
    parse arg filename
    say filename
    
    /* Initialize puzzle */
    do r = 0 to 8
        do c = 0 to 8
            puzzle.r.c = 0
        end
    end
    
    /* Read file */
    row = 0
    call stream filename, "C", "OPEN READ"
    do while lines(filename) > 0
        line = linein(filename)
        if left(line, 1) = "#" | length(line) < 2 then iterate
        
        /* Parse line manually to handle whitespace */
        col = 0
        do j = 1 to words(line)
            val = word(line, j)
            puzzle.row.col = val
            col = col + 1
        end
        if col = 9 then row = row + 1
        if row = 9 then leave
    end
    call stream filename, "C", "CLOSE"
    
    call print_puzzle
    count = 0
    if solve() then do
        call print_puzzle
        say "Solved in Iterations=" || count
        say ""
    end
    else say "No solution found"
    return

print_puzzle:
    say ""
    say "Puzzle:"
    do r = 0 to 8
        line = ""
        do c = 0 to 8
            line = line || puzzle.r.c || " "
        end
        say strip(line)
    end
    return

is_possible:
    parse arg r, c, val
    
    do i = 0 to 8
        if puzzle.i.c = val then return 0
        if puzzle.r.i = val then return 0
    end
    
    r0 = (r % 3) * 3
    c0 = (c % 3) * 3
    
    do i = 0 to 2
        do j = 0 to 2
            rr = r0 + i
            cc = c0 + j
            if puzzle.rr.cc = val then return 0
        end
    end
    return 1

solve:
    do r = 0 to 8
        do c = 0 to 8
            if puzzle.r.c = 0 then do
                do val = 1 to 9
                    count = count + 1
                    if is_possible(r, c, val) then do
                        puzzle.r.c = val
                        if solve() then return 1
                        puzzle.r.c = 0
                    end
                end
                return 0
            end
        end
    end
    return 1
