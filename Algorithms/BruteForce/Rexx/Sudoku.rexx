#!/usr/bin/env regina
/* Sudoku Solver in Regina Rexx
 * Matches C reference algorithm exactly
 * - Brute-force backtracking
 * - Row-major order search (top-to-bottom, left-to-right)
 * - Values tried 1-9 in ascending order
 * - COUNT BEFORE validity check
 */

/* Initialize global counter */
count = 0

/* Parse command line argument */
parse arg filename
filename = strip(filename)

if filename = '' then do
    say 'Usage: regina Sudoku.rexx <input_file>'
    exit 1
end

/* Initialize puzzle array (0-indexed to match C) */
do r = 0 to 8
    do c = 0 to 8
        puzzle.r.c = 0
    end
end

/* Read matrix file */
if \readMatrix(filename) then exit 1

/* Print initial puzzle state */
call printPuzzle

/* Solve the puzzle */
call solve

exit 0

/* ================================================================
 * Read matrix from file
 * ================================================================ */
readMatrix: procedure expose puzzle.
    parse arg filename

    /* Display path (normalize /app/Matrices to ../Matrices/) */
    if pos('/app/Matrices/', filename) = 1 then do
        displayPath = substr(filename, 6)  /* Skip "/app/" */
        say '../'displayPath
    end
    else do
        say filename
    end

    /* Open and read file */
    row = 0
    do while lines(filename) > 0
        line = linein(filename)
        line = strip(line)

        /* Skip empty lines and comments */
        if line = '' then iterate
        if left(line, 1) = '#' then iterate

        /* Parse 9 space-separated integers */
        col = 0
        do while words(line) > 0 & col < 9
            parse var line val line
            puzzle.row.col = val + 0  /* Convert to number */
            col = col + 1
        end

        /* Print the row as we read it (like C does) */
        outline = ''
        do c = 0 to 8
            if c > 0 then outline = outline || ' '
            outline = outline || puzzle.row.c
        end
        say outline

        row = row + 1
        if row >= 9 then leave
    end

    call stream filename, 'C', 'CLOSE'
    return 1

/* ================================================================
 * Print puzzle grid
 * ================================================================ */
printPuzzle: procedure expose puzzle.
    say ''
    say 'Puzzle:'
    do r = 0 to 8
        line = ''
        do c = 0 to 8
            if c > 0 then line = line || ' '
            line = line || puzzle.r.c
        end
        say line
    end
    return

/* ================================================================
 * Check if placing val at (row, col) is valid
 * ================================================================ */
isValid: procedure expose puzzle.
    parse arg row, col, val

    /* Check row */
    do i = 0 to 8
        if puzzle.row.i = val then return 0
    end

    /* Check column */
    do i = 0 to 8
        if puzzle.i.col = val then return 0
    end

    /* Check 3x3 box */
    boxRow = (row % 3) * 3
    boxCol = (col % 3) * 3
    do i = 0 to 2
        do j = 0 to 2
            r = boxRow + i
            c = boxCol + j
            if puzzle.r.c = val then return 0
        end
    end

    return 1

/* ================================================================
 * Recursive brute-force solver
 * ================================================================ */
solve: procedure expose puzzle. count
    /* Find first empty cell (row-major order) */
    foundRow = -1
    foundCol = -1
    do r = 0 to 8
        do c = 0 to 8
            if puzzle.r.c = 0 then do
                foundRow = r
                foundCol = c
                leave r  /* Break out of both loops */
            end
        end
    end

    /* If no empty cell found, puzzle is solved */
    if foundRow = -1 then do
        call printPuzzle
        say ''
        say 'Solved in Iterations='count
        say ''
        return 1
    end

    /* Try values 1-9 in order */
    do val = 1 to 9
        count = count + 1  /* COUNT BEFORE validity check - algorithm fingerprint */

        if isValid(foundRow, foundCol, val) then do
            puzzle.foundRow.foundCol = val  /* Place value */

            if solve() then return 1  /* Solved */

            puzzle.foundRow.foundCol = 0  /* Backtrack */
        end
    end

    return 0  /* No solution found */
