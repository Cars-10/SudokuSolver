$CONSOLE:ONLY
REM https://qb64.com/wiki

CONST FALSE = 0
CONST TRUE = -1

DIM SHARED board(9, 9)
DIM SHARED iterations AS LONG
DIM filename as STRING

iterations = 0

count = _COMMANDCOUNT
FOR c = 1 TO count
    PRINT COMMAND$(c) 'or process commands sent
NEXT

IF COMMAND$ <> "" THEN
    filename$ = COMMAND$(1)
ELSE
    PRINT "Usage: sudoku_solver.exe input_file.txt"
    END
END IF

PRINT filename$
ReadSudokuFromFile filename$
PRINT "Puzzle:"
PrintBoard
IF SolveSudoku THEN
    PRINT "Puzzle:"
    PrintBoard
    PRINT "Solved in Iterations="; iterations
ELSE
    PRINT "No solution found."
END IF
SYSTEM


SUB PrintBoard()
    FOR row = 1 TO 9
        FOR col = 1 TO 9
            PRINT board(row, col);
        NEXT
        PRINT
    NEXT
END SUB

FUNCTION IsValidMove(row, col, num)
    FOR i = 1 TO 9
        IF board(row, i) = num OR board(i, col) = num THEN
            IsValidMove = FALSE ' Return FALSE if there's a conflict
            EXIT FUNCTION
        END IF
    NEXT
    subRow = INT((row - 1) / 3) * 3 + 1
    subCol = INT((col - 1) / 3) * 3 + 1
    FOR i = subRow TO subRow + 2
        FOR j = subCol TO subCol + 2
            IF board(i, j) = num THEN
                IsValidMove = FALSE ' Return FALSE if there's a conflict
                EXIT FUNCTION
            END IF
        NEXT
    NEXT
    IsValidMove = TRUE ' Return TRUE if the move is valid
END FUNCTION

FUNCTION SolveSudoku()
    FOR row = 1 TO 9
        FOR col = 1 TO 9
            IF board(row, col) = 0 THEN
                FOR num = 1 TO 9
                    iterations = iterations + 1
                    IF IsValidMove(row, col, num) THEN
                        board(row, col) = num
                        IF SolveSudoku THEN
                            SolveSudoku = 1
                            EXIT FUNCTION
                        END IF
                        board(row, col) = 0
                    END IF
                NEXT
                SolveSudoku = 0
                EXIT FUNCTION
            END IF
        NEXT
    NEXT
    SolveSudoku = 1
END FUNCTION

SUB ReadSudokuFromFile(filename$)
    OPEN filename$ FOR INPUT AS #1
    row = 1
    col = 1
    DO UNTIL EOF(1) OR row > 9
        LINE INPUT #1, linetext$
        linetext$ = _TRIM$(linetext$)
        IF LEFT$(linetext$, 1) <> "#" AND linetext$ <> "" THEN
            FOR i = 1 TO LEN(linetext$)
                char$ = MID$(linetext$, i, 1)
                IF char$ >= "0" AND char$ <= "9" THEN
                    board(row, col) = VAL(char$)
                    col = col + 1
                    IF col > 9 THEN
                        col = 1
                        row = row + 1
                        IF row > 9 THEN EXIT FOR
                    END IF
                END IF
            NEXT
        END IF
    LOOP
    CLOSE #1
END SUB
