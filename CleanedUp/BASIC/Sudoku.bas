REM https://qb64.com/wiki


DIM board(9, 9)
DIM filename as STRING


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
filename = "../Matrices/1.matrix"
CALL ReadSudokuFromFile filename$
PRINT "Input Sudoku Puzzle:"
CALL PrintBoard
IF CALL SolveSudoku() THEN
    PRINT "Sudoku Solution:"
    CALL PrintBoard
ELSE
    PRINT "No solution found."
END IF


FUNCTION PrintBoard()
    FOR row = 1 TO 9
        FOR col = 1 TO 9
            PRINT board(row, col);
        NEXT
        PRINT
    NEXT
END FUNCTION

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
                    IF IsValidMove(row, col, num) THEN
                        board(row, col) = num
                        IF SolveSudoku = 1  THEN
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

FUNCTION ReadSudokuFromFile(filename$)
    OPEN filename$ FOR INPUT AS #1
    FOR row = 1 TO 9
        LINE INPUT #1, linetext$
        FOR col = 1 TO 9
            board(row, col) = VAL(MID$(linetext$, col, 1))
        NEXT
    NEXT
    CLOSE #1
END FUNCTION

