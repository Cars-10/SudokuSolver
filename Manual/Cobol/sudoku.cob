       IDENTIFICATION DIVISION.
       PROGRAM-ID. Sudoku.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT InputFile ASSIGN TO "../../Matrices/1.matrix"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD InputFile.
       01 InputLine PIC X(80).

       WORKING-STORAGE SECTION.
       01  WS-Board.
           05  WS-Board-Row OCCURS 9 TIMES.
               10  WS-Board-Cell OCCURS 9 TIMES PIC 9.

       01  WS-Iterations PIC 9(9) VALUE 0.
       01  WS-EOF        PIC X VALUE 'N'.
       01  WS-Line       PIC X(80).
       01  WS-Idx        PIC 99.
       01  WS-NumIdx     PIC 99.
       01  WS-RowIdx     PIC 99.
       01  WS-ColIdx     PIC 99.
       01  WS-Char       PIC X.
       01  WS-Filename   PIC X(100).
       
       01  LS-Solved     PIC X.

       PROCEDURE DIVISION.
       Main-Logic.
           DISPLAY "Starting Sudoku..."
           PERFORM Read-Matrix
           DISPLAY "Matrix Read."
           DISPLAY "Puzzle:"
           PERFORM Print-Board
           MOVE 0 TO WS-Iterations
           CALL "Solve" USING WS-Board WS-Iterations LS-Solved
           IF LS-Solved = 'Y'
               DISPLAY "Puzzle:"
               PERFORM Print-Board
               DISPLAY "Solved in Iterations=" WS-Iterations
           ELSE
               DISPLAY "No solution found."
           END-IF
           STOP RUN.

       Read-Matrix.
           ACCEPT WS-Filename FROM COMMAND-LINE
           DISPLAY "Filename: " WS-Filename
           OPEN INPUT InputFile
           MOVE 1 TO WS-RowIdx
           PERFORM UNTIL WS-EOF = 'Y' OR WS-RowIdx > 9
               READ InputFile INTO WS-Line
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF WS-Line(1:1) NOT = '#'
                           MOVE 1 TO WS-ColIdx
                           MOVE 1 TO WS-Idx
                           PERFORM UNTIL WS-ColIdx > 9 OR WS-Idx > 80
                               MOVE WS-Line(WS-Idx:1) TO WS-Char
                               IF WS-Char >= '0' AND WS-Char <= '9'
                                   MOVE WS-Char TO WS-Board-Cell(WS-RowIdx, WS-ColIdx)
                                   ADD 1 TO WS-ColIdx
                               END-IF
                               ADD 1 TO WS-Idx
                           END-PERFORM
                           ADD 1 TO WS-RowIdx
                       END-IF
               END-READ
           END-PERFORM
           CLOSE InputFile.

       Print-Board.
           PERFORM VARYING WS-RowIdx FROM 1 BY 1 UNTIL WS-RowIdx > 9
               DISPLAY WS-Board-Cell(WS-RowIdx, 1) " " WS-Board-Cell(WS-RowIdx, 2) " " WITH NO ADVANCING
               DISPLAY WS-Board-Cell(WS-RowIdx, 3) " " WS-Board-Cell(WS-RowIdx, 4) " " WITH NO ADVANCING
               DISPLAY WS-Board-Cell(WS-RowIdx, 5) " " WS-Board-Cell(WS-RowIdx, 6) " " WITH NO ADVANCING
               DISPLAY WS-Board-Cell(WS-RowIdx, 7) " " WS-Board-Cell(WS-RowIdx, 8) " " WITH NO ADVANCING
               DISPLAY WS-Board-Cell(WS-RowIdx, 9)
           END-PERFORM.

       END PROGRAM Sudoku.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. Solve RECURSIVE.
       
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  WS-Row        PIC 99.
       01  WS-Col        PIC 99.
       01  WS-Num        PIC 9.
       01  WS-Found      PIC X.
       01  WS-Valid      PIC X.

       LINKAGE SECTION.
       01  WS-Board.
           05  WS-Board-Row OCCURS 9 TIMES.
               10  WS-Board-Cell OCCURS 9 TIMES PIC 9.
       01  WS-Iterations PIC 9(9).
       01  LS-Result     PIC X.

       PROCEDURE DIVISION USING WS-Board WS-Iterations LS-Result.
           MOVE 'N' TO WS-Found
           PERFORM VARYING WS-Row FROM 1 BY 1 UNTIL WS-Row > 9 OR WS-Found = 'Y'
               PERFORM VARYING WS-Col FROM 1 BY 1 UNTIL WS-Col > 9 OR WS-Found = 'Y'
                   IF WS-Board-Cell(WS-Row, WS-Col) = 0
                       MOVE 'Y' TO WS-Found
                       SUBTRACT 1 FROM WS-Row
                       SUBTRACT 1 FROM WS-Col
                   END-IF
               END-PERFORM
           END-PERFORM

           IF WS-Found = 'N'
               MOVE 'Y' TO LS-Result
               EXIT PROGRAM
           END-IF

           PERFORM VARYING WS-Num FROM 1 BY 1 UNTIL WS-Num > 9
               ADD 1 TO WS-Iterations
               CALL "IsValid" USING BY CONTENT WS-Row BY CONTENT WS-Col BY CONTENT WS-Num WS-Board LS-Result
               IF LS-Result = 'Y'
                   MOVE WS-Num TO WS-Board-Cell(WS-Row, WS-Col)
                   CALL "Solve" USING WS-Board WS-Iterations LS-Result
                   IF LS-Result = 'Y'
                       EXIT PROGRAM
                   END-IF
                   MOVE 0 TO WS-Board-Cell(WS-Row, WS-Col)
               END-IF
           END-PERFORM

           MOVE 'N' TO LS-Result.
       
       END PROGRAM Solve.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. IsValid.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-I          PIC 99.
       01  WS-BoxRow     PIC 99.
       01  WS-BoxCol     PIC 99.
       01  WS-R          PIC 99.
       01  WS-C          PIC 99.

       LINKAGE SECTION.
       01  L-Row         PIC 99.
       01  L-Col         PIC 99.
       01  L-Num         PIC 9.
       01  WS-Board.
           05  WS-Board-Row OCCURS 9 TIMES.
               10  WS-Board-Cell OCCURS 9 TIMES PIC 9.
       01  L-Result      PIC X.

       PROCEDURE DIVISION USING L-Row L-Col L-Num WS-Board L-Result.
           MOVE 'Y' TO L-Result
           
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 9
               IF WS-Board-Cell(L-Row, WS-I) = L-Num
                   MOVE 'N' TO L-Result
                   EXIT PROGRAM
               END-IF
           END-PERFORM

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 9
               IF WS-Board-Cell(WS-I, L-Col) = L-Num
                   MOVE 'N' TO L-Result
                   EXIT PROGRAM
               END-IF
           END-PERFORM

           COMPUTE WS-BoxRow = ((L-Row - 1) / 3) * 3 + 1
           COMPUTE WS-BoxCol = ((L-Col - 1) / 3) * 3 + 1
           
           PERFORM VARYING WS-R FROM 0 BY 1 UNTIL WS-R > 2
               PERFORM VARYING WS-C FROM 0 BY 1 UNTIL WS-C > 2
                   IF WS-Board-Cell(WS-BoxRow + WS-R, WS-BoxCol + WS-C) = L-Num
                       MOVE 'N' TO L-Result
                       EXIT PROGRAM
                   END-IF
               END-PERFORM
           END-PERFORM.
       
       END PROGRAM IsValid.
