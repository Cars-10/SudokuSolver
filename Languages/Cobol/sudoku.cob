      *> Sudoku Solver in COBOL using iterative backtracking
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUDOKU.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE ASSIGN TO WS-FILENAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD INFILE.
       01 IN-REC PIC X(200).

       WORKING-STORAGE SECTION.
       01 WS-GRID.
          05 WS-ROW OCCURS 9.
             10 WS-CELL OCCURS 9 PIC 9.

       01 WS-EMPTY-CELLS.
          05 WS-EMPTY OCCURS 81.
             10 EC-ROW PIC 99.
             10 EC-COL PIC 99.

       01 WS-EC-COUNT     PIC 99 VALUE 0.
       01 WS-EC-IDX       PIC 99.
       01 WS-ITERATIONS   PIC 9(9) COMP VALUE 0.
       01 WS-FILENAME     PIC X(256).
       01 WS-FS           PIC XX.
       01 WS-EOF          PIC X VALUE 'N'.
       01 WS-LINE         PIC X(200).
       01 WS-RIDX         PIC 99.
       01 WS-CIDX         PIC 99.
       01 WS-IDX          PIC 999.
       01 WS-CH           PIC X.
       01 WS-DISP         PIC X(20).

       01 WS-R            PIC 99.
       01 WS-C            PIC 99.
       01 WS-V            PIC 99.
       01 WS-I            PIC 99.
       01 WS-BOXR         PIC 99.
       01 WS-BOXC         PIC 99.
       01 WS-VALID        PIC X.
       01 WS-SOLVED       PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       0000-MAIN.
           ACCEPT WS-FILENAME FROM COMMAND-LINE
           IF WS-FILENAME = SPACES
               DISPLAY "Usage: Sudoku <matrix>"
               STOP RUN
           END-IF

           PERFORM 1000-READ-MATRIX
           DISPLAY "Puzzle:"
           PERFORM 2000-PRINT-GRID

           PERFORM 3000-FIND-EMPTY-CELLS
           MOVE 0 TO WS-ITERATIONS
           PERFORM 4000-SOLVE

           IF WS-SOLVED = 'Y'
               DISPLAY " "
               DISPLAY "Puzzle:"
               PERFORM 2000-PRINT-GRID
               DISPLAY " "
               DISPLAY "Solved in Iterations=" WS-ITERATIONS
           ELSE
               DISPLAY "No solution found."
           END-IF
           DISPLAY " "
           STOP RUN.

       1000-READ-MATRIX.
           OPEN INPUT INFILE
           IF WS-FS NOT = "00"
               DISPLAY "Error: " WS-FILENAME
               STOP RUN
           END-IF
           MOVE 1 TO WS-RIDX
           PERFORM UNTIL WS-EOF = 'Y' OR WS-RIDX > 9
               READ INFILE INTO WS-LINE
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF WS-LINE(1:1) NOT = '#'
                          AND WS-LINE NOT = SPACES
                           MOVE 1 TO WS-CIDX
                           PERFORM VARYING WS-IDX FROM 1 BY 1
                               UNTIL WS-IDX > 200 OR WS-CIDX > 9
                               MOVE WS-LINE(WS-IDX:1) TO WS-CH
                               IF WS-CH >= '0' AND WS-CH <= '9'
                                   MOVE WS-CH TO WS-CELL(WS-RIDX WS-CIDX)
                                   ADD 1 TO WS-CIDX
                               END-IF
                           END-PERFORM
                           IF WS-CIDX > 1
                               ADD 1 TO WS-RIDX
                           END-IF
                       END-IF
               END-READ
           END-PERFORM
           CLOSE INFILE.

       2000-PRINT-GRID.
           PERFORM VARYING WS-RIDX FROM 1 BY 1 UNTIL WS-RIDX > 9
               INITIALIZE WS-DISP
               MOVE 1 TO WS-IDX
               PERFORM VARYING WS-CIDX FROM 1 BY 1 UNTIL WS-CIDX > 9
                   MOVE WS-CELL(WS-RIDX WS-CIDX) TO WS-DISP(WS-IDX:1)
                   ADD 1 TO WS-IDX
                   IF WS-CIDX < 9
                       MOVE SPACE TO WS-DISP(WS-IDX:1)
                       ADD 1 TO WS-IDX
                   END-IF
               END-PERFORM
               DISPLAY WS-DISP
           END-PERFORM.

       3000-FIND-EMPTY-CELLS.
      *> Build list of all empty cells for iteration
           MOVE 0 TO WS-EC-COUNT
           PERFORM VARYING WS-RIDX FROM 1 BY 1 UNTIL WS-RIDX > 9
               PERFORM VARYING WS-CIDX FROM 1 BY 1 UNTIL WS-CIDX > 9
                   IF WS-CELL(WS-RIDX WS-CIDX) = 0
                       ADD 1 TO WS-EC-COUNT
                       MOVE WS-RIDX TO EC-ROW(WS-EC-COUNT)
                       MOVE WS-CIDX TO EC-COL(WS-EC-COUNT)
                   END-IF
               END-PERFORM
           END-PERFORM.

       4000-SOLVE.
           IF WS-EC-COUNT = 0
               MOVE 'Y' TO WS-SOLVED
               EXIT PARAGRAPH
           END-IF

           MOVE 1 TO WS-EC-IDX
           MOVE EC-ROW(1) TO WS-R
           MOVE EC-COL(1) TO WS-C
           MOVE 1 TO WS-CELL(WS-R WS-C)

           PERFORM UNTIL WS-SOLVED = 'Y' OR WS-EC-IDX = 0
               MOVE EC-ROW(WS-EC-IDX) TO WS-R
               MOVE EC-COL(WS-EC-IDX) TO WS-C
               MOVE WS-CELL(WS-R WS-C) TO WS-V

               IF WS-V > 9
                   MOVE 0 TO WS-CELL(WS-R WS-C)
                   SUBTRACT 1 FROM WS-EC-IDX
               ELSE
                   ADD 1 TO WS-ITERATIONS
                   PERFORM 5000-IS-VALID
                   IF WS-VALID = 'Y'
                       IF WS-EC-IDX = WS-EC-COUNT
                           MOVE 'Y' TO WS-SOLVED
                       ELSE
                           ADD 1 TO WS-EC-IDX
                           MOVE EC-ROW(WS-EC-IDX) TO WS-R
                           MOVE EC-COL(WS-EC-IDX) TO WS-C
                           MOVE 1 TO WS-CELL(WS-R WS-C)
                       END-IF
                   ELSE
                       ADD 1 TO WS-CELL(WS-R WS-C)
                   END-IF
               END-IF
           END-PERFORM.

       5000-IS-VALID.
           MOVE 'Y' TO WS-VALID
      *> Check row
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > 9 OR WS-VALID = 'N'
               IF WS-I NOT = WS-C
                   IF WS-CELL(WS-R WS-I) = WS-V
                       MOVE 'N' TO WS-VALID
                   END-IF
               END-IF
           END-PERFORM
      *> Check column
           IF WS-VALID = 'Y'
               PERFORM VARYING WS-I FROM 1 BY 1
                       UNTIL WS-I > 9 OR WS-VALID = 'N'
                   IF WS-I NOT = WS-R
                       IF WS-CELL(WS-I WS-C) = WS-V
                           MOVE 'N' TO WS-VALID
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *> Check 3x3 box
           IF WS-VALID = 'Y'
               COMPUTE WS-BOXR = ((WS-R - 1) / 3) * 3 + 1
               COMPUTE WS-BOXC = ((WS-C - 1) / 3) * 3 + 1
               PERFORM 5100-CHECK-BOX
           END-IF.

       5100-CHECK-BOX.
           PERFORM VARYING WS-RIDX FROM WS-BOXR BY 1
                   UNTIL WS-RIDX > WS-BOXR + 2 OR WS-VALID = 'N'
               PERFORM VARYING WS-CIDX FROM WS-BOXC BY 1
                       UNTIL WS-CIDX > WS-BOXC + 2 OR WS-VALID = 'N'
                   IF WS-RIDX NOT = WS-R OR WS-CIDX NOT = WS-C
                       IF WS-CELL(WS-RIDX WS-CIDX) = WS-V
                           MOVE 'N' TO WS-VALID
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM.

       END PROGRAM SUDOKU.
