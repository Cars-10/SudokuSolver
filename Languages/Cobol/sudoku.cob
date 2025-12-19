>>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUDOKU.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE ASSIGN TO WS-FILENAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD INFILE.
       01 IN-REC PIC X(100).

       WORKING-STORAGE SECTION.
       01 WS-FILENAME     PIC X(256).
       01 FS-STATUS       PIC XX.
       01 WS-EOF          PIC X VALUE 'N'.
       01 WS-PTR          PIC 9(4).
       01 WS-CHAR         PIC X.
       
       01 GRID.
          05 G-ROW OCCURS 9.
             10 G-COL OCCURS 9 PIC 9.
             
       01 ITERATIONS PIC 9(12) COMP VALUE 0.
       01 SOLVED-FLAG PIC X VALUE 'N'.

       01 DISP-LINE PIC X(20).
       01 DISP-R PIC 9.
       01 DISP-C PIC 9.
       
       01 TEMP-R PIC 9.
       01 TEMP-C PIC 9.

       *> Stack for iterative backtracking
       01 STACK-DATA.
          05 STACK-PTR PIC 99 VALUE 0.
          05 STACK-ENTRY OCCURS 81.
             10 S-R PIC 9.
             10 S-C PIC 9.
             10 S-VAL PIC 9.

       01 CUR-R PIC 9.
       01 CUR-C PIC 9.
       01 CUR-VAL PIC 9.
       01 IS-VALID PIC X.
       01 I PIC 99.
       01 J PIC 99.
       01 BOX-R PIC 9.
       01 BOX-C PIC 9.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           ACCEPT WS-FILENAME FROM COMMAND-LINE
           IF WS-FILENAME = SPACES
               STOP RUN
           END-IF

           DISPLAY WS-FILENAME

           PERFORM READ-MATRIX
           DISPLAY " "
           DISPLAY "Puzzle:"
           PERFORM PRINT-BOARD

           MOVE 0 TO ITERATIONS
           MOVE 'N' TO SOLVED-FLAG
           
           PERFORM SOLVE-ITERATIVE
           
           IF SOLVED-FLAG = 'Y'
               DISPLAY " "
               DISPLAY "Puzzle:"
               PERFORM PRINT-BOARD
               DISPLAY " "
               DISPLAY "Solved in Iterations=" ITERATIONS
           ELSE
               DISPLAY "No solution found."
           END-IF
           
           STOP RUN.

       READ-MATRIX.
           OPEN INPUT INFILE
           IF FS-STATUS NOT = "00"
               STOP RUN
           END-IF
           MOVE 0 TO TEMP-R
           PERFORM UNTIL WS-EOF = 'Y' OR TEMP-R = 9
               READ INFILE INTO IN-REC
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF IN-REC NOT = SPACES AND IN-REC(1:1) NOT = '#'
                           ADD 1 TO TEMP-R
                           MOVE 1 TO TEMP-C
                           PERFORM VARYING WS-PTR FROM 1 BY 1
                               UNTIL WS-PTR > 100 OR TEMP-C > 9
                               MOVE IN-REC(WS-PTR:1) TO WS-CHAR
                               IF WS-CHAR >= '0' AND WS-CHAR <= '9'
                                   MOVE WS-CHAR TO G-COL(TEMP-R, TEMP-C)
                                   ADD 1 TO TEMP-C
                               END-IF
                           END-PERFORM
                       END-IF
               END-READ
           END-PERFORM
           CLOSE INFILE.

       PRINT-BOARD.
           PERFORM VARYING DISP-R FROM 1 BY 1 UNTIL DISP-R > 9
               MOVE SPACES TO DISP-LINE
               PERFORM VARYING DISP-C FROM 1 BY 1 UNTIL DISP-C > 9
                   MOVE G-COL(DISP-R, DISP-C) TO DISP-LINE(DISP-C * 2 - 1 : 1)
               END-PERFORM
               DISPLAY FUNCTION TRIM(DISP-LINE)
           END-PERFORM.

       SOLVE-ITERATIVE.
           MOVE 0 TO STACK-PTR
           PERFORM FIND-NEXT-EMPTY
           IF CUR-R = 0
               MOVE 'Y' TO SOLVED-FLAG
               EXIT PARAGRAPH
           END-IF
           
           ADD 1 TO STACK-PTR
           MOVE CUR-R TO S-R(STACK-PTR)
           MOVE CUR-C TO S-C(STACK-PTR)
           MOVE 0 TO S-VAL(STACK-PTR)

           PERFORM UNTIL STACK-PTR = 0 OR SOLVED-FLAG = 'Y'
               MOVE S-R(STACK-PTR) TO CUR-R
               MOVE S-C(STACK-PTR) TO CUR-C
               ADD 1 TO S-VAL(STACK-PTR)
               MOVE S-VAL(STACK-PTR) TO CUR-VAL
               
               IF CUR-VAL > 9
                   MOVE 0 TO G-COL(CUR-R, CUR-C)
                   SUBTRACT 1 FROM STACK-PTR
               ELSE
                   ADD 1 TO ITERATIONS
                   PERFORM CHECK-VALIDITY
                   IF IS-VALID = 'Y'
                       MOVE CUR-VAL TO G-COL(CUR-R, CUR-C)
                       PERFORM FIND-NEXT-EMPTY
                       IF CUR-R = 0
                           MOVE 'Y' TO SOLVED-FLAG
                       ELSE
                           ADD 1 TO STACK-PTR
                           MOVE CUR-R TO S-R(STACK-PTR)
                           MOVE CUR-C TO S-C(STACK-PTR)
                           MOVE 0 TO S-VAL(STACK-PTR)
                       END-IF
                   END-IF
               END-IF
           END-PERFORM.

       FIND-NEXT-EMPTY.
           MOVE 0 TO CUR-R
           MOVE 0 TO CUR-C
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 9 OR CUR-R > 0
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 9 OR CUR-R > 0
                   IF G-COL(I, J) = 0
                       MOVE I TO CUR-R
                       MOVE J TO CUR-C
                   END-IF
               END-PERFORM
           END-PERFORM.

       CHECK-VALIDITY.
           MOVE 'Y' TO IS-VALID
           *> Check Row
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 9 OR IS-VALID = 'N'
               IF G-COL(CUR-R, J) = CUR-VAL
                   MOVE 'N' TO IS-VALID
               END-IF
           END-PERFORM
           
           *> Check Col
           IF IS-VALID = 'Y'
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > 9 OR IS-VALID = 'N'
                   IF G-COL(I, CUR-C) = CUR-VAL
                       MOVE 'N' TO IS-VALID
                   END-IF
               END-PERFORM
           END-IF
           
           *> Check Box
           IF IS-VALID = 'Y'
               COMPUTE BOX-R = FUNCTION INTEGER((CUR-R - 1) / 3) * 3 + 1
               COMPUTE BOX-C = FUNCTION INTEGER((CUR-C - 1) / 3) * 3 + 1
               PERFORM VARYING I FROM 0 BY 1 UNTIL I > 2 OR IS-VALID = 'N'
                   PERFORM VARYING J FROM 0 BY 1 UNTIL J > 2 OR IS-VALID = 'N'
                       IF G-COL(BOX-R + I, BOX-C + J) = CUR-VAL
                           MOVE 'N' TO IS-VALID
                       END-IF
                   END-PERFORM
               END-PERFORM
           END-IF.