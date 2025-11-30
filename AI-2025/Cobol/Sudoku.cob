       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUDOKU.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO DYNAMIC WS-FILENAME
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD PIC X(80).

       WORKING-STORAGE SECTION.
       01  WS-FILENAME      PIC X(100).
       01  WS-EOF           PIC X VALUE 'N'.
       01  WS-PUZZLE.
           05 WS-ROW OCCURS 9 TIMES.
               10 WS-COL OCCURS 9 TIMES PIC 9.
       01  WS-COUNT         PIC 9(9) VALUE 0.
       01  WS-I             PIC 99.
       01  WS-J             PIC 99.
       01  WS-VAL           PIC 99.
       01  WS-TRY           PIC 99.
       01  WS-POSSIBLE      PIC X.
       01  WS-SOLVED        PIC X.
       01  WS-FOUND         PIC X.
       01  WS-EMPTY-LIST.
           05 WS-EMPTY-ITEM OCCURS 81 TIMES.
               10 WS-EMPTY-ROW PIC 9.
               10 WS-EMPTY-COL PIC 9.
       01  WS-EMPTY-COUNT   PIC 99.
       01  WS-PTR           PIC S99.
       01  WS-TEMP-STR      PIC X(80).
       01  WS-IDX           PIC 99.
       01  WS-CHAR          PIC X.
       01  WS-NUM-IDX       PIC 99.
       01  WS-ROW-IDX       PIC 99.
       01  WS-COL-IDX       PIC 99.
       01  WS-FILE-IDX      PIC 99.
       01  CMD-ARGS         PIC X(1000).
       01  ARG-PTR          PIC 9(4) VALUE 1.
       01  ARG-LEN          PIC 9(4).
       01  ARG-COUNT        PIC 9(4).
       01  CURRENT-ARG      PIC X(100).
       01  START-TIME-VAL.
           05 ST-YYYY       PIC 9(4).
           05 ST-MM         PIC 9(2).
           05 ST-DD         PIC 9(2).
           05 ST-HH         PIC 9(2).
           05 ST-MIN        PIC 9(2).
           05 ST-SS         PIC 9(2).
           05 ST-MS         PIC 9(2).
       01  END-TIME-VAL.
           05 ET-YYYY       PIC 9(4).
           05 ET-MM         PIC 9(2).
           05 ET-DD         PIC 9(2).
           05 ET-HH         PIC 9(2).
           05 ET-MIN        PIC 9(2).
           05 ET-SS         PIC 9(2).
           05 ET-MS         PIC 9(2).
       01  START-SEC        PIC 9(9)V99.
       01  END-SEC          PIC 9(9)V99.
       01  ELAPSED-SEC      PIC 9(9)V999.
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           ACCEPT CMD-ARGS FROM COMMAND-LINE.
           ACCEPT ARG-COUNT FROM ARGUMENT-NUMBER.
           
           MOVE FUNCTION CURRENT-DATE(1:16) TO START-TIME-VAL.
           COMPUTE START-SEC = (ST-HH * 3600) + (ST-MIN * 60) + ST-SS + (ST-MS / 100.0).

           PERFORM VARYING WS-FILE-IDX FROM 1 BY 1 UNTIL WS-FILE-IDX > ARG-COUNT
               DISPLAY WS-FILE-IDX UPON ARGUMENT-NUMBER
               ACCEPT CURRENT-ARG FROM ARGUMENT-VALUE
               MOVE FUNCTION TRIM(CURRENT-ARG) TO CURRENT-ARG
               COMPUTE ARG-LEN = FUNCTION STORED-CHAR-LENGTH(CURRENT-ARG)
               DISPLAY "Arg: " CURRENT-ARG(1:ARG-LEN)
               
               IF ARG-LEN > 7 AND CURRENT-ARG(ARG-LEN - 6:7) = ".matrix"
                   MOVE CURRENT-ARG TO WS-FILENAME
                   DISPLAY WS-FILENAME
                   PERFORM READ-MATRIX-FILE
                   PERFORM PRINT-PUZZLE
                   MOVE 0 TO WS-COUNT
                   MOVE 'N' TO WS-SOLVED
                   PERFORM SOLVE
               END-IF
           END-PERFORM.

           MOVE FUNCTION CURRENT-DATE(1:16) TO END-TIME-VAL.
           COMPUTE END-SEC = (ET-HH * 3600) + (ET-MIN * 60) + ET-SS + (ET-MS / 100.0).
           
           IF END-SEC < START-SEC
               ADD 86400 TO END-SEC
           END-IF.
           
           COMPUTE ELAPSED-SEC = END-SEC - START-SEC.
           DISPLAY "Seconds to process " ELAPSED-SEC.
           
           STOP RUN.

       READ-MATRIX-FILE.
           OPEN INPUT INPUT-FILE.
           MOVE 1 TO WS-ROW-IDX.
           MOVE 1 TO WS-COL-IDX.
           MOVE 'N' TO WS-EOF.
           
           PERFORM UNTIL WS-EOF = 'Y' OR WS-ROW-IDX > 9
               READ INPUT-FILE INTO WS-TEMP-STR
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF WS-TEMP-STR(1:1) NOT = '#' AND WS-TEMP-STR NOT = SPACES
                           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 80
                               MOVE WS-TEMP-STR(WS-IDX:1) TO WS-CHAR
                               IF WS-CHAR >= '0' AND WS-CHAR <= '9'
                                   MOVE WS-CHAR TO WS-COL(WS-ROW-IDX, WS-COL-IDX)
                                   ADD 1 TO WS-COL-IDX
                                   IF WS-COL-IDX > 9
                                       MOVE 1 TO WS-COL-IDX
                                       ADD 1 TO WS-ROW-IDX
                                       IF WS-ROW-IDX > 9 EXIT PERFORM END-IF
                                   END-IF
                               END-IF
                           END-PERFORM
                       END-IF
               END-READ
           END-PERFORM.
           CLOSE INPUT-FILE.

       PRINT-PUZZLE.
           DISPLAY " ".
           DISPLAY "Puzzle:".
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 9
               DISPLAY WS-COL(WS-I, 1) " " WS-COL(WS-I, 2) " " WS-COL(WS-I, 3) " "
                       WS-COL(WS-I, 4) " " WS-COL(WS-I, 5) " " WS-COL(WS-I, 6) " "
                       WS-COL(WS-I, 7) " " WS-COL(WS-I, 8) " " WS-COL(WS-I, 9)
           END-PERFORM.

       IS-POSSIBLE.
           MOVE 'Y' TO WS-POSSIBLE.
           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 9
               IF WS-COL(WS-I, WS-IDX) = WS-VAL OR WS-COL(WS-IDX, WS-J) = WS-VAL
                   MOVE 'N' TO WS-POSSIBLE
               END-IF
           END-PERFORM.
           
           IF WS-POSSIBLE = 'Y'
               COMPUTE WS-ROW-IDX = FUNCTION INTEGER-PART((WS-I - 1) / 3) * 3 + 1
               COMPUTE WS-COL-IDX = FUNCTION INTEGER-PART((WS-J - 1) / 3) * 3 + 1
               PERFORM VARYING WS-NUM-IDX FROM 0 BY 1 UNTIL WS-NUM-IDX > 2
                   PERFORM VARYING WS-IDX FROM 0 BY 1 UNTIL WS-IDX > 2
                       IF WS-COL(WS-ROW-IDX + WS-NUM-IDX, WS-COL-IDX + WS-IDX) = WS-VAL
                           MOVE 'N' TO WS-POSSIBLE
                       END-IF
                   END-PERFORM
               END-PERFORM
           END-IF.

       SOLVE.
           MOVE 0 TO WS-EMPTY-COUNT.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 9
               PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 9
                   IF WS-COL(WS-I, WS-J) = 0
                       ADD 1 TO WS-EMPTY-COUNT
                       MOVE WS-I TO WS-EMPTY-ROW(WS-EMPTY-COUNT)
                       MOVE WS-J TO WS-EMPTY-COL(WS-EMPTY-COUNT)
                   END-IF
               END-PERFORM
           END-PERFORM.

           MOVE 1 TO WS-PTR.
           PERFORM UNTIL WS-PTR > WS-EMPTY-COUNT OR WS-PTR = 0
               MOVE WS-EMPTY-ROW(WS-PTR) TO WS-I
               MOVE WS-EMPTY-COL(WS-PTR) TO WS-J
               MOVE WS-COL(WS-I, WS-J) TO WS-VAL
               ADD 1 TO WS-VAL
               
               MOVE 'N' TO WS-FOUND
               PERFORM VARYING WS-TRY FROM WS-VAL BY 1 UNTIL WS-TRY > 9
                   ADD 1 TO WS-COUNT
                   MOVE WS-TRY TO WS-VAL
                   PERFORM IS-POSSIBLE
                   IF WS-POSSIBLE = 'Y'
                       MOVE WS-TRY TO WS-COL(WS-I, WS-J)
                       MOVE 'Y' TO WS-FOUND
                       ADD 1 TO WS-PTR
                       EXIT PERFORM
                   END-IF
               END-PERFORM
               
               IF WS-FOUND = 'N'
                   MOVE 0 TO WS-COL(WS-I, WS-J)
                   SUBTRACT 1 FROM WS-PTR
               END-IF
           END-PERFORM.

           PERFORM PRINT-PUZZLE.
           DISPLAY " ".
           DISPLAY "Solved in Iterations=" WS-COUNT.
           DISPLAY " ".
