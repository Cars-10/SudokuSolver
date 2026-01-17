>>SOURCE FORMAT FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. CP-SUDOKU.

*> Constraint Propagation Sudoku Solver - COBOL Implementation
*> Uses bitsets for candidate tracking and MRV heuristic
*> Iterative approach with explicit stack for backtracking

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

*> Grid: values[9][9] - 0 = empty
01 GRID-VALUES.
   05 GV-ROW OCCURS 9.
      10 GV-COL OCCURS 9 PIC 99.

*> Candidates: bitsets[9][9] - bits 1-9 indicate possible values
01 GRID-CANDIDATES.
   05 GC-ROW OCCURS 9.
      10 GC-COL OCCURS 9 PIC 9(5) COMP.

*> Iteration counter
01 CP-ITERATIONS   PIC 9(12) COMP VALUE 0.
01 DISP-ITER       PIC Z(11)9.
01 SOLVED-FLAG     PIC X VALUE 'N'.

*> Display variables
01 DISP-LINE       PIC X(20).
01 DISP-R          PIC 99.
01 DISP-C          PIC 99.

*> Working variables
01 WS-I            PIC 99.
01 WS-J            PIC 99.
01 WS-K            PIC 99.
01 WS-DIGIT        PIC 99.
01 WS-ROW          PIC 99.
01 WS-COL          PIC 99.
01 RESULT-FLAG     PIC X.
01 CANDIDATE-SET   PIC 9(5) COMP.
01 CANDIDATE-COUNT PIC 99.
01 BOX-NUM         PIC 99.
01 BOX-R           PIC 99.
01 BOX-C           PIC 99.
01 TEMP-R          PIC 99.
01 TEMP-C          PIC 99.

*> Bit operations working variables
01 BIT-MASK        PIC 9(5) COMP.
01 BIT-TEST        PIC 9(5) COMP.
01 BIT-WORK        PIC 9(5) COMP.
01 BIT-QUOT        PIC 9(5) COMP.
01 BIT-REM         PIC 9(5) COMP.
01 POWER-TABLE.
   05 POWER-VAL PIC 9(5) COMP OCCURS 10.

*> MRV search variables
01 MRV-R           PIC 99 VALUE 0.
01 MRV-C           PIC 99 VALUE 0.
01 MRV-FOUND       PIC X VALUE 'N'.
01 MIN-CANDIDATES  PIC 99.
01 NUM-CANDIDATES  PIC 99.

*> Search stack (max 81 cells)
01 SEARCH-STACK.
   05 STACK-PTR PIC 99 VALUE 0.
   05 STACK-ENTRY OCCURS 81.
      10 SS-R PIC 99.
      10 SS-C PIC 99.
      10 SS-NEXT-DIGIT PIC 99.

*> State stack for backtracking (max 81 levels)
01 STATE-STACK.
   05 STATE-PTR PIC 99 VALUE 0.
   05 STATE-LEVEL OCCURS 81.
      10 SL-VALUES.
         15 SL-ROW OCCURS 9.
            20 SL-COL OCCURS 9 PIC 99.
      10 SL-CANDIDATES.
         15 SLC-ROW OCCURS 9.
            20 SLC-COL OCCURS 9 PIC 9(5) COMP.

*> Propagation tracking
01 CHANGED-FLAG    PIC X.
01 HS-COUNT        PIC 99.
01 HS-LAST-R       PIC 99.
01 HS-LAST-C       PIC 99.
01 HS-ASSIGNED     PIC X.

*> Eliminate helper variables
01 ELIM-ROW        PIC 99.
01 ELIM-COL        PIC 99.
01 ELIM-DIGIT      PIC 99.
01 ELIM-REMAINING  PIC 99.
01 ELIM-LAST-DIGIT PIC 9.
01 ELIM-I          PIC 99.
01 ELIM-J          PIC 99.
01 ASSIGN-I        PIC 99.
01 ASSIGN-J        PIC 99.
01 COUNT-K         PIC 99.
01 COUNT-DIGIT     PIC 99.
01 GET-K           PIC 99.
01 GET-SAVED-DIGIT PIC 99.

PROCEDURE DIVISION.
MAIN-LOGIC.
    *> Initialize power table (powers of 2)
    MOVE 1 TO POWER-VAL(1)
    MOVE 2 TO POWER-VAL(2)
    MOVE 4 TO POWER-VAL(3)
    MOVE 8 TO POWER-VAL(4)
    MOVE 16 TO POWER-VAL(5)
    MOVE 32 TO POWER-VAL(6)
    MOVE 64 TO POWER-VAL(7)
    MOVE 128 TO POWER-VAL(8)
    MOVE 256 TO POWER-VAL(9)
    MOVE 512 TO POWER-VAL(10)

    ACCEPT WS-FILENAME FROM COMMAND-LINE
    IF WS-FILENAME = SPACES
        STOP RUN
    END-IF

    DISPLAY FUNCTION TRIM(WS-FILENAME)

    PERFORM READ-MATRIX
    DISPLAY " "
    DISPLAY "Puzzle:"
    PERFORM PRINT-BOARD

    *> Initialize grid
    PERFORM INIT-GRID

    *> Solve with constraint propagation
    MOVE 0 TO CP-ITERATIONS
    MOVE 0 TO STACK-PTR
    MOVE 0 TO STATE-PTR
    MOVE 'N' TO SOLVED-FLAG
    PERFORM CP-SOLVE

    IF SOLVED-FLAG = 'Y'
        DISPLAY " "
        DISPLAY "Puzzle:"
        PERFORM PRINT-BOARD
        DISPLAY " "
        MOVE CP-ITERATIONS TO DISP-ITER
        DISPLAY "Solved in Iterations=" FUNCTION TRIM(DISP-ITER)
    ELSE
        DISPLAY "No solution found."
    END-IF

    STOP RUN.

READ-MATRIX.
    OPEN INPUT INFILE
    IF FS-STATUS NOT = "00"
        DISPLAY "File open failed: " FS-STATUS
        STOP RUN
    END-IF
    MOVE 'N' TO WS-EOF
    MOVE 0 TO TEMP-R
    PERFORM UNTIL WS-EOF = 'Y' OR TEMP-R >= 9
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
                            MOVE FUNCTION NUMVAL(WS-CHAR)
                                TO GV-COL(TEMP-R, TEMP-C)
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
            *> Get the single digit from the 2-digit value
            *> COBOL CHAR function is 1-based: CHAR(49) = '0'
            COMPUTE WS-DIGIT = FUNCTION MOD(GV-COL(DISP-R, DISP-C), 10)
            ADD 49 TO WS-DIGIT GIVING WS-K
            MOVE FUNCTION CHAR(WS-K) TO DISP-LINE(DISP-C * 2 - 1:1)
        END-PERFORM
        DISPLAY FUNCTION TRIM(DISP-LINE)
    END-PERFORM.

INIT-GRID.
    *> Initialize candidates for each cell
    PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 9
        PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 9
            IF GV-COL(WS-I, WS-J) = 0
                *> Empty cell: set all candidates 1-9 (bits 1-9 = 1022)
                MOVE 1022 TO GC-COL(WS-I, WS-J)
            ELSE
                *> Given clue: set single candidate bit
                MOVE GV-COL(WS-I, WS-J) TO WS-DIGIT
                MOVE POWER-VAL(WS-DIGIT + 1) TO GC-COL(WS-I, WS-J)
            END-IF
        END-PERFORM
    END-PERFORM

    *> Propagate initial clues to eliminate candidates from peers
    DISPLAY "Starting initial clue propagation..."
    PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 9
        PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 9
            IF GV-COL(WS-I, WS-J) NOT = 0
                DISPLAY "  Propagating clue at " WS-I "," WS-J
                    " = " GV-COL(WS-I, WS-J)
                MOVE GV-COL(WS-I, WS-J) TO ELIM-DIGIT
                MOVE WS-I TO ELIM-ROW
                MOVE WS-J TO ELIM-COL
                *> Use internal elimination (don't count as iteration)
                PERFORM ELIMINATE-FROM-PEERS
            END-IF
        END-PERFORM
    END-PERFORM
    DISPLAY "Done with initial clue propagation".

CP-SOLVE.
    *> Initial propagation
    PERFORM PROPAGATE
    DISPLAY "After initial propagate: result=" RESULT-FLAG
        " iter=" CP-ITERATIONS
    IF RESULT-FLAG = 'N'
        DISPLAY "Initial propagation failed"
        EXIT PARAGRAPH
    END-IF

    *> Iterative search with explicit stack
    PERFORM UNTIL SOLVED-FLAG = 'Y'
        *> Find MRV cell
        PERFORM FIND-MRV-CELL

        IF MRV-FOUND = 'N'
            *> No empty cells - solved!
            MOVE 'Y' TO SOLVED-FLAG
            EXIT PARAGRAPH
        END-IF

        *> Push new search level
        ADD 1 TO STACK-PTR
        IF STACK-PTR > 81
            MOVE 'N' TO SOLVED-FLAG
            EXIT PARAGRAPH
        END-IF
        MOVE MRV-R TO SS-R(STACK-PTR)
        MOVE MRV-C TO SS-C(STACK-PTR)
        MOVE 1 TO SS-NEXT-DIGIT(STACK-PTR)

        PERFORM TRY-NEXT-CANDIDATE
        IF RESULT-FLAG = 'N'
            PERFORM BACKTRACK
            IF RESULT-FLAG = 'N'
                *> No solution
                EXIT PARAGRAPH
            END-IF
        END-IF
    END-PERFORM.

TRY-NEXT-CANDIDATE.
    MOVE SS-R(STACK-PTR) TO WS-ROW
    MOVE SS-C(STACK-PTR) TO WS-COL
    MOVE GC-COL(WS-ROW, WS-COL) TO CANDIDATE-SET

    *> Find next untried candidate
    PERFORM VARYING WS-DIGIT FROM SS-NEXT-DIGIT(STACK-PTR) BY 1
        UNTIL WS-DIGIT > 9
        *> Check if digit is a candidate
        MOVE WS-DIGIT TO ELIM-DIGIT
        PERFORM HAS-CANDIDATE-BIT
        IF RESULT-FLAG = 'Y'
            *> Update next digit to try
            ADD 1 TO WS-DIGIT
            MOVE WS-DIGIT TO SS-NEXT-DIGIT(STACK-PTR)
            SUBTRACT 1 FROM WS-DIGIT

            *> Save state
            PERFORM SAVE-STATE

            *> Try this assignment
            MOVE WS-DIGIT TO ELIM-DIGIT
            MOVE WS-ROW TO ELIM-ROW
            MOVE WS-COL TO ELIM-COL
            PERFORM ASSIGN-DIGIT
            IF RESULT-FLAG = 'Y'
                PERFORM PROPAGATE
                IF RESULT-FLAG = 'Y'
                    *> Success - continue search
                    EXIT PARAGRAPH
                END-IF
            END-IF

            *> Failed - restore and try next
            PERFORM RESTORE-STATE
        END-IF
    END-PERFORM

    *> No more candidates
    MOVE 'N' TO RESULT-FLAG.

BACKTRACK.
    *> Pop stack until we find a cell with untried candidates
    PERFORM UNTIL STACK-PTR = 0
        PERFORM RESTORE-STATE

        PERFORM TRY-NEXT-CANDIDATE
        IF RESULT-FLAG = 'Y'
            EXIT PARAGRAPH
        END-IF

        SUBTRACT 1 FROM STACK-PTR
    END-PERFORM

    MOVE 'N' TO RESULT-FLAG.

SAVE-STATE.
    ADD 1 TO STATE-PTR
    PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 9
        PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 9
            MOVE GV-COL(WS-I, WS-J)
                TO SL-COL(STATE-PTR, WS-I, WS-J)
            MOVE GC-COL(WS-I, WS-J)
                TO SLC-COL(STATE-PTR, WS-I, WS-J)
        END-PERFORM
    END-PERFORM.

RESTORE-STATE.
    PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 9
        PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 9
            MOVE SL-COL(STATE-PTR, WS-I, WS-J)
                TO GV-COL(WS-I, WS-J)
            MOVE SLC-COL(STATE-PTR, WS-I, WS-J)
                TO GC-COL(WS-I, WS-J)
        END-PERFORM
    END-PERFORM
    SUBTRACT 1 FROM STATE-PTR.

ASSIGN-DIGIT.
    *> Increment iteration counter
    ADD 1 TO CP-ITERATIONS
    DISPLAY "  ASSIGN-DIGIT: row=" ELIM-ROW " col=" ELIM-COL
        " digit=" ELIM-DIGIT

    *> Set value
    MOVE ELIM-DIGIT TO GV-COL(ELIM-ROW, ELIM-COL)
    MOVE POWER-VAL(ELIM-DIGIT + 1) TO GC-COL(ELIM-ROW, ELIM-COL)

    *> Eliminate digit from all peers
    *> Row peers
    DISPLAY "    Eliminating from row peers..."
    PERFORM VARYING ASSIGN-J FROM 1 BY 1 UNTIL ASSIGN-J > 9
        IF ASSIGN-J NOT = ELIM-COL
            MOVE ELIM-ROW TO WS-ROW
            MOVE ASSIGN-J TO WS-COL
            PERFORM ELIMINATE-FROM-PEER
            IF RESULT-FLAG = 'N'
                DISPLAY "    Row peer failed at col " ASSIGN-J
                EXIT PARAGRAPH
            END-IF
        END-IF
    END-PERFORM

    *> Column peers
    DISPLAY "    Eliminating from column peers..."
    PERFORM VARYING ASSIGN-I FROM 1 BY 1 UNTIL ASSIGN-I > 9
        IF ASSIGN-I NOT = ELIM-ROW
            MOVE ASSIGN-I TO WS-ROW
            MOVE ELIM-COL TO WS-COL
            PERFORM ELIMINATE-FROM-PEER
            IF RESULT-FLAG = 'N'
                DISPLAY "    Col peer failed at row " ASSIGN-I
                EXIT PARAGRAPH
            END-IF
        END-IF
    END-PERFORM

    *> Box peers
    COMPUTE BOX-R = ((ELIM-ROW - 1) / 3) * 3 + 1
    COMPUTE BOX-C = ((ELIM-COL - 1) / 3) * 3 + 1
    DISPLAY "    Eliminating from box peers (box " BOX-R "," BOX-C ")..."

    PERFORM VARYING ASSIGN-I FROM 0 BY 1 UNTIL ASSIGN-I > 2
        PERFORM VARYING ASSIGN-J FROM 0 BY 1 UNTIL ASSIGN-J > 2
            COMPUTE TEMP-R = BOX-R + ASSIGN-I
            COMPUTE TEMP-C = BOX-C + ASSIGN-J
            DISPLAY "      Box check: I=" ASSIGN-I " J=" ASSIGN-J
                " -> (" TEMP-R "," TEMP-C ")"
            IF TEMP-R NOT = ELIM-ROW OR TEMP-C NOT = ELIM-COL
                MOVE TEMP-R TO WS-ROW
                MOVE TEMP-C TO WS-COL
                PERFORM ELIMINATE-FROM-PEER
                IF RESULT-FLAG = 'N'
                    DISPLAY "    Box peer failed at " TEMP-R "," TEMP-C
                    EXIT PARAGRAPH
                END-IF
            END-IF
        END-PERFORM
    END-PERFORM

    MOVE 'Y' TO RESULT-FLAG.

ELIMINATE-FROM-PEER.
    *> Check if digit is already eliminated
    MOVE GC-COL(WS-ROW, WS-COL) TO CANDIDATE-SET
    PERFORM HAS-CANDIDATE-BIT
    IF RESULT-FLAG = 'N'
        MOVE 'Y' TO RESULT-FLAG
        EXIT PARAGRAPH
    END-IF

    DISPLAY "      Eliminating " ELIM-DIGIT " from (" WS-ROW "," WS-COL
        ") cands=" GC-COL(WS-ROW, WS-COL)

    *> Remove digit from candidates
    MOVE POWER-VAL(ELIM-DIGIT + 1) TO BIT-MASK
    SUBTRACT BIT-MASK FROM GC-COL(WS-ROW, WS-COL)

    *> Check for contradiction
    MOVE GC-COL(WS-ROW, WS-COL) TO CANDIDATE-SET
    PERFORM COUNT-BITS
    IF CANDIDATE-COUNT = 0
        DISPLAY "      -> Contradiction! Now has 0 candidates"
        MOVE 'N' TO RESULT-FLAG
        EXIT PARAGRAPH
    END-IF

    MOVE 'Y' TO RESULT-FLAG.

*> Eliminate digit from all peers (for initial clue propagation)
ELIMINATE-FROM-PEERS.
    *> Row peers
    PERFORM VARYING ELIM-J FROM 1 BY 1 UNTIL ELIM-J > 9
        IF ELIM-J NOT = ELIM-COL
            MOVE ELIM-ROW TO WS-ROW
            MOVE ELIM-J TO WS-COL
            PERFORM ELIMINATE-FROM-PEER-SILENT
        END-IF
    END-PERFORM

    *> Column peers
    PERFORM VARYING ELIM-I FROM 1 BY 1 UNTIL ELIM-I > 9
        IF ELIM-I NOT = ELIM-ROW
            MOVE ELIM-I TO WS-ROW
            MOVE ELIM-COL TO WS-COL
            PERFORM ELIMINATE-FROM-PEER-SILENT
        END-IF
    END-PERFORM

    *> Box peers
    COMPUTE BOX-R = ((ELIM-ROW - 1) / 3) * 3 + 1
    COMPUTE BOX-C = ((ELIM-COL - 1) / 3) * 3 + 1

    PERFORM VARYING ELIM-I FROM 0 BY 1 UNTIL ELIM-I > 2
        PERFORM VARYING ELIM-J FROM 0 BY 1 UNTIL ELIM-J > 2
            COMPUTE TEMP-R = BOX-R + ELIM-I
            COMPUTE TEMP-C = BOX-C + ELIM-J
            IF TEMP-R NOT = ELIM-ROW OR TEMP-C NOT = ELIM-COL
                MOVE TEMP-R TO WS-ROW
                MOVE TEMP-C TO WS-COL
                PERFORM ELIMINATE-FROM-PEER-SILENT
            END-IF
        END-PERFORM
    END-PERFORM.

*> Silent elimination (no contradiction check needed for initial setup)
ELIMINATE-FROM-PEER-SILENT.
    *> Check if digit is already eliminated
    MOVE GC-COL(WS-ROW, WS-COL) TO CANDIDATE-SET
    PERFORM HAS-CANDIDATE-BIT
    IF RESULT-FLAG = 'N'
        EXIT PARAGRAPH
    END-IF

    *> Remove digit from candidates
    MOVE POWER-VAL(ELIM-DIGIT + 1) TO BIT-MASK
    SUBTRACT BIT-MASK FROM GC-COL(WS-ROW, WS-COL).

PROPAGATE.
    MOVE 'Y' TO CHANGED-FLAG
    MOVE 'Y' TO RESULT-FLAG

    PERFORM UNTIL CHANGED-FLAG = 'N'
        MOVE 'N' TO CHANGED-FLAG

        *> Strategy 1: Singleton elimination
        PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 9
            PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 9
                IF GV-COL(WS-I, WS-J) = 0
                    MOVE GC-COL(WS-I, WS-J) TO CANDIDATE-SET
                    PERFORM COUNT-BITS
                    IF CANDIDATE-COUNT = 0
                        DISPLAY "Contradiction: cell " WS-I "," WS-J
                            " has 0 candidates, cands=" CANDIDATE-SET
                        MOVE 'N' TO RESULT-FLAG
                        EXIT PARAGRAPH
                    END-IF
                    IF CANDIDATE-COUNT = 1
                        PERFORM GET-FIRST-BIT
                        DISPLAY "Singleton: cell " WS-I "," WS-J
                            " = " WS-DIGIT
                        MOVE WS-DIGIT TO ELIM-DIGIT
                        MOVE WS-I TO ELIM-ROW
                        MOVE WS-J TO ELIM-COL
                        PERFORM ASSIGN-DIGIT
                        IF RESULT-FLAG = 'N'
                            DISPLAY "  ASSIGN-DIGIT failed!"
                            EXIT PARAGRAPH
                        END-IF
                        MOVE 'Y' TO CHANGED-FLAG
                    END-IF
                END-IF
            END-PERFORM
        END-PERFORM

        IF RESULT-FLAG = 'N'
            EXIT PARAGRAPH
        END-IF

        *> Strategy 2: Hidden singles - rows
        PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 9
            PERFORM VARYING WS-DIGIT FROM 1 BY 1 UNTIL WS-DIGIT > 9
                MOVE 0 TO HS-COUNT
                MOVE 0 TO HS-LAST-C
                MOVE 'N' TO HS-ASSIGNED

                PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 9
                    IF GV-COL(WS-I, WS-J) = WS-DIGIT
                        MOVE 'Y' TO HS-ASSIGNED
                        EXIT PERFORM
                    END-IF
                    MOVE GC-COL(WS-I, WS-J) TO CANDIDATE-SET
                    MOVE WS-DIGIT TO ELIM-DIGIT
                    PERFORM HAS-CANDIDATE-BIT
                    IF RESULT-FLAG = 'Y'
                        ADD 1 TO HS-COUNT
                        MOVE WS-J TO HS-LAST-C
                    END-IF
                END-PERFORM

                IF HS-ASSIGNED = 'N'
                    IF HS-COUNT = 1
                        MOVE WS-DIGIT TO ELIM-DIGIT
                        MOVE WS-I TO ELIM-ROW
                        MOVE HS-LAST-C TO ELIM-COL
                        PERFORM ASSIGN-DIGIT
                        IF RESULT-FLAG = 'N'
                            EXIT PARAGRAPH
                        END-IF
                        MOVE 'Y' TO CHANGED-FLAG
                    ELSE
                        IF HS-COUNT = 0
                            MOVE 'N' TO RESULT-FLAG
                            EXIT PARAGRAPH
                        END-IF
                    END-IF
                END-IF
            END-PERFORM
        END-PERFORM

        *> Strategy 2: Hidden singles - columns
        PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 9
            PERFORM VARYING WS-DIGIT FROM 1 BY 1 UNTIL WS-DIGIT > 9
                MOVE 0 TO HS-COUNT
                MOVE 0 TO HS-LAST-R
                MOVE 'N' TO HS-ASSIGNED

                PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 9
                    IF GV-COL(WS-I, WS-J) = WS-DIGIT
                        MOVE 'Y' TO HS-ASSIGNED
                        EXIT PERFORM
                    END-IF
                    MOVE GC-COL(WS-I, WS-J) TO CANDIDATE-SET
                    MOVE WS-DIGIT TO ELIM-DIGIT
                    PERFORM HAS-CANDIDATE-BIT
                    IF RESULT-FLAG = 'Y'
                        ADD 1 TO HS-COUNT
                        MOVE WS-I TO HS-LAST-R
                    END-IF
                END-PERFORM

                IF HS-ASSIGNED = 'N'
                    IF HS-COUNT = 1
                        MOVE WS-DIGIT TO ELIM-DIGIT
                        MOVE HS-LAST-R TO ELIM-ROW
                        MOVE WS-J TO ELIM-COL
                        PERFORM ASSIGN-DIGIT
                        IF RESULT-FLAG = 'N'
                            EXIT PARAGRAPH
                        END-IF
                        MOVE 'Y' TO CHANGED-FLAG
                    ELSE
                        IF HS-COUNT = 0
                            MOVE 'N' TO RESULT-FLAG
                            EXIT PARAGRAPH
                        END-IF
                    END-IF
                END-IF
            END-PERFORM
        END-PERFORM

        *> Strategy 2: Hidden singles - boxes
        PERFORM VARYING BOX-NUM FROM 0 BY 1 UNTIL BOX-NUM > 8
            COMPUTE BOX-R = (BOX-NUM / 3) * 3 + 1
            COMPUTE BOX-C = FUNCTION MOD(BOX-NUM, 3) * 3 + 1

            PERFORM VARYING WS-DIGIT FROM 1 BY 1 UNTIL WS-DIGIT > 9
                MOVE 0 TO HS-COUNT
                MOVE 0 TO HS-LAST-R
                MOVE 0 TO HS-LAST-C
                MOVE 'N' TO HS-ASSIGNED

                PERFORM VARYING WS-I FROM 0 BY 1 UNTIL WS-I > 2
                    PERFORM VARYING WS-J FROM 0 BY 1 UNTIL WS-J > 2
                        COMPUTE TEMP-R = BOX-R + WS-I
                        COMPUTE TEMP-C = BOX-C + WS-J
                        IF GV-COL(TEMP-R, TEMP-C) = WS-DIGIT
                            MOVE 'Y' TO HS-ASSIGNED
                        END-IF
                        IF HS-ASSIGNED = 'N'
                            MOVE GC-COL(TEMP-R, TEMP-C) TO CANDIDATE-SET
                            MOVE WS-DIGIT TO ELIM-DIGIT
                            PERFORM HAS-CANDIDATE-BIT
                            IF RESULT-FLAG = 'Y'
                                ADD 1 TO HS-COUNT
                                MOVE TEMP-R TO HS-LAST-R
                                MOVE TEMP-C TO HS-LAST-C
                            END-IF
                        END-IF
                    END-PERFORM
                END-PERFORM

                IF HS-ASSIGNED = 'N'
                    IF HS-COUNT = 1
                        MOVE WS-DIGIT TO ELIM-DIGIT
                        MOVE HS-LAST-R TO ELIM-ROW
                        MOVE HS-LAST-C TO ELIM-COL
                        PERFORM ASSIGN-DIGIT
                        IF RESULT-FLAG = 'N'
                            EXIT PARAGRAPH
                        END-IF
                        MOVE 'Y' TO CHANGED-FLAG
                    ELSE
                        IF HS-COUNT = 0
                            MOVE 'N' TO RESULT-FLAG
                            EXIT PARAGRAPH
                        END-IF
                    END-IF
                END-IF
            END-PERFORM
        END-PERFORM
    END-PERFORM

    MOVE 'Y' TO RESULT-FLAG.

FIND-MRV-CELL.
    MOVE 10 TO MIN-CANDIDATES
    MOVE 0 TO MRV-R
    MOVE 0 TO MRV-C
    MOVE 'N' TO MRV-FOUND

    PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 9
        PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 9
            IF GV-COL(WS-I, WS-J) = 0
                MOVE GC-COL(WS-I, WS-J) TO CANDIDATE-SET
                PERFORM COUNT-BITS
                MOVE CANDIDATE-COUNT TO NUM-CANDIDATES
                IF NUM-CANDIDATES < MIN-CANDIDATES
                    MOVE NUM-CANDIDATES TO MIN-CANDIDATES
                    MOVE WS-I TO MRV-R
                    MOVE WS-J TO MRV-C
                    MOVE 'Y' TO MRV-FOUND
                END-IF
            END-IF
        END-PERFORM
    END-PERFORM.

*> Check if ELIM-DIGIT is set in CANDIDATE-SET
HAS-CANDIDATE-BIT.
    MOVE POWER-VAL(ELIM-DIGIT + 1) TO BIT-MASK
    MOVE CANDIDATE-SET TO BIT-WORK
    DIVIDE BIT-WORK BY BIT-MASK GIVING BIT-QUOT
        REMAINDER BIT-REM
    DIVIDE BIT-QUOT BY 2 GIVING BIT-WORK REMAINDER BIT-REM
    IF BIT-REM = 1
        MOVE 'Y' TO RESULT-FLAG
    ELSE
        MOVE 'N' TO RESULT-FLAG
    END-IF.

*> Get first set bit (1-9) from CANDIDATE-SET, result in WS-DIGIT
*> Preserves ELIM-DIGIT
GET-FIRST-BIT.
    MOVE ELIM-DIGIT TO GET-SAVED-DIGIT
    MOVE 0 TO WS-DIGIT
    PERFORM VARYING GET-K FROM 1 BY 1 UNTIL GET-K > 9
        OR WS-DIGIT > 0
        MOVE GET-K TO ELIM-DIGIT
        PERFORM HAS-CANDIDATE-BIT
        IF RESULT-FLAG = 'Y'
            MOVE GET-K TO WS-DIGIT
        END-IF
    END-PERFORM
    MOVE GET-SAVED-DIGIT TO ELIM-DIGIT.

*> Count set bits in CANDIDATE-SET, result in CANDIDATE-COUNT
*> Preserves ELIM-DIGIT
COUNT-BITS.
    MOVE ELIM-DIGIT TO COUNT-DIGIT
    MOVE 0 TO CANDIDATE-COUNT
    PERFORM VARYING COUNT-K FROM 1 BY 1 UNTIL COUNT-K > 9
        MOVE COUNT-K TO ELIM-DIGIT
        PERFORM HAS-CANDIDATE-BIT
        IF RESULT-FLAG = 'Y'
            ADD 1 TO CANDIDATE-COUNT
        END-IF
    END-PERFORM
    MOVE COUNT-DIGIT TO ELIM-DIGIT.

END PROGRAM CP-SUDOKU.
