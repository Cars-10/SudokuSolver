PROGRAM SudokuSolver

  IMPLICIT NONE
  INTEGER, DIMENSION(81) :: board
  CHARACTER(len=32) :: filename
  INTEGER :: complexity, iterations

  ! Step 6: Read filename from command line
  CALL GET_COMMAND_ARGUMENT(1, filename)
  IF (filename == '') THEN
    PRINT *, "Please provide the Sudoku file name as a command line argument."
    STOP
  END IF

  ! Step 1: Read the Sudoku board from file
  CALL ReadBoardFromFile(filename, board)
  CALL PrintBoard(board)

  ! Step 2: Calculate the complexity of the board
  complexity = CalculateComplexity(board)
  PRINT *, "Complexity of the unsolved board: ", complexity

  ! Step 3: Print the unsolved board
  ! Already done above after reading the board

  ! Step 4: Solve the Sudoku board
  iterations = 0
  IF (SolveSudoku(board, iterations)) THEN
    PRINT *, "Sudoku solved in ", iterations, " iterations."

    ! Step 5: Print the final solved board
    CALL PrintBoard(board)
  ELSE
    PRINT *, "Failed to solve the Sudoku."
  END IF

CONTAINS

  SUBROUTINE ReadBoardFromFile(filename, board)
    CHARACTER(len=*), INTENT(IN) :: filename
    INTEGER, DIMENSION(81), INTENT(OUT) :: board
    INTEGER :: i, j, k, ios
    CHARACTER(len=100) :: line
    CHARACTER(len=1) :: ch
    LOGICAL :: commentFlag

    OPEN(UNIT=10, FILE=filename, ACTION='READ', STATUS='OLD', IOSTAT=ios)
    IF (ios /= 0) THEN
      PRINT *, "Error opening file: ", filename
      STOP
    END IF

    k = 1
    DO i = 1, 9
      READ(10, '(A)', IOSTAT=ios) line
      IF (ios /= 0) THEN
        PRINT *, "Error reading file: ", filename
        STOP
      END IF
      commentFlag = .FALSE.
      DO j = 1, LEN_TRIM(line)
        ch = line(j:j)
        IF (ch == '#') THEN
          commentFlag = .TRUE.
        END IF
        IF (.NOT. commentFlag) THEN
          IF (ch >= '0' .AND. ch <= '9') THEN
            board(k) = ICHAR(ch) - ICHAR('0')
            k = k + 1
          ELSE IF (ch == ' ' .OR. ch == '!') THEN
            ! Skip spaces and comments
          ELSE
            PRINT *, "Invalid character in file: ", ch
            STOP
          END IF
        END IF
      END DO
    END DO
    CLOSE(UNIT=10)
  END SUBROUTINE ReadBoardFromFile

  FUNCTION CalculateComplexity(board) RESULT(complexity)
    INTEGER, DIMENSION(81), INTENT(IN) :: board
    INTEGER :: complexity, i
    complexity = 0
    DO i = 1, 81
      IF (board(i) == 0) THEN
        complexity = complexity + 1
      END IF
    ENDDO
  END FUNCTION CalculateComplexity

  SUBROUTINE PrintBoard(board)
    INTEGER, DIMENSION(81), INTENT(IN) :: board
    INTEGER :: i, j
    DO i = 1, 9
      DO j = 1, 9
        WRITE(*, '(I1,1X)', ADVANCE='NO') board((i - 1) * 9 + j)
      END DO
      PRINT *, ''
    END DO
  END SUBROUTINE PrintBoard

  RECURSIVE FUNCTION SolveSudoku(board, iterations) RESULT(success)
    INTEGER, DIMENSION(81), INTENT(INOUT) :: board
    INTEGER, INTENT(INOUT) :: iterations
    INTEGER :: i, row, col, num, start
    LOGICAL :: success
    success = .FALSE.
    iterations = iterations + 1
    IF (ALL(board /= 0)) THEN
      success = .TRUE.
      RETURN
    END IF
    DO i = 1, 81
      IF (board(i) == 0) THEN
        row = (i - 1) / 9 + 1
        col = MOD(i - 1, 9) + 1
        DO num = 1, 9
          IF (IsSafe(board, row, col, num)) THEN
            board(i) = num
            IF (SolveSudoku(board, iterations)) THEN
              success = .TRUE.
              RETURN
            ELSE
              board(i) = 0
            END IF
          END IF
        ENDDO
        EXIT
      END IF
    END DO
  END FUNCTION SolveSudoku

  FUNCTION IsSafe(board, row, col, num) RESULT(safe)
    INTEGER, DIMENSION(81), INTENT(IN) :: board
    INTEGER, INTENT(IN) :: row, col, num
    INTEGER :: i, j, startRow, startCol
    LOGICAL :: safe
    safe = .TRUE.
    startRow = (row - 1) / 3 * 3 + 1
    startCol = (col - 1) / 3 * 3 + 1
    DO i = startRow, startRow + 2
      DO j = startCol, startCol + 2
        IF (board((i - 1) * 9 + j) == num) THEN
          safe = .FALSE.
          RETURN
        END IF
      END DO
    ENDDO
    DO i = 1, 9
      IF (board((row - 1) * 9 + i) == num .OR. board((i - 1) * 9 + col) == num) THEN
        safe = .FALSE.
        RETURN
      END IF
    END DO
  END FUNCTION IsSafe

END PROGRAM SudokuSolver
