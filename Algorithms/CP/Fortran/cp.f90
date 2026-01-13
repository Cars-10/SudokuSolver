! CP Sudoku Solver in Fortran
! Constraint Propagation with MRV heuristic
! Uses bitsets for candidate tracking

MODULE CP_TYPES
    IMPLICIT NONE

    ! Grid structure with assigned values and candidate tracking
    TYPE :: CPGrid
        INTEGER, DIMENSION(0:8,0:8) :: values       ! Assigned values (0 = empty)
        INTEGER(KIND=2), DIMENSION(0:8,0:8) :: candidates  ! Bitset for candidates (16-bit)
    END TYPE CPGrid

    ! Global iteration counter
    INTEGER(KIND=8) :: cp_iterations

CONTAINS

    ! Count number of candidates in a bitset
    INTEGER FUNCTION count_candidates(cs)
        INTEGER(KIND=2), INTENT(IN) :: cs
        INTEGER :: digit, count

        count = 0
        DO digit = 1, 9
            IF (IAND(cs, INT(ISHFT(1, digit), KIND=2)) /= 0) THEN
                count = count + 1
            END IF
        END DO
        count_candidates = count
    END FUNCTION count_candidates

    ! Get first candidate digit from bitset (1-9)
    INTEGER FUNCTION get_first_candidate(cs)
        INTEGER(KIND=2), INTENT(IN) :: cs
        INTEGER :: digit

        DO digit = 1, 9
            IF (IAND(cs, INT(ISHFT(1, digit), KIND=2)) /= 0) THEN
                get_first_candidate = digit
                RETURN
            END IF
        END DO
        get_first_candidate = 0
    END FUNCTION get_first_candidate

    ! Check if candidate exists in bitset
    LOGICAL FUNCTION has_candidate(cs, digit)
        INTEGER(KIND=2), INTENT(IN) :: cs
        INTEGER, INTENT(IN) :: digit
        has_candidate = (IAND(cs, INT(ISHFT(1, digit), KIND=2)) /= 0)
    END FUNCTION has_candidate

    ! Remove candidate from bitset
    SUBROUTINE remove_candidate(cs, digit)
        INTEGER(KIND=2), INTENT(INOUT) :: cs
        INTEGER, INTENT(IN) :: digit
        cs = IAND(cs, NOT(INT(ISHFT(1, digit), KIND=2)))
    END SUBROUTINE remove_candidate

END MODULE CP_TYPES

PROGRAM CP_SUDOKU
    USE CP_TYPES
    IMPLICIT NONE
    INTEGER :: num_args, ix
    INTEGER, DIMENSION(0:8,0:8) :: puzzle, solution_grid
    CHARACTER(len=200) :: arg
    REAL :: start_time, end_time
    TYPE(CPGrid) :: grid
    INTEGER, DIMENSION(0:80) :: solution
    INTEGER :: solved

    CALL CPU_TIME(start_time)

    num_args = COMMAND_ARGUMENT_COUNT()

    DO ix = 1, num_args
        CALL GET_COMMAND_ARGUMENT(ix, arg)

        IF (INDEX(arg, ".matrix", BACK=.TRUE.) > 0) THEN
            CALL read_matrix_file(arg, puzzle)
            CALL print_puzzle(puzzle)

            ! Initialize CP grid
            CALL init_grid(grid, puzzle)

            ! Apply initial propagation
            IF (.NOT. propagate(grid)) THEN
                WRITE(*,'(/,A)') 'No solution found (contradiction during initial propagation)'
                CYCLE
            END IF

            ! Run search
            cp_iterations = 0
            CALL cp_search(grid, solution, solved)

            IF (solved == 1) THEN
                CALL extract_solution(solution, solution_grid)
                CALL print_puzzle(solution_grid)
                WRITE(*,'(/,A,I0,/)') 'Solved in Iterations=', cp_iterations
            ELSE
                WRITE(*,'(/,A)') 'No solution found'
            END IF
        END IF
    END DO

    CALL CPU_TIME(end_time)
    WRITE(*,'(A,F8.3)') 'Seconds to process ', end_time - start_time

CONTAINS

! ============================================================================
! INITIALIZATION
! ============================================================================

SUBROUTINE init_grid(grid, puzzle)
    TYPE(CPGrid), INTENT(OUT) :: grid
    INTEGER, DIMENSION(0:8,0:8), INTENT(IN) :: puzzle
    INTEGER :: row, col, digit

    DO row = 0, 8
        DO col = 0, 8
            IF (puzzle(row, col) == 0) THEN
                ! Empty cell: set all candidates 1-9 (bits 1-9 set)
                grid%values(row, col) = 0
                grid%candidates(row, col) = INT(Z'03FE', KIND=2)  ! Binary: 0011 1111 1110
            ELSE
                ! Given clue: set single value
                digit = puzzle(row, col)
                grid%values(row, col) = digit
                grid%candidates(row, col) = INT(ISHFT(1, digit), KIND=2)
            END IF
        END DO
    END DO
END SUBROUTINE init_grid

! ============================================================================
! CONSTRAINT PROPAGATION
! ============================================================================

FUNCTION eliminate(grid, row, col, digit) RESULT(success)
    TYPE(CPGrid), INTENT(INOUT) :: grid
    INTEGER, INTENT(IN) :: row, col, digit
    INTEGER :: success
    INTEGER :: remaining, last_digit

    ! Check if digit is already eliminated
    IF (.NOT. has_candidate(grid%candidates(row, col), digit)) THEN
        success = 1  ! Already eliminated, no change
        RETURN
    END IF

    ! Remove digit from candidates
    CALL remove_candidate(grid%candidates(row, col), digit)

    ! Check for contradiction (no candidates left)
    remaining = count_candidates(grid%candidates(row, col))
    IF (remaining == 0) THEN
        success = 0  ! Contradiction
        RETURN
    END IF

    ! If only one candidate left, assign it (singleton elimination)
    IF (remaining == 1 .AND. grid%values(row, col) == 0) THEN
        last_digit = get_first_candidate(grid%candidates(row, col))
        success = assign(grid, row, col, last_digit)
        RETURN
    END IF

    success = 1
END FUNCTION eliminate

FUNCTION assign(grid, row, col, digit) RESULT(success)
    TYPE(CPGrid), INTENT(INOUT) :: grid
    INTEGER, INTENT(IN) :: row, col, digit
    INTEGER :: success
    INTEGER :: peers(0:19,0:1), i, peer_row, peer_col

    ! Increment iteration counter (this is our benchmark metric)
    cp_iterations = cp_iterations + 1

    ! Set value
    grid%values(row, col) = digit
    grid%candidates(row, col) = INT(ISHFT(1, digit), KIND=2)

    ! Eliminate digit from all peers
    CALL get_peers(row, col, peers)

    DO i = 0, 19
        peer_row = peers(i, 0)
        peer_col = peers(i, 1)

        IF (eliminate(grid, peer_row, peer_col, digit) == 0) THEN
            success = 0  ! Contradiction in peer elimination
            RETURN
        END IF
    END DO

    success = 1
END FUNCTION assign

FUNCTION propagate(grid) RESULT(success)
    TYPE(CPGrid), INTENT(INOUT) :: grid
    LOGICAL :: success
    INTEGER :: changed, row, col, num_candidates, digit
    INTEGER :: count, last_row, last_col, found, box, box_row, box_col, r, c, last_r, last_c

    changed = 1

    DO WHILE (changed == 1)
        changed = 0

        ! Strategy 1: Singleton elimination
        DO row = 0, 8
            DO col = 0, 8
                IF (grid%values(row, col) == 0) THEN
                    num_candidates = count_candidates(grid%candidates(row, col))
                    IF (num_candidates == 0) THEN
                        success = .FALSE.
                        RETURN
                    END IF
                    IF (num_candidates == 1) THEN
                        digit = get_first_candidate(grid%candidates(row, col))
                        IF (assign(grid, row, col, digit) == 0) THEN
                            success = .FALSE.
                            RETURN
                        END IF
                        changed = 1
                    END IF
                END IF
            END DO
        END DO

        ! Strategy 2: Hidden singles - Rows
        DO row = 0, 8
            DO digit = 1, 9
                count = 0
                last_col = -1
                found = 0

                ! Check if digit is already assigned in this row
                DO col = 0, 8
                    IF (grid%values(row, col) == digit) THEN
                        found = 1
                        EXIT
                    END IF
                END DO

                IF (found == 0) THEN
                    DO col = 0, 8
                        IF (has_candidate(grid%candidates(row, col), digit)) THEN
                            count = count + 1
                            last_col = col
                        END IF
                    END DO

                    IF (count == 1) THEN
                        IF (assign(grid, row, last_col, digit) == 0) THEN
                            success = .FALSE.
                            RETURN
                        END IF
                        changed = 1
                    ELSE IF (count == 0) THEN
                        success = .FALSE.
                        RETURN
                    END IF
                END IF
            END DO
        END DO

        ! Strategy 2: Hidden singles - Columns
        DO col = 0, 8
            DO digit = 1, 9
                count = 0
                last_row = -1
                found = 0

                ! Check if digit is already assigned in this column
                DO row = 0, 8
                    IF (grid%values(row, col) == digit) THEN
                        found = 1
                        EXIT
                    END IF
                END DO

                IF (found == 0) THEN
                    DO row = 0, 8
                        IF (has_candidate(grid%candidates(row, col), digit)) THEN
                            count = count + 1
                            last_row = row
                        END IF
                    END DO

                    IF (count == 1) THEN
                        IF (assign(grid, last_row, col, digit) == 0) THEN
                            success = .FALSE.
                            RETURN
                        END IF
                        changed = 1
                    ELSE IF (count == 0) THEN
                        success = .FALSE.
                        RETURN
                    END IF
                END IF
            END DO
        END DO

        ! Strategy 2: Hidden singles - Boxes
        DO box = 0, 8
            box_row = (box / 3) * 3
            box_col = MOD(box, 3) * 3

            DO digit = 1, 9
                count = 0
                last_r = -1
                last_c = -1
                found = 0

                ! Check if digit is already assigned in this box
                DO r = box_row, box_row + 2
                    DO c = box_col, box_col + 2
                        IF (grid%values(r, c) == digit) THEN
                            found = 1
                            EXIT
                        END IF
                    END DO
                    IF (found == 1) EXIT
                END DO

                IF (found == 0) THEN
                    DO r = box_row, box_row + 2
                        DO c = box_col, box_col + 2
                            IF (has_candidate(grid%candidates(r, c), digit)) THEN
                                count = count + 1
                                last_r = r
                                last_c = c
                            END IF
                        END DO
                    END DO

                    IF (count == 1) THEN
                        IF (assign(grid, last_r, last_c, digit) == 0) THEN
                            success = .FALSE.
                            RETURN
                        END IF
                        changed = 1
                    ELSE IF (count == 0) THEN
                        success = .FALSE.
                        RETURN
                    END IF
                END IF
            END DO
        END DO
    END DO

    success = .TRUE.
END FUNCTION propagate

! ============================================================================
! SEARCH
! ============================================================================

FUNCTION find_mrv_cell(grid, mrv_row, mrv_col) RESULT(found)
    TYPE(CPGrid), INTENT(IN) :: grid
    INTEGER, INTENT(OUT) :: mrv_row, mrv_col
    INTEGER :: found
    INTEGER :: min_candidates, r, c, num_candidates

    min_candidates = 10  ! More than 9, so any cell will be smaller
    found = 0

    DO r = 0, 8
        DO c = 0, 8
            IF (grid%values(r, c) == 0) THEN
                num_candidates = count_candidates(grid%candidates(r, c))
                IF (num_candidates < min_candidates) THEN
                    min_candidates = num_candidates
                    mrv_row = r
                    mrv_col = c
                    found = 1
                END IF
            END IF
        END DO
    END DO
END FUNCTION find_mrv_cell

RECURSIVE SUBROUTINE cp_search(grid, solution, solved)
    TYPE(CPGrid), INTENT(INOUT) :: grid
    INTEGER, DIMENSION(0:80), INTENT(OUT) :: solution
    INTEGER, INTENT(OUT) :: solved
    INTEGER :: mrv_row, mrv_col, found, digit, r, c
    INTEGER(KIND=2) :: candidates
    TYPE(CPGrid) :: grid_copy

    ! Base case: check if grid is complete
    found = find_mrv_cell(grid, mrv_row, mrv_col)
    IF (found == 0) THEN
        ! No empty cells - grid is complete, extract solution
        DO r = 0, 8
            DO c = 0, 8
                solution(r * 9 + c) = grid%values(r, c)
            END DO
        END DO
        solved = 1
        RETURN
    END IF

    ! Recursive case: try each candidate for the MRV cell
    candidates = grid%candidates(mrv_row, mrv_col)

    DO digit = 1, 9
        IF (has_candidate(candidates, digit)) THEN
            ! Save grid state for backtracking
            grid_copy = grid

            ! Try assigning this digit
            IF (assign(grid, mrv_row, mrv_col, digit) == 1) THEN
                ! Assignment succeeded, propagate constraints
                IF (propagate(grid)) THEN
                    ! Propagation succeeded, recurse
                    CALL cp_search(grid, solution, solved)
                    IF (solved == 1) RETURN
                END IF
            END IF

            ! Failed - restore grid state and try next candidate
            grid = grid_copy
        END IF
    END DO

    ! All candidates exhausted - dead end
    solved = 0
END SUBROUTINE cp_search

! ============================================================================
! HELPER FUNCTIONS
! ============================================================================

SUBROUTINE get_peers(row, col, peers)
    INTEGER, INTENT(IN) :: row, col
    INTEGER, DIMENSION(0:19,0:1), INTENT(OUT) :: peers
    INTEGER :: idx, c, r, box_row, box_col

    idx = 0

    ! Same row (9 cells minus self = 8)
    DO c = 0, 8
        IF (c /= col) THEN
            peers(idx, 0) = row
            peers(idx, 1) = c
            idx = idx + 1
        END IF
    END DO

    ! Same column (9 cells minus self = 8)
    DO r = 0, 8
        IF (r /= row) THEN
            peers(idx, 0) = r
            peers(idx, 1) = col
            idx = idx + 1
        END IF
    END DO

    ! Same 3x3 box (9 cells minus self minus already counted = 4)
    box_row = (row / 3) * 3
    box_col = (col / 3) * 3
    DO r = box_row, box_row + 2
        DO c = box_col, box_col + 2
            IF (r /= row .AND. c /= col) THEN
                peers(idx, 0) = r
                peers(idx, 1) = c
                idx = idx + 1
            END IF
        END DO
    END DO
END SUBROUTINE get_peers

SUBROUTINE extract_solution(solution, solution_grid)
    INTEGER, DIMENSION(0:80), INTENT(IN) :: solution
    INTEGER, DIMENSION(0:8,0:8), INTENT(OUT) :: solution_grid
    INTEGER :: r, c

    DO r = 0, 8
        DO c = 0, 8
            solution_grid(r, c) = solution(r * 9 + c)
        END DO
    END DO
END SUBROUTINE extract_solution

! ============================================================================
! I/O ROUTINES
! ============================================================================

SUBROUTINE read_matrix_file(filename, puzzle)
    CHARACTER(len=*), INTENT(IN) :: filename
    INTEGER, DIMENSION(0:8,0:8), INTENT(OUT) :: puzzle
    CHARACTER(len=200) :: line, display_path
    INTEGER :: ios, line_count, c, slash_pos

    ! Normalize path for output
    slash_pos = INDEX(filename, '/Matrices/', BACK=.TRUE.)
    IF (slash_pos > 0) THEN
        display_path = '../' // filename(slash_pos+1:)
    ELSE
        display_path = filename
    END IF
    WRITE(*,'(A)') TRIM(display_path)

    OPEN(UNIT=9, FILE=filename, IOSTAT=ios, STATUS='old')
    IF (ios /= 0) THEN
        WRITE(*,'(A)') 'Error opening file'
        STOP
    END IF

    line_count = 0
    DO
        READ(9, '(A)', IOSTAT=ios) line
        IF (ios /= 0) EXIT
        IF (LEN_TRIM(line) == 0) CYCLE
        IF (line(1:1) == '#') CYCLE

        READ(line, *) puzzle(line_count, 0:8)

        ! Print row as we read it
        DO c = 0, 8
            WRITE(*, '(I1,A)', ADVANCE='no') puzzle(line_count, c), ' '
        END DO
        WRITE(*,*)

        line_count = line_count + 1
        IF (line_count >= 9) EXIT
    END DO

    CLOSE(UNIT=9)
END SUBROUTINE read_matrix_file

SUBROUTINE print_puzzle(puzzle)
    INTEGER, DIMENSION(0:8,0:8), INTENT(IN) :: puzzle
    INTEGER :: r, c

    WRITE(*,'(/,A)') 'Puzzle:'
    DO r = 0, 8
        DO c = 0, 8
            WRITE(*, '(I1,A)', ADVANCE='no') puzzle(r, c), ' '
        END DO
        WRITE(*,*)
    END DO
END SUBROUTINE print_puzzle

END PROGRAM CP_SUDOKU
