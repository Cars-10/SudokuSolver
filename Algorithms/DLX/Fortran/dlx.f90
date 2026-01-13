! DLX Sudoku Solver in Fortran
! Implementation of Knuth's Dancing Links Algorithm X for exact cover
! Uses array-based circular doubly-linked lists instead of pointers

MODULE DLX_TYPES
    IMPLICIT NONE

    ! DLX Node structure (using array indices instead of pointers)
    ! Index 0 is reserved as NULL
    TYPE :: DlxNode
        INTEGER :: up, down, left, right  ! Indices in node array
        INTEGER :: column                  ! Index of column header
        INTEGER :: row_id                  ! ID of the row this node belongs to
    END TYPE DlxNode

    ! Column header (extends DlxNode with size and name)
    TYPE :: DlxColumn
        INTEGER :: node_idx                ! Index of this column's node
        INTEGER :: size                    ! Number of nodes in this column
        CHARACTER(len=16) :: name
    END TYPE DlxColumn

END MODULE DLX_TYPES

PROGRAM DLX_SUDOKU
    USE DLX_TYPES
    IMPLICIT NONE
    INTEGER :: num_args, ix, i, j, n
    INTEGER, DIMENSION(0:8,0:8) :: puzzle, solution_grid
    CHARACTER(len=200) :: arg
    REAL :: start_time, end_time
    INTEGER :: dlx_iterations
    INTEGER :: result

    CALL CPU_TIME(start_time)

    num_args = COMMAND_ARGUMENT_COUNT()

    DO ix = 1, num_args
        CALL GET_COMMAND_ARGUMENT(ix, arg)

        IF (INDEX(arg, ".matrix", BACK=.TRUE.) > 0) THEN
            CALL read_matrix_file(arg, puzzle)
            CALL print_puzzle(puzzle)

            ! Solve using DLX
            dlx_iterations = 0
            CALL solve_dlx(puzzle, solution_grid, result, dlx_iterations)

            IF (result == 1) THEN
                CALL print_puzzle(solution_grid)
                WRITE(*,'(/,A,I0,/)') 'Solved in Iterations=', dlx_iterations
            ELSE
                WRITE(*,'(/,A)') 'No solution found'
            END IF
        END IF
    END DO

    CALL CPU_TIME(end_time)
    WRITE(*,'(A,F8.3)') 'Seconds to process ', end_time - start_time

CONTAINS

! ============================================================================
! DLX ALGORITHM IMPLEMENTATION
! ============================================================================

SUBROUTINE solve_dlx(puzzle, solution_grid, result, iterations)
    USE DLX_TYPES
    INTEGER, DIMENSION(0:8,0:8), INTENT(IN) :: puzzle
    INTEGER, DIMENSION(0:8,0:8), INTENT(OUT) :: solution_grid
    INTEGER, INTENT(OUT) :: result
    INTEGER, INTENT(INOUT) :: iterations

    ! DLX matrix structures
    TYPE(DlxNode), ALLOCATABLE :: nodes(:)
    TYPE(DlxColumn), ALLOCATABLE :: columns(:)
    INTEGER :: root_idx
    INTEGER :: node_count, max_nodes
    INTEGER, DIMENSION(0:728,0:2) :: row_info  ! row, col, num for each DLX row
    INTEGER, DIMENSION(0:728) :: row_starts    ! First node index for each row
    INTEGER, DIMENSION(0:80) :: solution_rows
    INTEGER :: solution_count

    ! Initialize DLX matrix
    max_nodes = 729 * 4 + 325  ! 729 rows * 4 nodes + 324 columns + root
    ALLOCATE(nodes(0:max_nodes))
    ALLOCATE(columns(0:324))

    node_count = 0
    CALL init_dlx_matrix(nodes, columns, root_idx, node_count)
    CALL build_dlx_matrix(nodes, columns, node_count, puzzle, row_info, row_starts)
    CALL cover_clues(nodes, columns, puzzle, row_info, row_starts)

    ! Solve
    solution_count = 0
    CALL dlx_search(nodes, columns, root_idx, 0, solution_rows, solution_count, &
                   result, iterations)

    ! Extract solution
    IF (result == 1) THEN
        CALL extract_solution(puzzle, solution_grid, solution_rows, solution_count, row_info)
    END IF

    DEALLOCATE(nodes)
    DEALLOCATE(columns)
END SUBROUTINE solve_dlx

! Initialize DLX matrix structure
SUBROUTINE init_dlx_matrix(nodes, columns, root_idx, node_count)
    USE DLX_TYPES
    TYPE(DlxNode), INTENT(INOUT) :: nodes(0:)
    TYPE(DlxColumn), INTENT(INOUT) :: columns(0:)
    INTEGER, INTENT(OUT) :: root_idx, node_count
    INTEGER :: i, prev_idx

    ! Index 0 is NULL/unused
    ! Root is at index 1
    root_idx = 1
    node_count = 1

    ! Initialize root
    nodes(root_idx)%up = root_idx
    nodes(root_idx)%down = root_idx
    nodes(root_idx)%left = root_idx
    nodes(root_idx)%right = root_idx
    nodes(root_idx)%column = 0
    nodes(root_idx)%row_id = -1

    columns(0)%node_idx = root_idx
    columns(0)%size = 0
    columns(0)%name = 'root'

    ! Create 324 column headers (indices 2-325)
    prev_idx = root_idx
    DO i = 1, 324
        node_count = node_count + 1

        ! Initialize column node
        nodes(node_count)%up = node_count
        nodes(node_count)%down = node_count
        nodes(node_count)%column = i
        nodes(node_count)%row_id = -1

        ! Link into header list (left of root)
        nodes(node_count)%left = nodes(root_idx)%left
        nodes(node_count)%right = root_idx
        nodes(nodes(root_idx)%left)%right = node_count
        nodes(root_idx)%left = node_count

        ! Initialize column
        columns(i)%node_idx = node_count
        columns(i)%size = 0
        columns(i)%name = 'C'
    END DO
END SUBROUTINE init_dlx_matrix

! Calculate constraint column indices
INTEGER FUNCTION get_position_col(r, c)
    INTEGER, INTENT(IN) :: r, c
    get_position_col = r * 9 + c + 1  ! +1 for 1-based indexing
END FUNCTION get_position_col

INTEGER FUNCTION get_row_col(r, n)
    INTEGER, INTENT(IN) :: r, n
    get_row_col = 81 + r * 9 + (n - 1) + 1
END FUNCTION get_row_col

INTEGER FUNCTION get_col_col(c, n)
    INTEGER, INTENT(IN) :: c, n
    get_col_col = 162 + c * 9 + (n - 1) + 1
END FUNCTION get_col_col

INTEGER FUNCTION get_box_col(r, c, n)
    INTEGER, INTENT(IN) :: r, c, n
    INTEGER :: box
    box = (r / 3) * 3 + (c / 3)
    get_box_col = 243 + box * 9 + (n - 1) + 1
END FUNCTION get_box_col

! Add a node to the DLX matrix
SUBROUTINE add_node(nodes, columns, node_count, col_idx, row_id, new_node_idx)
    USE DLX_TYPES
    TYPE(DlxNode), INTENT(INOUT) :: nodes(0:)
    TYPE(DlxColumn), INTENT(INOUT) :: columns(0:)
    INTEGER, INTENT(INOUT) :: node_count
    INTEGER, INTENT(IN) :: col_idx, row_id
    INTEGER, INTENT(OUT) :: new_node_idx
    INTEGER :: col_node_idx

    node_count = node_count + 1
    new_node_idx = node_count
    col_node_idx = columns(col_idx)%node_idx

    ! Initialize node
    nodes(new_node_idx)%column = col_idx
    nodes(new_node_idx)%row_id = row_id

    ! Insert at end of column's circular list
    nodes(new_node_idx)%down = col_node_idx
    nodes(new_node_idx)%up = nodes(col_node_idx)%up
    nodes(nodes(col_node_idx)%up)%down = new_node_idx
    nodes(col_node_idx)%up = new_node_idx

    columns(col_idx)%size = columns(col_idx)%size + 1
END SUBROUTINE add_node

! Build a DLX row for Sudoku cell (r,c) with value n
SUBROUTINE build_dlx_row(nodes, columns, node_count, r, c, n, row_id, row_info, row_starts)
    USE DLX_TYPES
    TYPE(DlxNode), INTENT(INOUT) :: nodes(0:)
    TYPE(DlxColumn), INTENT(INOUT) :: columns(0:)
    INTEGER, INTENT(INOUT) :: node_count
    INTEGER, INTENT(IN) :: r, c, n, row_id
    INTEGER, INTENT(INOUT) :: row_info(0:,0:)
    INTEGER, INTENT(INOUT) :: row_starts(0:)
    INTEGER :: n1, n2, n3, n4
    INTEGER :: c1, c2, c3, c4

    ! Store row metadata
    row_info(row_id, 0) = r
    row_info(row_id, 1) = c
    row_info(row_id, 2) = n

    ! Create nodes for the 4 constraints
    c1 = get_position_col(r, c)
    c2 = get_row_col(r, n)
    c3 = get_col_col(c, n)
    c4 = get_box_col(r, c, n)

    CALL add_node(nodes, columns, node_count, c1, row_id, n1)
    CALL add_node(nodes, columns, node_count, c2, row_id, n2)
    CALL add_node(nodes, columns, node_count, c3, row_id, n3)
    CALL add_node(nodes, columns, node_count, c4, row_id, n4)

    ! Link nodes horizontally in circular list
    nodes(n1)%right = n2
    nodes(n2)%right = n3
    nodes(n3)%right = n4
    nodes(n4)%right = n1

    nodes(n1)%left = n4
    nodes(n2)%left = n1
    nodes(n3)%left = n2
    nodes(n4)%left = n3

    ! Store first node for this row
    row_starts(row_id) = n1
END SUBROUTINE build_dlx_row

! Build the complete DLX matrix from the puzzle
SUBROUTINE build_dlx_matrix(nodes, columns, node_count, puzzle, row_info, row_starts)
    USE DLX_TYPES
    TYPE(DlxNode), INTENT(INOUT) :: nodes(0:)
    TYPE(DlxColumn), INTENT(INOUT) :: columns(0:)
    INTEGER, INTENT(INOUT) :: node_count
    INTEGER, DIMENSION(0:8,0:8), INTENT(IN) :: puzzle
    INTEGER, INTENT(INOUT) :: row_info(0:,0:)
    INTEGER, INTENT(INOUT) :: row_starts(0:)
    INTEGER :: r, c, n, row_id

    row_id = 0
    DO r = 0, 8
        DO c = 0, 8
            IF (puzzle(r,c) /= 0) THEN
                ! Cell has a clue - create only one row for that value
                CALL build_dlx_row(nodes, columns, node_count, r, c, puzzle(r,c), &
                                  row_id, row_info, row_starts)
                row_id = row_id + 1
            ELSE
                ! Cell is empty - create rows for all possible values
                DO n = 1, 9
                    CALL build_dlx_row(nodes, columns, node_count, r, c, n, &
                                      row_id, row_info, row_starts)
                    row_id = row_id + 1
                END DO
            END IF
        END DO
    END DO
END SUBROUTINE build_dlx_matrix

! Cover a column in the DLX matrix
SUBROUTINE dlx_cover_column(nodes, columns, col_idx)
    USE DLX_TYPES
    TYPE(DlxNode), INTENT(INOUT) :: nodes(0:)
    TYPE(DlxColumn), INTENT(INOUT) :: columns(0:)
    INTEGER, INTENT(IN) :: col_idx
    INTEGER :: col_node_idx, row_node, right_node

    col_node_idx = columns(col_idx)%node_idx

    ! Remove column header from the header list
    nodes(nodes(col_node_idx)%right)%left = nodes(col_node_idx)%left
    nodes(nodes(col_node_idx)%left)%right = nodes(col_node_idx)%right

    ! For each row in this column
    row_node = nodes(col_node_idx)%down
    DO WHILE (row_node /= col_node_idx)
        ! For each node in this row (excluding the column itself)
        right_node = nodes(row_node)%right
        DO WHILE (right_node /= row_node)
            ! Remove this node from its column
            nodes(nodes(right_node)%down)%up = nodes(right_node)%up
            nodes(nodes(right_node)%up)%down = nodes(right_node)%down
            columns(nodes(right_node)%column)%size = columns(nodes(right_node)%column)%size - 1
            right_node = nodes(right_node)%right
        END DO
        row_node = nodes(row_node)%down
    END DO
END SUBROUTINE dlx_cover_column

! Uncover a column (exact reverse of cover)
SUBROUTINE dlx_uncover_column(nodes, columns, col_idx)
    USE DLX_TYPES
    TYPE(DlxNode), INTENT(INOUT) :: nodes(0:)
    TYPE(DlxColumn), INTENT(INOUT) :: columns(0:)
    INTEGER, INTENT(IN) :: col_idx
    INTEGER :: col_node_idx, row_node, left_node

    col_node_idx = columns(col_idx)%node_idx

    ! For each row in this column (in reverse order)
    row_node = nodes(col_node_idx)%up
    DO WHILE (row_node /= col_node_idx)
        ! For each node in this row (in reverse order)
        left_node = nodes(row_node)%left
        DO WHILE (left_node /= row_node)
            ! Restore this node to its column
            columns(nodes(left_node)%column)%size = columns(nodes(left_node)%column)%size + 1
            nodes(nodes(left_node)%down)%up = left_node
            nodes(nodes(left_node)%up)%down = left_node
            left_node = nodes(left_node)%left
        END DO
        row_node = nodes(row_node)%up
    END DO

    ! Restore column header to the header list
    nodes(nodes(col_node_idx)%right)%left = col_node_idx
    nodes(nodes(col_node_idx)%left)%right = col_node_idx
END SUBROUTINE dlx_uncover_column

! Choose column with minimum size (Knuth's S heuristic)
INTEGER FUNCTION choose_column(nodes, columns, root_idx)
    USE DLX_TYPES
    TYPE(DlxNode), INTENT(IN) :: nodes(0:)
    TYPE(DlxColumn), INTENT(IN) :: columns(0:)
    INTEGER, INTENT(IN) :: root_idx
    INTEGER :: col_node, best_col, min_size, col_idx

    best_col = 0
    min_size = 999999

    col_node = nodes(root_idx)%right
    DO WHILE (col_node /= root_idx)
        col_idx = nodes(col_node)%column
        IF (columns(col_idx)%size < min_size) THEN
            min_size = columns(col_idx)%size
            best_col = col_idx
        END IF
        col_node = nodes(col_node)%right
    END DO

    choose_column = best_col
END FUNCTION choose_column

! DLX Search - Algorithm X with Dancing Links
RECURSIVE SUBROUTINE dlx_search(nodes, columns, root_idx, k, solution, &
                                solution_count, result, iterations)
    USE DLX_TYPES
    TYPE(DlxNode), INTENT(INOUT) :: nodes(0:)
    TYPE(DlxColumn), INTENT(INOUT) :: columns(0:)
    INTEGER, INTENT(IN) :: root_idx, k
    INTEGER, INTENT(INOUT) :: solution(0:)
    INTEGER, INTENT(INOUT) :: solution_count
    INTEGER, INTENT(OUT) :: result
    INTEGER, INTENT(INOUT) :: iterations
    INTEGER :: col_idx, col_node_idx, row_node, right_node, left_node

    ! Count every search call
    iterations = iterations + 1

    ! If matrix is empty, we found a solution
    IF (nodes(root_idx)%right == root_idx) THEN
        solution_count = k
        result = 1
        RETURN
    END IF

    ! Choose column with minimum size
    col_idx = choose_column(nodes, columns, root_idx)

    ! If column has no rows, no solution possible
    IF (columns(col_idx)%size == 0) THEN
        result = 0
        RETURN
    END IF

    ! Cover this column
    CALL dlx_cover_column(nodes, columns, col_idx)

    ! Try each row in this column
    col_node_idx = columns(col_idx)%node_idx
    row_node = nodes(col_node_idx)%down
    DO WHILE (row_node /= col_node_idx)
        ! Add row to partial solution
        solution(k) = nodes(row_node)%row_id

        ! Cover all other columns in this row
        right_node = nodes(row_node)%right
        DO WHILE (right_node /= row_node)
            CALL dlx_cover_column(nodes, columns, nodes(right_node)%column)
            right_node = nodes(right_node)%right
        END DO

        ! Recurse
        CALL dlx_search(nodes, columns, root_idx, k + 1, solution, &
                       solution_count, result, iterations)

        IF (result == 1) THEN
            RETURN  ! Solution found
        END IF

        ! Backtrack: uncover all columns in this row
        left_node = nodes(row_node)%left
        DO WHILE (left_node /= row_node)
            CALL dlx_uncover_column(nodes, columns, nodes(left_node)%column)
            left_node = nodes(left_node)%left
        END DO

        row_node = nodes(row_node)%down
    END DO

    ! Uncover column
    CALL dlx_uncover_column(nodes, columns, col_idx)

    result = 0  ! No solution found
END SUBROUTINE dlx_search

! Cover given clues (pre-selected rows)
SUBROUTINE cover_clues(nodes, columns, puzzle, row_info, row_starts)
    USE DLX_TYPES
    TYPE(DlxNode), INTENT(INOUT) :: nodes(0:)
    TYPE(DlxColumn), INTENT(INOUT) :: columns(0:)
    INTEGER, DIMENSION(0:8,0:8), INTENT(IN) :: puzzle
    INTEGER, INTENT(IN) :: row_info(0:,0:)
    INTEGER, INTENT(IN) :: row_starts(0:)
    INTEGER :: r, c, n, row_id, node_idx, curr

    DO r = 0, 8
        DO c = 0, 8
            IF (puzzle(r,c) /= 0) THEN
                n = puzzle(r,c)

                ! Find the row for this clue
                DO row_id = 0, 728
                    IF (row_starts(row_id) /= 0 .AND. &
                        row_info(row_id, 0) == r .AND. &
                        row_info(row_id, 1) == c .AND. &
                        row_info(row_id, 2) == n) THEN

                        ! Cover all columns in this row
                        node_idx = row_starts(row_id)
                        curr = node_idx
                        DO
                            CALL dlx_cover_column(nodes, columns, nodes(curr)%column)
                            curr = nodes(curr)%right
                            IF (curr == node_idx) EXIT
                        END DO
                        EXIT
                    END IF
                END DO
            END IF
        END DO
    END DO
END SUBROUTINE cover_clues

! Extract solution from DLX and populate solution_grid
SUBROUTINE extract_solution(puzzle, solution_grid, solution_rows, solution_count, row_info)
    INTEGER, DIMENSION(0:8,0:8), INTENT(IN) :: puzzle
    INTEGER, DIMENSION(0:8,0:8), INTENT(OUT) :: solution_grid
    INTEGER, INTENT(IN) :: solution_rows(0:)
    INTEGER, INTENT(IN) :: solution_count
    INTEGER, INTENT(IN) :: row_info(0:,0:)
    INTEGER :: i, row_id, r, c, n

    ! Start with the original puzzle (includes clues)
    solution_grid = puzzle

    ! Each solution entry is a row_id
    DO i = 0, solution_count - 1
        row_id = solution_rows(i)
        IF (row_id >= 0 .AND. row_id <= 728) THEN
            r = row_info(row_id, 0)
            c = row_info(row_id, 1)
            n = row_info(row_id, 2)
            solution_grid(r, c) = n
        END IF
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

END PROGRAM DLX_SUDOKU
