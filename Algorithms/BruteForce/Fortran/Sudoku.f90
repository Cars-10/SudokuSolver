PROGRAM MAIN  
    INTEGER :: num_args, ix, count, r=0 ,DEBUG=0
    INTEGER, DIMENSION(0:8,0:8) :: puzzle
    CHARACTER(len=50), DIMENSION(:), ALLOCATABLE :: args
    INTEGER, EXTERNAL :: solve
    real :: start, finish
    call cpu_time(start)
  

    num_args = command_argument_count()
    ALLOCATE(args(num_args))  ! I've omitted checking the return status of the allocation 

    DO ix = 1, num_args
        CALL get_command_argument(ix,args(ix))
        ! now parse the argument as you wish
        IF (INDEX(args(ix),".matrix",BACK=.TRUE.) > 0) THEN
            CALL readMatrixFile(args(ix))
            CALL printPuzzle(puzzle)
            count = 0
            r = solve(puzzle, count)
        END IF
    END DO
    call cpu_time(finish)
    print '(/A,F8.3)',"Seconds to process ", finish-start
CONTAINS    

SUBROUTINE readMatrixFile(filename)
    CHARACTER(len=50) :: filename, line
    INTEGER :: n, ios, c
    PRINT '(A)',  filename
    OPEN(UNIT=9,FILE=filename, iostat=ios)
    if ( ios /= 0 ) stop "Error opening file"
    n = 0
    DO
        read(9, '(A)', iostat=ios) line
        if (ios /= 0) exit
        if (len_trim(line) == 0) cycle
        if (line(1:1) == '#') cycle

        read (line, *) puzzle(n,0),puzzle(n,1),puzzle(n,2),puzzle(n,3),puzzle(n,4)&
            ,puzzle(n,5),puzzle(n,6),puzzle(n,7),puzzle(n,8)
        ! Print row as we read it (matches C format)
        do c = 0, 8
            write(*, '(I1,A)', advance='no') puzzle(n,c), ' '
        end do
        write(*,*)
        n = n + 1
    END DO
    CLOSE(UNIT=9)
END SUBROUTINE readMatrixFile
END PROGRAM MAIN

SUBROUTINE printPuzzle(puzzle)
    INTEGER :: n, c
    INTEGER, DIMENSION(0:8,0:8), INTENT(INOUT) :: puzzle
    print '(/a)','Puzzle:'
    DO n=0,8
        do c = 0, 8
            write(*, '(I1,A)', advance='no') puzzle(n,c), ' '
        end do
        write(*,*)
    END DO
END SUBROUTINE printPuzzle


RECURSIVE FUNCTION solve(puzzle, count)  RESULT(r)
    INTEGER :: j,i,val,r, DEBUG=0
    INTEGER, INTENT(INOUT) :: count
    INTEGER, DIMENSION(0:8,0:8), INTENT(INOUT) :: puzzle
    CHARACTER(len=10) :: str_count

    DO j=0,8
        DO i=0,8
            IF (puzzle(j,i) == 0) THEN
            IF (DEBUG>0) print '(A,i1,A,i1,A,i1)','Solve: j=',j,',i=',i,': ',puzzle(j,i)
                DO val=1,9
                    count = count + 1
                    write(str_count,'(i8)') count
                    IF (DEBUG>0) print '(2a)','Count= ',adjustl(str_count)
                    IF (isPossible(j,i,val) == 1) THEN
                        puzzle(j,i) = val
                        IF (solve(puzzle,count) == 2) THEN
                            r = 2
                            !//Makes sure to do a quick exit when solution was found
                            RETURN
                        END IF
                        puzzle(j,i) = 0
                    END IF
                END DO
                r = 0
                RETURN
            END IF 
        END DO
    END DO
    CALL printPuzzle(puzzle)
    write(str_count,'(i10)') count
    print '(/2a)', 'Solved in Iterations=', adjustl(str_count)
    r = 2
    RETURN 

    CONTAINS
    FUNCTION isPossible(y,x,pval) 
        INTEGER :: y, x, x0, y0
        INTEGER, INTENT(IN) :: pval 
        if (DEBUG>0) print '(A,i2,A,i2,A,i2)', 'Is possible ', y,',', x,',',pval
        ! Find if a matching number (pval) already exists
        ! in the same row (y) or column (x) or within its rectangle
        DO ii=0,8
            if (DEBUG>2) print *,ii,x,puzzle(ii,x),pval
            IF (puzzle(ii,x) == pval) THEN
                isPossible = 0
                RETURN
            END IF
        END DO
        DO ii=0,8
            if (DEBUG>2) print *,y,ii,puzzle(y,ii),pval
            IF (puzzle(y,ii) == pval) THEN
                isPossible = 0
                RETURN
            END IF
        END DO
        
        ! Search the Rectangle containing x & y
        ! Find which 3x3 square we are in using the floor quotient
        x0 = floor(x/3.0)*3
        y0 = floor(y/3.0)*3
    
        DO ii=0,2
            DO jj=0,2
                IF (DEBUG>1) print '(A,i1,A,i1,A,i1,A,i1,A,i1,A,i1)', 'Is Possible: y0+i=', &
                y0+ii,',i=',ii,', x0+j=',x0+jj,',j=',jj,' Puzzle[y0+i][x0+j]=',puzzle(y0+ii,x0+jj),' val=', pval
                if(puzzle(y0+ii,x0+jj) == pval ) THEN
                    isPossible = 0
                    RETURN
                END IF
            END DO 
        END DO 
        IF (DEBUG>0) print '(A)','YES possible '
        isPossible = 1
        RETURN
    END FUNCTION isPossible
END FUNCTION solve