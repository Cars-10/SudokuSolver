\ Sudoku Solver in GNU Forth
\ Exact match of C brute-force algorithm

variable iterations
create grid 81 chars allot

: grid-idx ( r c -- addr ) swap 9 * + grid + ;
: grid@ ( r c -- n ) grid-idx c@ ;
: grid! ( n r c -- ) grid-idx c! ;

: print-grid ( -- )
    cr ." Puzzle:" cr
    9 0 do
        9 0 do
            j i grid@ 1 u.r space
        loop
        cr
    loop ;

: is-valid { r c val -- flag }
    \ Check row
    9 0 do
        r i grid@ val = if false unloop exit then
    loop
    \ Check col
    9 0 do
        i c grid@ val = if false unloop exit then
    loop
    \ Check box
    r 3 / 3 *
    c 3 / 3 *
    { br bc }
    3 0 do
        3 0 do
            br i + bc j + grid@ val = if false unloop unloop exit then
        loop
    loop
    true ;

: find-empty ( -- r c found )
    9 0 do
        9 0 do
            j i grid@ 0= if
                j i true unloop unloop exit
            then
        loop
    loop
    -1 -1 false ;

defer solve

: solve-recursive ( -- solved? )
    find-empty { r c found }
    found 0= if
        print-grid
        cr cr ." Solved in Iterations=" iterations @ 1 u.r cr
        true exit
    then
    
    10 1 do
        1 iterations +!
        r c i is-valid if
            i r c grid!
            solve if true unloop exit then
            0 r c grid! \ Backtrack
        then
    loop
    false ;

' solve-recursive is solve

: read-matrix { c-addr u -- }
    c-addr u r/o open-file throw { fd }
    0 0 { r c }
    begin
        pad 1 fd read-file throw 0>
    while
        pad c@
        dup [char] 0 >= over [char] 9 <= and if
            [char] 0 - r c grid!
            c 1+ to c
            c 9 = if 0 to c r 1+ to r then
        else
            drop
        then
    repeat
    fd close-file throw ;

: process-file ( c-addr u -- )
    2dup type cr
    read-matrix
    cr ." Puzzle:" cr
    9 0 do
        9 0 do
            j i grid@ 1 u.r space
        loop
        cr
    loop
    0 iterations !
    solve drop ;

: main
    next-arg dup if
        process-file
    else
        2drop ." Usage: gforth Sudoku.fs <matrix_file>" cr
    then
    bye ;

main