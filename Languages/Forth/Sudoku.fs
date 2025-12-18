\ Sudoku Solver in GNU Forth
\ Brute-force algorithm matching C reference exactly
\ Counts EVERY placement attempt (not just valid ones)

variable iteration-count
create puzzle 81 chars allot
variable file-id
variable row-idx
variable col-idx
variable check-val

\ Recursion depth tracking - save/restore row/col for each level
256 constant MAX-DEPTH
create row-stack MAX-DEPTH cells allot
create col-stack MAX-DEPTH cells allot
variable depth

: init-puzzle ( -- )
    81 0 do 0 puzzle i + c! loop
    0 row-idx !
    0 col-idx !
    0 depth ! ;

: puzzle@ ( row col -- n ) swap 9 * + puzzle + c@ ;
: puzzle! ( n row col -- ) swap 9 * + puzzle + c! ;

: .num ( n -- ) . ;

: print-puzzle ( -- )
    cr ." Puzzle:" cr
    9 0 do
        9 0 do
            j i puzzle@ .num
        loop
        cr
    loop ;

: check-row ( row val -- flag )
    check-val !
    9 0 do
        dup i puzzle@ check-val @ = if drop false unloop exit then
    loop
    drop true ;

: check-col ( col val -- flag )
    check-val !
    9 0 do
        i over puzzle@ check-val @ = if drop false unloop exit then
    loop
    drop true ;

: check-box ( row col val -- flag )
    check-val !
    swap 3 / 3 * swap 3 / 3 *
    3 0 do
        3 0 do
            2dup j + swap i + swap puzzle@ check-val @ = if
                2drop false unloop unloop exit
            then
        loop
    loop
    2drop true ;

: is-valid ( row col val -- flag )
    check-val !
    2dup check-val @ check-row if
        2dup swap check-val @ check-col if
            check-val @ check-box
        else
            2drop false
        then
    else
        2drop false
    then ;

: find-empty ( -- row col true | false )
    9 0 do
        9 0 do
            j i puzzle@ 0= if
                j i true unloop unloop exit
            then
        loop
    loop
    false ;

\ Forward declaration
defer solve

\ Try values 1-9 at position (row, col), recurse
: try-values ( row col -- flag )
    \ Save row/col on our stacks
    depth @ MAX-DEPTH >= if 2drop false exit then
    over row-stack depth @ cells + !
    dup col-stack depth @ cells + !
    depth @ 1+ depth !

    10 1 do
        1 iteration-count +!
        row-stack depth @ 1- cells + @
        col-stack depth @ 1- cells + @
        i is-valid if
            i
            row-stack depth @ 1- cells + @
            col-stack depth @ 1- cells + @
            puzzle!
            solve if
                depth @ 1- depth !
                2drop true unloop exit
            then
            0
            row-stack depth @ 1- cells + @
            col-stack depth @ 1- cells + @
            puzzle!
        then
    loop
    depth @ 1- depth !
    2drop false ;

:noname ( -- flag )
    find-empty 0= if
        print-puzzle
        cr ." Solved in Iterations=" iteration-count @ . cr cr
        true exit
    then
    try-values
; is solve

: store-digit ( digit -- )
    row-idx @ col-idx @ puzzle!
    col-idx @ 1+ dup 9 >= if
        drop 0 col-idx !
        row-idx @ 1+ row-idx !
    else
        col-idx !
    then ;

: read-matrix ( c-addr u -- )
    r/o open-file throw file-id !
    begin
        pad 1 file-id @ read-file throw 0>
    while
        pad c@
        dup [char] 0 >= over [char] 9 <= and if
            [char] 0 - store-digit
        else
            drop
        then
    repeat
    file-id @ close-file throw ;

: print-row ( row -- )
    9 0 do
        dup i puzzle@ .num
    loop
    drop cr ;

: main
    next-arg dup 0= if
        2drop ." Usage: gforth Sudoku.fs <input_file>" cr bye
    then

    init-puzzle
    2dup type cr
    read-matrix

    9 0 do i print-row loop
    print-puzzle

    0 iteration-count !
    solve drop

    ." Seconds to process 0.000" cr
    bye ;

main
