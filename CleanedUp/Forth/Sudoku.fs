\ Sudoku Solver in Forth

variable iterations
create board 81 cells allot

: cell-addr ( row col -- addr )
    9 * + board + ;

: cell@ ( row col -- n )
    cell-addr c@ ;

: cell! ( n row col -- )
    cell-addr c! ;

: print-board
    9 0 do
        9 0 do
            j i cell@ .
        loop
        cr
    loop ;

: check-row ( row num -- bool )
    >r \ save num
    true swap \ flag row
    9 0 do
        dup i cell@ r@ = if
            drop false leave
        then
    loop
    drop r> drop ;

: check-col ( col num -- bool )
    >r \ save num
    true swap \ flag col
    9 0 do
        i over cell@ r@ = if
            drop false leave
        then
    loop
    drop r> drop ;

: check-box ( row col num -- bool )
    >r \ save num
    \ Calculate start row/col
    3 / 3 * swap 3 / 3 * swap \ start-row start-col
    
    true -rot \ flag start-row start-col
    3 0 do \ row loop
        3 0 do \ col loop
            2dup \ copy start-row start-col
            j + swap i + swap \ current-row current-col
            cell@ r@ = if
                rot drop false -rot leave
            then
        loop
        \ if flag is false, leave outer loop
        rot dup 0= if -rot leave then rot
    loop
    2drop r> drop ;

: is-valid ( row col num -- bool )
    >r 2dup r@ check-row if
        2dup r@ check-col if
            r@ check-box
        else
            2drop r> drop false
        then
    else
        2drop r> drop false
    then ;

: find-empty ( -- row col true | false )
    9 0 do
        9 0 do
            j i cell@ 0= if
                j i true unloop unloop exit
            then
        loop
    loop
    false ;

recursive solve

: solve ( -- bool )
    find-empty 0= if true exit then
    \ Stack: row col
    
    10 1 do
        2dup i is-valid if
            i 2dup cell! \ place number
            1 iterations +!
            
            solve if
                2drop true unloop exit
            then
            
            0 2dup cell! \ backtrack
        then
    loop
    2drop false ;

\ File I/O and Main
: read-file ( addr u -- )
    r/o open-file throw >r
    
    0 0 \ row col
    begin
        pad 1 r@ read-file throw 0>
    while
        pad c@
        dup 48 >= over 57 <= and if
            48 - \ convert ascii to int
            -rot 2dup >r >r cell! r> r>
            1+ dup 9 = if drop 0 swap 1+ swap then \ next col/row
        else
            drop
        then
    repeat
    2drop
    r> close-file throw ;

: main
    next-arg 2dup 0= if
        2drop ." Usage: gforth Sudoku.fs <input_file>" cr bye
    then
    read-file
    
    solve if
        ." Solved in Iterations= " iterations @ . cr
        print-board
    else
        ." No solution found." cr
    then
    bye ;

main
