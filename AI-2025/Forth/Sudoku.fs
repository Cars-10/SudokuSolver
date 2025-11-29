\ Forth Sudoku Solver

variable count
create puzzle 81 cells allot

: puzzle@ ( r c -- val )
    9 * + cells puzzle + @ ;

: puzzle! ( val r c -- )
    9 * + cells puzzle + ! ;

: print-puzzle
    cr ." Puzzle:" cr
    9 0 do
        9 0 do
            j i puzzle@ .
        loop
        cr
    loop ;

: read-matrix-file ( addr u -- )
    2dup type cr
    r/o open-file throw >r
    0 ( row )
    begin
        pad 1024 r@ read-line throw
    while
        ( row addr u )
        drop
        pad swap
        \ Parse line
        0 ( col )
        begin
            32 $split
            dup 0>
        while
            ( row col addr u )
            2swap 2dup s" #" compare 0= if
                2drop 2drop 0 ( break inner loop )
            else
                s>number? if
                    drop
                    ( row col val )
                    rot rot 2dup 2rot puzzle!
                    1+ ( next col )
                else
                    2drop
                then
            then
        repeat
        2drop drop
        1+ ( next row )
        dup 9 = if drop r> close-file throw exit then
    repeat
    drop drop r> close-file throw ;

: is-possible ( r c val -- f )
    >r
    \ Check row
    9 0 do
        j i puzzle@ r@ = if r> drop unloop false exit then
    loop
    \ Check col
    9 0 do
        i j puzzle@ r@ = if r> drop unloop false exit then
    loop
    \ Check box
    j 3 / 3 * i 3 / 3 *
    3 0 do
        3 0 do
            j k + i l + puzzle@ r@ = if r> drop unloop unloop false exit then
        loop
    loop
    r> drop true ;

: solve ( -- f )
    9 0 do
        9 0 do
            j i puzzle@ 0= if
                10 1 do
                    1 count +!
                    j i k is-possible if
                        k j i puzzle!
                        recurse if unloop unloop true exit then
                        0 j i puzzle!
                    then
                loop
                unloop unloop false exit
            then
        loop
    loop
    print-puzzle
    cr ." Solved in Iterations=" count @ . cr cr
    true ;

: process-file ( addr u -- )
    read-matrix-file
    print-puzzle
    0 count !
    solve drop ;

: main
    utime 2>r
    argc @ 1 ?do
        i arg process-file
    loop
    utime 2r> d- d>f 1000000.0 f/
    ." Seconds to process " f. cr
    bye ;

main
