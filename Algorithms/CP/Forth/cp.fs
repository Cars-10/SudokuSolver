\ Constraint Propagation (CP) Sudoku Solver - Forth Implementation
\ Mechanical translation from Python reference to preserve algorithm correctness.
\
\ Algorithm: Constraint propagation with MRV (Minimum Remaining Values) heuristic
\ - Uses bitsets to track candidate values (bits 1-9)
\ - Propagates constraints: singleton elimination, hidden singles
\ - Search with MRV cell selection for efficiency

\ Global iteration counter
variable cp-iterations

\ Grid structures
\ values[row][col] - assigned values (0 = empty)
\ candidates[row][col] - possible values per cell (bitset)
create grid-values 81 cells allot
create grid-candidates 81 cells allot

\ Solution array
create solution 81 cells allot

\ Input puzzle
create puzzle 81 cells allot

\ Helper to access 2D arrays as 1D
: grid-index ( row col -- index ) 9 * + ;

: get-value ( row col -- value )
    grid-index cells grid-values + @ ;

: set-value ( value row col -- )
    grid-index cells grid-values + ! ;

: get-candidates ( row col -- candidates )
    grid-index cells grid-candidates + @ ;

: set-candidates ( candidates row col -- )
    grid-index cells grid-candidates + ! ;

: get-puzzle ( row col -- value )
    grid-index cells puzzle + @ ;

: set-puzzle ( value row col -- )
    grid-index cells puzzle + ! ;

\ Bitset operations
: has-candidate ( candidate-set digit -- flag )
    1 swap lshift and 0<> ;

: add-candidate ( candidate-set digit -- new-set )
    1 swap lshift or ;

: remove-candidate ( candidate-set digit -- new-set )
    1 swap lshift invert and ;

: count-candidates ( candidate-set -- count )
    0 swap
    10 1 do
        dup i has-candidate if swap 1+ swap then
    loop drop ;

: get-first-candidate ( candidate-set -- digit )
    10 1 do
        dup i has-candidate if drop i unloop exit then
    loop drop 0 ;

\ Forward declarations
defer eliminate
defer assign
defer propagate

\ Get peers for a cell (row, col)
\ This is complex in Forth - we'll use a different strategy
\ Instead of returning a list, we'll iterate and call a function

\ eliminate-from-peer - eliminate digit from peer cell (using locals)
: eliminate-from-peer { digit peer-row peer-col -- success }
    \ Check if digit is already eliminated
    peer-row peer-col get-candidates digit has-candidate invert if
        true exit
    then

    \ Remove digit from candidates
    peer-row peer-col get-candidates digit remove-candidate
    peer-row peer-col set-candidates

    \ Check for contradiction (no candidates left)
    peer-row peer-col get-candidates count-candidates { remaining }
    remaining 0= if
        false exit
    then

    \ Singleton elimination: if only one candidate left and cell is empty
    remaining 1 = peer-row peer-col get-value 0= and if
        peer-row peer-col get-candidates get-first-candidate { last-digit }
        last-digit peer-row peer-col assign invert if
            false exit
        then
    then

    true ;

\ Assign digit to cell at (row, col) - using locals for clarity
: do-assign { digit row col -- success }
    1 cp-iterations +!  \ Increment iteration counter

    \ Set value
    digit row col set-value

    \ Set candidates to just this digit
    1 digit lshift row col set-candidates

    \ Eliminate from peers - same row
    9 0 do
        i col <> if
            digit row i eliminate-from-peer invert if
                false unloop exit
            then
        then
    loop

    \ Eliminate from peers - same column
    9 0 do
        i row <> if
            digit i col eliminate-from-peer invert if
                false unloop exit
            then
        then
    loop

    \ Eliminate from peers - same box
    row 3 / 3 *  \ box-row-start
    col 3 / 3 *  \ box-col-start
    { box-row-start box-col-start }
    box-row-start 3 + box-row-start do
        box-col-start 3 + box-col-start do
            j row <> i col <> or if
                digit j i eliminate-from-peer invert if
                    false unloop unloop exit
                then
            then
        loop
    loop

    true ;

' do-assign is assign

\ Variables for propagate
variable prop-changed
variable prop-row
variable prop-col
variable prop-digit
variable prop-count
variable prop-last
variable prop-assigned

\ Helper: check hidden single in row
: check-hs-row ( row digit -- success changed )
    prop-digit ! prop-row !
    0 prop-count !
    -1 prop-last !
    false prop-assigned !

    9 0 do
        prop-row @ i get-value prop-digit @ = if
            true prop-assigned !
            leave
        then
        prop-row @ i get-candidates prop-digit @ has-candidate if
            prop-count @ 1+ prop-count !
            i prop-last !
        then
    loop

    prop-assigned @ if
        true false exit
    then

    prop-count @ 1 = if
        prop-digit @ prop-row @ prop-last @ assign invert if
            false false exit
        then
        true true exit
    then

    prop-count @ 0= if
        false false exit
    then

    true false ;

\ Helper: check hidden single in column
: check-hs-col ( col digit -- success changed )
    prop-digit ! prop-col !
    0 prop-count !
    -1 prop-last !
    false prop-assigned !

    9 0 do
        i prop-col @ get-value prop-digit @ = if
            true prop-assigned !
            leave
        then
        i prop-col @ get-candidates prop-digit @ has-candidate if
            prop-count @ 1+ prop-count !
            i prop-last !
        then
    loop

    prop-assigned @ if
        true false exit
    then

    prop-count @ 1 = if
        prop-digit @ prop-last @ prop-col @ assign invert if
            false false exit
        then
        true true exit
    then

    prop-count @ 0= if
        false false exit
    then

    true false ;

\ Variables for box hidden singles
variable box-row-start
variable box-col-start
variable last-br
variable last-bc

\ Helper: check hidden single in box
: check-hs-box ( box digit -- success changed )
    prop-digit ! { box }
    box 3 / 3 * box-row-start !
    box 3 mod 3 * box-col-start !
    0 prop-count !
    -1 last-br !
    -1 last-bc !
    false prop-assigned !

    box-row-start @ 3 + box-row-start @ do
        box-col-start @ 3 + box-col-start @ do
            j i get-value prop-digit @ = if
                true prop-assigned !
            then
            prop-assigned @ invert if
                j i get-candidates prop-digit @ has-candidate if
                    prop-count @ 1+ prop-count !
                    j last-br !
                    i last-bc !
                then
            then
        loop
    loop

    prop-assigned @ if
        true false exit
    then

    prop-count @ 1 = if
        prop-digit @ last-br @ last-bc @ assign invert if
            false false exit
        then
        true true exit
    then

    prop-count @ 0= if
        false false exit
    then

    true false ;

\ Propagate constraints until quiescence
: do-propagate ( -- success )
    begin
        false prop-changed !

        \ Strategy 1: Singleton elimination
        9 0 do
            9 0 do
                j i get-value 0= if
                    j i get-candidates count-candidates
                    dup 0= if drop unloop unloop false exit then
                    1 = if
                        j i get-candidates get-first-candidate j i assign invert if
                            unloop unloop false exit
                        then
                        true prop-changed !
                    then
                then
            loop
        loop

        \ Strategy 2: Hidden singles - rows
        9 0 do  \ row
            10 1 do  \ digit
                j i check-hs-row  ( success changed )
                if true prop-changed ! then
                invert if unloop unloop false exit then
            loop
        loop

        \ Strategy 2: Hidden singles - columns
        9 0 do  \ col
            10 1 do  \ digit
                j i check-hs-col  ( success changed )
                if true prop-changed ! then
                invert if unloop unloop false exit then
            loop
        loop

        \ Strategy 2: Hidden singles - boxes
        9 0 do  \ box
            10 1 do  \ digit
                j i check-hs-box  ( success changed )
                if true prop-changed ! then
                invert if unloop unloop false exit then
            loop
        loop

        prop-changed @ invert
    until
    true ;

' do-propagate is propagate

\ Find MRV cell - using locals for clarity
: find-mrv-cell ( -- row col found )
    10  \ min-candidates
    -1  \ mrv-row
    -1  \ mrv-col
    false  \ found
    { min-cand mrv-row mrv-col found }

    9 0 do
        9 0 do
            j i get-value 0= if
                j i get-candidates count-candidates { num-cand }
                num-cand min-cand < if
                    num-cand to min-cand
                    j to mrv-row
                    i to mrv-col
                    true to found
                then
            then
        loop
    loop

    mrv-row mrv-col found ;

\ State stack for recursive backtracking
\ Max depth is 81 (one per empty cell)
\ Each level stores 81 values and 81 candidates = 162 cells
81 162 * constant STATE-STACK-SIZE
create state-stack STATE-STACK-SIZE cells allot
variable state-depth

: state-level-addr ( level -- addr )
    162 * cells state-stack + ;

: save-grid ( -- )
    state-depth @ state-level-addr { base }
    81 0 do
        i cells grid-values + @
        base i cells + !
        i cells grid-candidates + @
        base i 81 + cells + !
    loop
    1 state-depth +! ;

: restore-grid ( -- )
    -1 state-depth +!
    state-depth @ state-level-addr { base }
    81 0 do
        base i cells + @
        i cells grid-values + !
        base i 81 + cells + @
        i cells grid-candidates + !
    loop ;

\ CP search with recursion - using locals for clarity
: cp-search ( -- success )
    find-mrv-cell { mrv-row mrv-col found }

    found invert if
        \ No empty cells - solved! Extract solution
        9 0 do
            9 0 do
                j i get-value
                j i grid-index cells solution + !
            loop
        loop
        true exit
    then

    \ Get candidates for MRV cell
    mrv-row mrv-col get-candidates { candidates }

    \ Try each candidate digit
    10 1 do
        candidates i has-candidate if
            save-grid
            i mrv-row mrv-col assign if
                propagate if
                    recurse if
                        true unloop exit
                    then
                then
            then
            restore-grid
        then
    loop

    false ;

\ Initialize grid from puzzle
: init-grid ( -- )
    9 0 do
        9 0 do
            j i get-puzzle 0= if
                \ Empty cell: set all candidates 1-9
                0 j i set-value
                0x3FE j i set-candidates  \ bits 1-9
            else
                \ Given clue: set single value
                j i get-puzzle dup j i set-value
                1 swap lshift j i set-candidates
            then
        loop
    loop ;

\ Print puzzle
: .puzzle ( -- )
    cr ." Puzzle:" cr
    9 0 do
        9 0 do
            j i get-value .
        loop cr
    loop ;

: .solution ( -- )
    cr ." Puzzle:" cr
    9 0 do
        9 0 do
            j i grid-index cells solution + @ .
        loop cr
    loop ;

: .puzzle-input ( -- )
    9 0 do
        9 0 do
            j i get-puzzle .
        loop cr
    loop ;

\ Read matrix file
: read-matrix { c-addr u -- }
    c-addr u r/o open-file throw { fd }
    0 0 { r c }
    begin
        pad 1 fd read-file throw 0>
    while
        pad c@
        dup [char] 0 >= over [char] 9 <= and if
            [char] 0 - r c set-puzzle
            c 1+ to c
            c 9 = if 0 to c r 1+ to r then
        else
            dup [char] . = if
                0 r c set-puzzle
                c 1+ to c
                c 9 = if 0 to c r 1+ to r then
            else
                drop
            then
        then
    repeat
    fd close-file throw ;

\ Main program
: main ( -- )
    0 cp-iterations !
    0 state-depth !

    \ Get filename from command line
    next-arg dup if
        \ Print filename
        2dup type cr

        \ Read puzzle
        2dup read-matrix
        .puzzle-input

        \ Initialize grid
        init-grid
        .puzzle

        \ Initial propagation
        propagate invert if
            cr ." No solution found (contradiction during initial propagation)" cr
            cr ." Solved in Iterations=" cp-iterations @ . cr
            bye
        then

        \ Run search
        cp-search if
            .solution
            cr ." Solved in Iterations=" cp-iterations @ . cr
        else
            cr ." No solution found" cr
        then
    else
        2drop ." Usage: gforth cp.fs <matrix_file>" cr
    then
    bye ;

main
