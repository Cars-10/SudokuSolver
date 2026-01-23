USING: accessors calendar command-line formatting io io.encodings.utf8 io.files kernel locals math math.bitwise math.parser namespaces prettyprint sequences splitting system tools.time vectors ;
IN: sudoku.cp

SYMBOL: grid-values
SYMBOL: grid-candidates
SYMBOL: iterations
SYMBOL: peers-cache

:: init-peers ( -- )
    9 iota [
        :> r
        9 iota [
            :> c
            V{ } clone :> p
            ! Row
            9 iota [ :> cc cc c = not [ r cc 2array p push ] when ] each
            ! Col
            9 iota [ :> rr rr r = not [ rr c 2array p push ] when ] each
            ! Box
            r 3 /i 3 * :> br
            c 3 /i 3 * :> bc
            3 iota [ br + ] map [
                :> rr
                3 iota [ bc + ] map [
                    :> cc
                    rr r = cc c = and not [ rr cc 2array p push ] when
                ] each
            ] each
            p
        ] map
    ] map peers-cache set ;

:: get-peers ( r c -- seq )
    peers-cache get r nth c nth ;

: count-candidates ( mask -- n ) bit-count ;
:: get-first-candidate ( mask -- n )
    1 9 [a,b] [ mask swap 1 swap shift bitand 0 > ] find drop ;

DEFER: eliminate

:: assign ( r c val -- ? )
    iterations [ 1 + ] change
    grid-values get r nth c val set-nth
    grid-candidates get r nth c 1 val shift set-nth
    
    get-peers call( r c -- seq ) :> peers
    peers [
        first2 :> ( pr pc )
        pr pc val eliminate
    ] all? ;

:: eliminate ( r c val -- ? )
    grid-candidates get r nth c nth :> mask
    mask 1 val shift bitand 0 = [ t ] [
        mask 1 val shift bitnot bitand :> new-mask
        grid-candidates get r nth c new-mask set-nth
        
        new-mask bit-count :> cnt
        cnt 0 = [ f ] [
            cnt 1 = grid-values get r nth c nth 0 = and [
                new-mask get-first-candidate :> val2
                r c val2 assign
            ] [ t ] if
        ] if
    ] if ;

:: save-state ( -- values candidates )
    grid-values get [ clone ] map
    grid-candidates get [ clone ] map ;

:: restore-state ( values candidates -- )
    values grid-values set
    candidates grid-candidates set ;

:: find-mrv ( -- r c )
    -1 -1 :> ( min-r! min-c! )
    10 :> min-cnt!
    
    9 iota [
        :> r
        9 iota [
            :> c
            grid-values get r nth c nth 0 = [
                grid-candidates get r nth c nth bit-count :> cnt
                cnt min-cnt < [
                    cnt min-cnt!
                    r min-r!
                    c min-c!
                ] when
            ] when
        ] each
    ] each
    min-r min-c ;

:: solve ( -- ? )
    find-mrv :> ( r c )
    r -1 = [ t ] [
        grid-candidates get r nth c nth :> mask
        1 9 [a,b] [
            :> val
            mask 1 val shift bitand 0 > [
                save-state :> ( saved-v saved-c )
                r c val assign [
                    solve [ t ] [
                        saved-v saved-c restore-state
                        f
                    ] if
                ] [
                    saved-v saved-c restore-state
                    f
                ] if
            ] [ f ] if
        ] any?
    ] if ;

:: print-puzzle ( -- )
    nl "Puzzle:" print
    grid-values get [ [ number>string write " " write ] each nl ] each ;

:: run-matrix ( filename -- )
    filename "/app/Matrices/" head? [
        "../" write filename 14 tail print
    ] [
        filename print
    ] if
    
    filename utf8 file-lines
    V{ } clone :> g
    
    g grid-values set
    
    [
        "# " split first strip :> line ! Simple comment strip
        line empty? not [
             line " " split [ empty? not ] filter [ string>number ] map :> parts
             parts length 9 = [
                 g parts >vector push
                 ! Echo
                 parts [ number>string write " " write ] each nl
             ] when
        ] when
    ] each
    
    ! Init candidates
    9 iota [ V{ 1022 1022 1022 1022 1022 1022 1022 1022 1022 } clone ] map grid-candidates set
    
    init-peers
    print-puzzle
    0 iterations set
    
    ! Initial propagation
    t :> valid!
    9 iota [
        :> r
        9 iota [
            :> c
            grid-values get r nth c nth :> v
            v 0 > [
                 r c v assign valid and valid!
            ] when
        ] each
    ] each
    
    valid [
        solve [
            print-puzzle
            nl "Solved in Iterations=" write iterations get .
        ] [
            nl "No solution found" print
        ] if
    ] [
        nl "No solution found (Initial invalid)" print
    ] if ;

: main ( -- )
    now :> start-time
    command-line get [
        dup ".matrix" tail? [ run-matrix ] [ drop ] if
    ] each
    now start-time time- duration>seconds :> sec
    "Seconds to process " write sec 1.0 1000.0 / + . ;

MAIN: main
