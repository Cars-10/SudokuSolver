USING: accessors calendar command-line formatting io io.encodings.utf8 io.files kernel locals math math.parser math.ranges namespaces prettyprint sequences splitting system tools.time vectors ;
IN: sudoku.dlx

TUPLE: node { left initial: f } { right initial: f } { up initial: f } { down initial: f } col row-idx ;
TUPLE: column < node { size integer initial: 0 } { id integer } ;

SYMBOL: head
SYMBOL: columns
SYMBOL: solution
SYMBOL: iterations
SYMBOL: grid

:: init-columns ( -- )
    column new :> h
    h h >>left h >>right h >>up h >>down drop
    h head set
    
    324 iota [
        :> i
        column new i >>id :> c
        c c >>up c >>down drop
        c c >>left c >>right drop ! temp
        
        c head get >>col drop
        c 0 >>size drop
        
        ! Link into header list (at end)
        head get :> h
        h left>> :> last
        
        c h >>right drop
        c last >>left drop
        c last right<<
        h c left<<
        
        c
    ] map columns set ;

:: add-node ( c-idx r-idx -- node )
    columns get c-idx nth :> c
    node new c >>col r-idx >>row-idx :> n
    
    ! Link vertical (at bottom)
    c up>> :> last
    n c >>down drop
    n last >>up drop
    last n down<<
    c n up<<
    
    c size>> 1 + c size<<
    n ;

:: link-row ( nodes -- )
    nodes length :> len
    len 0 > [
        nodes first :> first-node
        len iota [
            :> i
            nodes i nth :> n
            nodes i 1 - len + len mod nth n left<<
            nodes i 1 + len mod nth n right<<
        ] each
    ] when ;

:: cover ( c -- )
    c right>> left<<
    c left>> right<<
    
    c down>> :> i!
    [ i c eq? not ] [
        i right>> :> j!
        [ j i eq? not ] [
            j down>> up<<
            j up>> down<<
            j col>> :> col
            col size>> 1 - col size<<
            j right>> j!
        ] while
        i down>> i!
    ] while ;

:: uncover ( c -- )
    c up>> :> i!
    [ i c eq? not ] [
        i left>> :> j!
        [ j i eq? not ] [
            j col>> :> col
            col size>> 1 + col size<<
            j down>> up<<
            j up>> down<<
            j left>> j!
        ] while
        i up>> i!
    ] while
    c right>> left<<
    c left>> right<< ;

DEFER: search

:: search ( k -- ? )
    head get right>> head get eq? [ t ] [
        ! Choose col
        head get right>> :> c!
        c size>> :> min-size!
        
        c right>> :> curr!
        [ curr head get eq? not ] [
            curr size>> min-size < [
                curr size>> min-size!
                curr c!
            ] when
            curr right>> curr!
        ] while
        
        c cover
        
        c down>> :> r!
        f :> found!
        [ r c eq? not found not and ] [
            solution get r push
            iterations [ 1 + ] change
            
            r right>> :> j!
            [ j r eq? not ] [
                j col>> cover
                j right>> j!
            ] while
            
            k 1 + search [ t found! ] [
                ! Backtrack
                r left>> :> jj!
                [ jj r eq? not ] [
                    jj col>> uncover
                    jj left>> jj!
                ] while
                solution get pop drop
                f
            ] if
            
            r down>> r!
        ] while
        
        c uncover
        found
    ] if ;

:: build-matrix ( input-grid -- )
    init-columns
    
    9 iota [
        :> r
        9 iota [
            :> c
            input-grid r nth c nth :> val
            
            val 0 = [ 1 9 [a,b] ] [ val val [a,b] ] if :> vals
            
            vals [
                :> v
                r 81 * c 9 * + v 1 - + :> r-idx
                
                ! Constraints
                r 9 * c + :> c1
                81 r 9 * + v 1 - + :> c2
                162 c 9 * + v 1 - + :> c3
                r 3 /i 3 * c 3 /i + :> b
                243 b 9 * + v 1 - + :> c4
                
                c1 r-idx add-node
                c2 r-idx add-node
                c3 r-idx add-node
                c4 r-idx add-node
                
                4array link-row
            ] each
        ] each
    ] each ;

:: print-solution ( -- )
    9 iota [ V{ 0 0 0 0 0 0 0 0 0 } clone ] map :> out-grid
    
    solution get [
        :> node
        node row-idx>> :> idx
        idx 9 mod 1 + :> v
        idx 9 /i :> tmp
        tmp 9 mod :> c
        tmp 9 /i :> r
        out-grid r nth c v set-nth
    ] each
    
    nl "Puzzle:" print
    out-grid [ [ number>string write " " write ] each nl ] each ;

:: run-matrix ( filename -- )
    filename "/app/Matrices/" head? [
        "../" write filename 14 tail print
    ] [
        filename print
    ] if
    
    filename utf8 file-lines
    V{ } clone :> g
    
    [
        "# " split first strip :> line
        line empty? not [
             line " " split [ empty? not ] filter [ string>number ] map :> parts
             parts length 9 = [
                 g parts >vector push
                 ! Echo
                 parts [ number>string write " " write ] each nl
             ] when
        ] when
    ] each
    
    g build-matrix
    
    V{ } clone solution set
    0 iterations set
    0 search [
        print-solution
        nl "Solved in Iterations=" write iterations get .
    ] [
        nl "No solution found" print
    ] if ;

: main ( -- )
    now :> start-time
    command-line get [
        dup ".matrix" tail? [ run-matrix ] [ drop ] if
    ] each
    now start-time time- duration>seconds :> sec
    "Seconds to process " write sec 1.0 1000.0 / + . ;

MAIN: main
