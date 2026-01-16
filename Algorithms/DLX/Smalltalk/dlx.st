"DLX (Dancing Links) Sudoku Solver - GNU Smalltalk Implementation"

Object subclass: DlxNode [
    | left right up down column size rowId colId |

    DlxNode class >> new [
        ^super new initialize
    ]

    initialize [
        left := self.
        right := self.
        up := self.
        down := self.
        column := self.
        size := 0.
        rowId := -1.
        colId := -1.
    ]

    left [ ^left ]
    left: aNode [ left := aNode ]
    right [ ^right ]
    right: aNode [ right := aNode ]
    up [ ^up ]
    up: aNode [ up := aNode ]
    down [ ^down ]
    down: aNode [ down := aNode ]
    column [ ^column ]
    column: aNode [ column := aNode ]
    size [ ^size ]
    size: aNumber [ size := aNumber ]
    rowId [ ^rowId ]
    rowId: aNumber [ rowId := aNumber ]
    colId [ ^colId ]
    colId: aNumber [ colId := aNumber ]
]

Object subclass: RowInfo [
    | row col num |

    RowInfo class >> new [
        ^super new initialize
    ]

    initialize [
        row := 0. col := 0. num := 0.
    ]

    row [ ^row ]
    row: n [ row := n ]
    col [ ^col ]
    col: n [ col := n ]
    num [ ^num ]
    num: n [ num := n ]
]

Object subclass: DlxSolver [
    | root columns iterations rowInfo rowStarts puzzle |

    DlxSolver class >> new [
        ^super new initialize
    ]

    initialize [
        puzzle := Array new: 9.
        1 to: 9 do: [:i | puzzle at: i put: (Array new: 9 withAll: 0)].
        iterations := 0.
        rowInfo := Array new: 729.
        rowStarts := Array new: 729.
        1 to: 729 do: [:i | rowInfo at: i put: RowInfo new].
        self initMatrix.
    ]

    initMatrix [
        | col |
        root := DlxNode new.
        columns := Array new: 324.
        1 to: 324 do: [:i |
            col := DlxNode new.
            col size: 0.
            col colId: i - 1.
            col left: root left.
            col right: root.
            root left right: col.
            root left: col.
            columns at: i put: col.
        ].
    ]

    addNode: col rowId: rid [
        | node |
        node := DlxNode new.
        node column: col.
        node rowId: rid.
        node down: col.
        node up: col up.
        col up down: node.
        col up: node.
        col size: col size + 1.
        ^node
    ]

    coverColumn: c [
        | rowNode rightNode |
        c right left: c left.
        c left right: c right.
        rowNode := c down.
        [rowNode == c] whileFalse: [
            rightNode := rowNode right.
            [rightNode == rowNode] whileFalse: [
                rightNode down up: rightNode up.
                rightNode up down: rightNode down.
                rightNode column size: rightNode column size - 1.
                rightNode := rightNode right.
            ].
            rowNode := rowNode down.
        ].
    ]

    uncoverColumn: c [
        | rowNode leftNode |
        rowNode := c up.
        [rowNode == c] whileFalse: [
            leftNode := rowNode left.
            [leftNode == rowNode] whileFalse: [
                leftNode column size: leftNode column size + 1.
                leftNode down up: leftNode.
                leftNode up down: leftNode.
                leftNode := leftNode left.
            ].
            rowNode := rowNode up.
        ].
        c right left: c.
        c left right: c.
    ]

    chooseColumn [
        | best minSize colNode |
        best := nil.
        minSize := 999999.
        colNode := root right.
        [colNode == root] whileFalse: [
            colNode size < minSize ifTrue: [
                minSize := colNode size.
                best := colNode.
            ].
            colNode := colNode right.
        ].
        ^best
    ]

    dlxSearch: k solution: solution [
        | col rowNode rightNode leftNode |
        iterations := iterations + 1.
        root right == root ifTrue: [^true].
        col := self chooseColumn.
        (col isNil or: [col size = 0]) ifTrue: [^false].
        self coverColumn: col.
        rowNode := col down.
        [rowNode == col] whileFalse: [
            solution at: k + 1 put: rowNode rowId.
            rightNode := rowNode right.
            [rightNode == rowNode] whileFalse: [
                self coverColumn: rightNode column.
                rightNode := rightNode right.
            ].
            (self dlxSearch: k + 1 solution: solution) ifTrue: [^true].
            leftNode := rowNode left.
            [leftNode == rowNode] whileFalse: [
                self uncoverColumn: leftNode column.
                leftNode := leftNode left.
            ].
            rowNode := rowNode down.
        ].
        self uncoverColumn: col.
        ^false
    ]

    buildDlxRow: r col: c num: n rowId: rid [
        | posCol rowCol colCol boxIdx boxCol n1 n2 n3 n4 info numVal |
        rid >= 729 ifTrue: [^self].
        numVal := n.
        (numVal < 1 or: [numVal > 9]) ifTrue: [^self].
        info := rowInfo at: rid + 1.
        info row: r. info col: c. info num: numVal.
        posCol := (r * 9) + c + 1.
        rowCol := 82 + (r * 9) + (numVal - 1).
        colCol := 163 + (c * 9) + (numVal - 1).
        boxIdx := ((r // 3) * 3) + (c // 3).
        boxCol := 244 + (boxIdx * 9) + (numVal - 1).
        n1 := self addNode: (columns at: posCol) rowId: rid.
        n2 := self addNode: (columns at: rowCol) rowId: rid.
        n3 := self addNode: (columns at: colCol) rowId: rid.
        n4 := self addNode: (columns at: boxCol) rowId: rid.
        n1 right: n2. n2 right: n3. n3 right: n4. n4 right: n1.
        n1 left: n4. n2 left: n1. n3 left: n2. n4 left: n3.
        rowStarts at: rid + 1 put: n1.
    ]

    buildMatrixFromPuzzle [
        | rowId val |
        rowId := 0.
        0 to: 8 do: [:r |
            0 to: 8 do: [:c |
                val := (puzzle at: r + 1) at: c + 1.
                val > 0
                    ifTrue: [
                        self buildDlxRow: r col: c num: val rowId: rowId.
                        rowId := rowId + 1.
                    ]
                    ifFalse: [
                        1 to: 9 do: [:n |
                            self buildDlxRow: r col: c num: n rowId: rowId.
                            rowId := rowId + 1.
                        ].
                    ].
            ].
        ].
    ]

    coverClues [
        | val start curr n |
        0 to: 8 do: [:r |
            0 to: 8 do: [:c |
                val := (puzzle at: r + 1) at: c + 1.
                val > 0 ifTrue: [
                    1 to: 729 do: [:rid |
                        start := rowStarts at: rid.
                        start notNil ifTrue: [
                            ((rowInfo at: rid) row = r and: [
                             (rowInfo at: rid) col = c and: [
                             (rowInfo at: rid) num = val]]) ifTrue: [
                                curr := start.
                                [self coverColumn: curr column.
                                 curr := curr right.
                                 curr == start] whileFalse.
                            ].
                        ].
                    ].
                ].
            ].
        ].
    ]

    extractSolution: solution [
        | rid info |
        1 to: 81 do: [:i |
            rid := solution at: i.
            (rid notNil and: [rid >= 0 and: [rid < 729]]) ifTrue: [
                info := rowInfo at: rid + 1.
                (puzzle at: info row + 1) at: info col + 1 put: info num.
            ].
        ].
    ]

    printPuzzle [
        Transcript cr; nextPutAll: 'Puzzle:'; cr.
        1 to: 9 do: [:row |
            1 to: 9 do: [:col |
                Transcript nextPutAll: ((puzzle at: row) at: col) printString.
                col < 9 ifTrue: [Transcript nextPut: $ ].
            ].
            Transcript nextPutAll: ' '; cr.
        ].
    ]

    readMatrixFile: filename [
        | file line lineCount values displayPath |
        displayPath := filename.
        (filename startsWith: '/app/Matrices/') ifTrue: [
            displayPath := '../', (filename copyFrom: 6 to: filename size).
        ].
        Transcript nextPutAll: displayPath; cr.
        lineCount := 0.
        file := FileStream open: filename mode: FileStream read.
        [file atEnd] whileFalse: [
            line := file nextLine.
            line := line trimSeparators.
            (line isEmpty or: [line first = $#]) ifFalse: [
                lineCount < 9 ifTrue: [
                    values := ((line subStrings: ' 	') select: [:s | s notEmpty]) collect: [:s | s asInteger].
                    values size = 9 ifTrue: [
                        lineCount := lineCount + 1.
                        puzzle at: lineCount put: (Array withAll: values).
                        1 to: 9 do: [:i |
                            Transcript nextPutAll: (values at: i) printString.
                            i < 9 ifTrue: [Transcript nextPut: $ ].
                        ].
                        Transcript nextPutAll: ' '; cr.
                    ].
                ].
            ].
        ].
        file close.
    ]

    run: filename [
        | solution startTime elapsed |
        startTime := Time millisecondClockValue.
        self readMatrixFile: filename.
        self printPuzzle.
        self initMatrix.
        self buildMatrixFromPuzzle.
        self coverClues.
        iterations := 0.
        solution := Array new: 81.
        (self dlxSearch: 0 solution: solution)
            ifTrue: [
                self extractSolution: solution.
                self printPuzzle.
                Transcript cr; nextPutAll: 'Solved in Iterations='; nextPutAll: iterations printString; cr.
            ]
            ifFalse: [Transcript cr; nextPutAll: 'No solution found'; cr].
        elapsed := Time millisecondClockValue - startTime.
        Transcript cr; nextPutAll: 'Seconds to process ';
                   nextPutAll: (elapsed // 1000) printString;
                   nextPut: $.;
                   nextPutAll: (elapsed \\ 1000) printString; cr.
    ]
]

"Main program"
| solver args |
args := Smalltalk arguments.
args do: [:arg |
    (arg endsWith: '.matrix') ifTrue: [
        solver := DlxSolver new.
        solver run: arg.
    ].
].
