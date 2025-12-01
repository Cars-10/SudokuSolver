Object subclass: Sudoku [
    | board iterations |

    Sudoku class >> new [
        ^super new init
    ]

    init [
        board := Array new: 9.
        1 to: 9 do: [:i | board at: i put: (Array new: 9)].
        iterations := 0.
    ]

    readMatrix: filename [
        | file stream line row col char |
        file := FileStream open: filename mode: FileStream read.
        row := 1.
        [file atEnd] whileFalse: [
            line := file nextLine.
            (line size > 0) ifTrue: [
                (line at: 1) ~= $# ifTrue: [
                    col := 1.
                    stream := ReadStream on: line.
                    [stream atEnd] whileFalse: [
                        char := stream next.
                        (char isDigit and: [char ~= $0]) ifTrue: [
                            (board at: row) at: col put: (char digitValue).
                            col := col + 1.
                        ] ifFalse: [
                            (char = $0 or: [char = $.]) ifTrue: [
                                (board at: row) at: col put: 0.
                                col := col + 1.
                            ]
                        ]
                    ].
                    row := row + 1.
                ]
            ]
        ].
        file close.
    ]

    printBoard [
        1 to: 9 do: [:r |
            1 to: 9 do: [:c |
                Transcript show: ((board at: r) at: c) printString; show: ' '.
            ].
            Transcript nl.
        ].
    ]

    isValid: row col: col num: num [
        | boxRow boxCol |
        
        "Row check"
        1 to: 9 do: [:c |
            ((board at: row) at: c) = num ifTrue: [^false].
        ].

        "Col check"
        1 to: 9 do: [:r |
            ((board at: r) at: col) = num ifTrue: [^false].
        ].

        "Box check"
        boxRow := ((row - 1) // 3) * 3 + 1.
        boxCol := ((col - 1) // 3) * 3 + 1.
        0 to: 2 do: [:r |
            0 to: 2 do: [:c |
                ((board at: boxRow + r) at: boxCol + c) = num ifTrue: [^false].
            ].
        ].

        ^true
    ]

    findEmpty [
        1 to: 9 do: [:r |
            1 to: 9 do: [:c |
                ((board at: r) at: c) = 0 ifTrue: [^{r. c}].
            ].
        ].
        ^nil
    ]

    solve [
        | empty row col |
        empty := self findEmpty.
        empty ifNil: [^true].
        
        row := empty at: 1.
        col := empty at: 2.

        1 to: 9 do: [:num |
            iterations := iterations + 1.
            (self isValid: row col: col num: num) ifTrue: [
                (board at: row) at: col put: num.
                self solve ifTrue: [^true].
                (board at: row) at: col put: 0.
            ].
        ].

        ^false
    ]

    getIterations [
        ^iterations
    ]
]

| sudoku filename args |
args := Smalltalk arguments.
(args size < 1) ifTrue: [
    Transcript show: 'Usage: gst sudoku.st -a filename'; nl.
    ObjectMemory quit.
].

filename := args at: 1.
sudoku := Sudoku new.
sudoku readMatrix: filename.

Transcript show: 'Puzzle:'; nl.
sudoku printBoard.

(sudoku solve) ifTrue: [
    Transcript show: 'Puzzle:'; nl.
    sudoku printBoard.
    Transcript show: 'Solved in Iterations='; show: sudoku getIterations printString; nl.
] ifFalse: [
    Transcript show: 'No solution found.'; nl.
].
