"Sudoku Solver - GNU Smalltalk Implementation
 Brute-force backtracking algorithm matching C reference exactly.

 Algorithm:
 - Row-major search for empty cells (top-to-bottom, left-to-right)
 - Try values 1-9 in ascending order
 - Count EVERY placement attempt (algorithm fingerprint)"

Object subclass: SudokuSolver [
    | puzzle count |

    SudokuSolver class >> new [
        ^super new initialize
    ]

    initialize [
        puzzle := Array new: 9.
        1 to: 9 do: [:i | puzzle at: i put: (Array new: 9 withAll: 0)].
        count := 0.
    ]

    puzzle [ ^puzzle ]
    count [ ^count ]

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

        "Normalize path for output"
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

            "Skip comments and empty lines"
            (line isEmpty or: [line first = $#]) ifFalse: [
                lineCount < 9 ifTrue: [
                    values := ((line subStrings: ' 	')
                        select: [:s | s notEmpty])
                        collect: [:s | s asInteger].
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

    isValid: row col: col val: val [
        | boxRow boxCol |

        "Check row"
        1 to: 9 do: [:i |
            ((puzzle at: row) at: i) = val ifTrue: [^false].
        ].

        "Check column"
        1 to: 9 do: [:i |
            ((puzzle at: i) at: col) = val ifTrue: [^false].
        ].

        "Check 3x3 box"
        boxRow := ((row - 1) // 3) * 3 + 1.
        boxCol := ((col - 1) // 3) * 3 + 1.
        0 to: 2 do: [:i |
            0 to: 2 do: [:j |
                ((puzzle at: boxRow + i) at: boxCol + j) = val ifTrue: [^false].
            ].
        ].

        ^true
    ]

    solve [
        | row col found |

        "Find first empty cell (row-major order)"
        row := -1.
        col := -1.
        found := false.
        1 to: 9 do: [:r |
            found ifFalse: [
                1 to: 9 do: [:c |
                    found ifFalse: [
                        ((puzzle at: r) at: c) = 0 ifTrue: [
                            row := r.
                            col := c.
                            found := true.
                        ].
                    ].
                ].
            ].
        ].

        "If no empty cell found, puzzle is solved"
        row = -1 ifTrue: [
            self printPuzzle.
            Transcript cr; nextPutAll: 'Solved in Iterations='; nextPutAll: count printString; cr.
            ^true
        ].

        "Try values 1-9 in order"
        1 to: 9 do: [:val |
            count := count + 1.  "COUNT EVERY ATTEMPT - algorithm fingerprint"

            (self isValid: row col: col val: val) ifTrue: [
                (puzzle at: row) at: col put: val.  "Place value"

                self solve ifTrue: [^true].

                (puzzle at: row) at: col put: 0.  "Backtrack"
            ].
        ].

        ^false
    ]

    run: filename [
        | startTime elapsed secs ms |
        startTime := Time millisecondClockValue.

        self readMatrixFile: filename.
        self printPuzzle.
        count := 0.
        self solve.

        elapsed := Time millisecondClockValue - startTime.
        secs := elapsed // 1000.
        ms := elapsed \\ 1000.
        Transcript nextPutAll: 'Seconds to process ';
                   nextPutAll: secs printString;
                   nextPut: $.;
                   nextPutAll: ms printString; cr.
    ]
]

"Main program"
| solver args |
args := Smalltalk arguments.
args do: [:arg |
    (arg endsWith: '.matrix') ifTrue: [
        solver := SudokuSolver new.
        solver run: arg.
    ].
].
