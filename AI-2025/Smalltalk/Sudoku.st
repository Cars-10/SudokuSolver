Object subclass: Sudoku [
    | puzzle count |
    
    Sudoku class >> new [
        ^super new init
    ]
    
    init [
        puzzle := Array new: 9.
        1 to: 9 do: [:i | puzzle at: i put: (Array new: 9)].
        count := 0.
    ]
    
    printPuzzle [
        Transcript nl; show: 'Puzzle:'; nl.
        1 to: 9 do: [:r |
            1 to: 9 do: [:c |
                Transcript show: ((puzzle at: r) at: c) printString; show: ' '.
            ].
            Transcript nl.
        ].
    ]
    
    readMatrixFile: filename [
        | f line parts row |
        Transcript show: filename; nl.
        f := FileStream open: filename mode: FileStream read.
        row := 1.
        [f atEnd] whileFalse: [
            line := f nextLine.
            (line startsWith: '#') ifFalse: [
                parts := line subStrings: ' '.
                (parts size = 9) ifTrue: [
                    1 to: 9 do: [:col |
                        (puzzle at: row) at: col put: (parts at: col) asInteger.
                    ].
                    row := row + 1.
                    (row > 9) ifTrue: [^self].
                ].
            ].
        ].
        f close.
    ]
    
    isPossible: r col: c val: val [
        | r0 c0 |
        1 to: 9 do: [:i |
            ((puzzle at: i) at: c) = val ifTrue: [^false].
            ((puzzle at: r) at: i) = val ifTrue: [^false].
        ].
        
        r0 := ((r - 1) // 3) * 3 + 1.
        c0 := ((c - 1) // 3) * 3 + 1.
        
        0 to: 2 do: [:i |
            0 to: 2 do: [:j |
                ((puzzle at: r0 + i) at: c0 + j) = val ifTrue: [^false].
            ].
        ].
        ^true
    ]
    
    solve [
        1 to: 9 do: [:r |
            1 to: 9 do: [:c |
                ((puzzle at: r) at: c) = 0 ifTrue: [
                    1 to: 9 do: [:val |
                        count := count + 1.
                        (self isPossible: r col: c val: val) ifTrue: [
                            (puzzle at: r) at: c put: val.
                            (self solve) ifTrue: [^true].
                            (puzzle at: r) at: c put: 0.
                        ].
                    ].
                    ^false
                ].
            ].
        ].
        self printPuzzle.
        Transcript nl; show: 'Solved in Iterations='; show: count printString; nl; nl.
        ^true
    ]
    
    resetCount [
        count := 0.
    ]
]

| sudoku start end args |
sudoku := Sudoku new.
start := Time millisecondClockValue.
args := Smalltalk arguments.
args do: [:arg |
    (arg endsWith: '.matrix') ifTrue: [
        sudoku readMatrixFile: arg.
        sudoku printPuzzle.
        sudoku resetCount.
        sudoku solve.
    ].
].
end := Time millisecondClockValue.
Transcript show: 'Seconds to process '; show: ((end - start) / 1000.0) printString; nl.
