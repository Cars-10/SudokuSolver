"CP (Constraint Propagation) Sudoku Solver - GNU Smalltalk Implementation"

Object subclass: CPSolver [
    | values candidates iterations |

    CPSolver class >> new [
        ^super new initialize
    ]

    initialize [
        values := Array new: 81 withAll: 0.
        candidates := Array new: 81 withAll: 0.
        iterations := 0.
    ]

    idx: r col: c [ ^(r * 9) + c + 1 ]

    hasCandidate: set digit: d [ ^((set bitAnd: (1 bitShift: d)) ~= 0) ]
    removeCandidate: set digit: d [ ^(set bitAnd: (1 bitShift: d) bitInvert) ]

    countCandidates: set [
        | count s |
        count := 0. s := set.
        [s > 0] whileTrue: [
            count := count + (s bitAnd: 1).
            s := s bitShift: -1.
        ].
        ^count
    ]

    firstCandidate: set [
        1 to: 9 do: [:d |
            (self hasCandidate: set digit: d) ifTrue: [^d].
        ].
        ^0
    ]

    getPeers: idx [
        | peers r c boxR boxC |
        peers := Set new.
        r := (idx - 1) // 9.
        c := (idx - 1) \\ 9.
        0 to: 8 do: [:i |
            | ri ci |
            ri := (self idx: r col: i).
            ci := (self idx: i col: c).
            ri ~= idx ifTrue: [peers add: ri].
            ci ~= idx ifTrue: [peers add: ci].
        ].
        boxR := (r // 3) * 3.
        boxC := (c // 3) * 3.
        boxR to: boxR + 2 do: [:br |
            boxC to: boxC + 2 do: [:bc |
                | bi |
                bi := (self idx: br col: bc).
                (br ~= r or: [bc ~= c]) ifTrue: [peers add: bi].
            ].
        ].
        ^peers asArray
    ]

    eliminate: idx digit: d [
        | cands remaining lastD |
        (self hasCandidate: (candidates at: idx) digit: d) ifFalse: [^true].
        candidates at: idx put: (self removeCandidate: (candidates at: idx) digit: d).
        cands := candidates at: idx.
        remaining := self countCandidates: cands.
        remaining = 0 ifTrue: [^false].
        (remaining = 1 and: [(values at: idx) = 0]) ifTrue: [
            lastD := self firstCandidate: cands.
            (self assign: idx digit: lastD) ifFalse: [^false].
        ].
        ^true
    ]

    assign: idx digit: d [
        | peers |
        iterations := iterations + 1.
        values at: idx put: d.
        candidates at: idx put: (1 bitShift: d).
        peers := self getPeers: idx.
        peers do: [:p |
            (self eliminate: p digit: d) ifFalse: [^false].
        ].
        ^true
    ]

    propagate [
        | changed |
        changed := true.
        [changed] whileTrue: [
            changed := false.
            1 to: 81 do: [:idx |
                (values at: idx) = 0 ifTrue: [
                    | numCands digit |
                    numCands := self countCandidates: (candidates at: idx).
                    numCands = 0 ifTrue: [^false].
                    numCands = 1 ifTrue: [
                        digit := self firstCandidate: (candidates at: idx).
                        (self assign: idx digit: digit) ifFalse: [^false].
                        changed := true.
                    ].
                ].
            ].
        ].
        ^true
    ]

    findMrv [
        | minCands mrvIdx |
        minCands := 10. mrvIdx := -1.
        1 to: 81 do: [:idx |
            (values at: idx) = 0 ifTrue: [
                | numCands |
                numCands := self countCandidates: (candidates at: idx).
                numCands < minCands ifTrue: [
                    minCands := numCands.
                    mrvIdx := idx.
                ].
            ].
        ].
        ^mrvIdx
    ]

    copyState [
        ^Array with: (values copy) with: (candidates copy)
    ]

    restoreState: state [
        values := (state at: 1) copy.
        candidates := (state at: 2) copy.
    ]

    search [
        | mrvIdx cands saved |
        mrvIdx := self findMrv.
        mrvIdx = -1 ifTrue: [^true].
        cands := candidates at: mrvIdx.
        1 to: 9 do: [:d |
            (self hasCandidate: cands digit: d) ifTrue: [
                saved := self copyState.
                (self assign: mrvIdx digit: d) ifTrue: [
                    self propagate ifTrue: [
                        self search ifTrue: [^true].
                    ].
                ].
                self restoreState: saved.
            ].
        ].
        ^false
    ]

    initFromPuzzle: puzzle [
        0 to: 8 do: [:r |
            0 to: 8 do: [:c |
                | idx val |
                idx := (self idx: r col: c).
                val := (puzzle at: r + 1) at: c + 1.
                val = 0
                    ifTrue: [candidates at: idx put: 16r3FE]
                    ifFalse: [
                        values at: idx put: val.
                        candidates at: idx put: (1 bitShift: val).
                    ].
            ].
        ].
    ]

    solve: puzzle [
        self initFromPuzzle: puzzle.
        self propagate ifFalse: [^false].
        iterations := 0.
        ^self search
    ]

    getSolution [
        | solution |
        solution := Array new: 9.
        1 to: 9 do: [:r |
            | row |
            row := Array new: 9.
            1 to: 9 do: [:c |
                row at: c put: (values at: ((r - 1) * 9) + c).
            ].
            solution at: r put: row.
        ].
        ^solution
    ]

    iterations [ ^iterations ]
]

Object subclass: Main [
    Main class >> run: filename [
        | puzzle solver solution file line lineCount startTime elapsed |
        puzzle := Array new: 9.
        1 to: 9 do: [:i | puzzle at: i put: (Array new: 9 withAll: 0)].

        "Read matrix"
        (filename startsWith: '/app/Matrices/')
            ifTrue: [Transcript nextPutAll: '../'; nextPutAll: (filename copyFrom: 6 to: filename size); cr]
            ifFalse: [Transcript nextPutAll: filename; cr].

        lineCount := 0.
        file := FileStream open: filename mode: FileStream read.
        [file atEnd] whileFalse: [
            line := file nextLine trimSeparators.
            (line isEmpty or: [line first = $#]) ifFalse: [
                lineCount < 9 ifTrue: [
                    | values |
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

        Transcript cr; nextPutAll: 'Puzzle:'; cr.
        1 to: 9 do: [:r |
            1 to: 9 do: [:c |
                Transcript nextPutAll: ((puzzle at: r) at: c) printString.
                c < 9 ifTrue: [Transcript nextPut: $ ].
            ].
            Transcript nextPutAll: ' '; cr.
        ].

        startTime := Time millisecondClockValue.
        solver := CPSolver new.
        (solver solve: puzzle)
            ifTrue: [
                solution := solver getSolution.
                Transcript cr; nextPutAll: 'Puzzle:'; cr.
                1 to: 9 do: [:r |
                    1 to: 9 do: [:c |
                        Transcript nextPutAll: ((solution at: r) at: c) printString.
                        c < 9 ifTrue: [Transcript nextPut: $ ].
                    ].
                    Transcript nextPutAll: ' '; cr.
                ].
                Transcript cr; nextPutAll: 'Solved in Iterations='; nextPutAll: solver iterations printString; cr.
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
Smalltalk arguments do: [:arg |
    (arg endsWith: '.matrix') ifTrue: [
        Main run: arg.
    ].
].
