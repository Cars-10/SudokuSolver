Red [
    Title: "Sudoku Solver"
    Purpose: "Brute-force backtracking Sudoku solver for benchmark"
]

; Global variables
puzzle: copy []
count: 0

; Initialize 9x9 puzzle grid
init-puzzle: does [
    puzzle: copy []
    loop 81 [append puzzle 0]
]

; Get cell value at (row, col) - 0-indexed
get-cell: func [row [integer!] col [integer!]][
    pick puzzle (row * 9 + col + 1)
]

; Set cell value at (row, col) - 0-indexed
set-cell: func [row [integer!] col [integer!] val [integer!]][
    poke puzzle (row * 9 + col + 1) val
]

; Print the puzzle grid
print-puzzle: does [
    print "^/Puzzle:"
    repeat row 9 [
        repeat col 9 [
            prin get-cell (row - 1) (col - 1)
            prin " "
        ]
        print ""
    ]
]

; Check if placing val at (row, col) is valid
is-valid: func [row [integer!] col [integer!] val [integer!] /local i box-row box-col br bc][
    ; Check row
    repeat i 9 [
        if val = get-cell row (i - 1) [return false]
    ]

    ; Check column
    repeat i 9 [
        if val = get-cell (i - 1) col [return false]
    ]

    ; Check 3x3 box
    box-row: row / 3 * 3
    box-col: col / 3 * 3
    repeat br 3 [
        repeat bc 3 [
            if val = get-cell (box-row + br - 1) (box-col + bc - 1) [return false]
        ]
    ]

    true
]

; Find first empty cell (row-major order)
; Returns block [row col] or none
find-empty: func [/local row col][
    repeat row 9 [
        repeat col 9 [
            if 0 = get-cell (row - 1) (col - 1) [
                return reduce [row - 1 col - 1]
            ]
        ]
    ]
    none
]

; Brute-force solver with backtracking
solve: func [/local empty row col val][
    empty: find-empty

    ; If no empty cell, puzzle is solved
    if none? empty [
        print-puzzle
        print rejoin ["^/Solved in Iterations=" count "^/"]
        return true
    ]

    row: first empty
    col: second empty

    ; Try values 1-9 in order
    repeat val 9 [
        count: count + 1  ; Count EVERY attempt

        if is-valid row col val [
            set-cell row col val

            if solve [return true]

            set-cell row col 0  ; Backtrack
        ]
    ]

    false
]

; Read matrix file and populate puzzle
read-matrix: func [filename [string!] /local lines line row nums i n][
    ; Print filename
    print filename

    ; Initialize puzzle
    init-puzzle

    ; Read file
    lines: read/lines to-red-file filename
    row: 0

    foreach line lines [
        ; Skip comments and empty lines
        if all [
            not empty? line
            #"#" <> first line
        ][
            ; Parse 9 integers from line
            nums: copy []
            parse line [
                any [
                    copy n some [#"0" | #"1" | #"2" | #"3" | #"4" | #"5" | #"6" | #"7" | #"8" | #"9"]
                    (append nums to-integer n)
                    | skip
                ]
            ]

            if 9 <= length? nums [
                repeat i 9 [
                    set-cell row (i - 1) pick nums i
                    prin pick nums i
                    prin " "
                ]
                print ""
                row: row + 1
                if row >= 9 [break]
            ]
        ]
    ]
]

; Main execution
main: does [
    args: system/options/args

    if any [none? args empty? args][
        print "Usage: red Sudoku.red <matrix-file>"
        quit
    ]

    start: now/time/precise

    foreach arg args [
        if find arg ".matrix" [
            read-matrix arg
            print-puzzle
            count: 0
            solve
        ]
    ]

    elapsed: difference now/time/precise start
    print rejoin ["Seconds to process " round/to to-float elapsed 0.001]
]

main
