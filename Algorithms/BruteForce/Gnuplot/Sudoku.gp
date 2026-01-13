# Sudoku Solver in Gnuplot 6.0
# Exact match of C brute-force algorithm

# Redirect print to stdout
set print "-"

# Functions
function $isValid(r, c, v) << EOF
    # Row check
    do for [i=0:8] {
        if (board[r*9 + i + 1] == v) { return 0 }
    }
    # Col check
    do for [i=0:8] {
        if (board[i*9 + c + 1] == v) { return 0 }
    }
    # Box check
    br = int(r / 3) * 3
    bc = int(c / 3) * 3
    do for [bi=0:2] {
        do for [bj=0:2] {
            if (board[(br+bi)*9 + bc + bj + 1] == v) { return 0 }
        }
    }
    return 1
EOF

function $printBoard() << EOF
    do for [pr=0:8] {
        pline = ""
        do for [pc=0:8] {
            pline = pline . sprintf("%d ", board[pr*9 + pc + 1])
        }
        print pline
    }
    return 1
EOF

# 1. Identify all empty cells
array empty_cells[81]
num_empty = 0
do for [i=1:81] {
    if (board[i] == 0) {
        num_empty = num_empty + 1
        empty_cells[num_empty] = i
    }
}

# 2. Tried values stack
array tried[81]
do for [i=1:81] { tried[i] = 0 }

# 3. Solver
sptr = 1
solved = (num_empty == 0)

while (sptr > 0 && !solved) {
    idx = empty_cells[sptr]
    row = int((idx - 1) / 9)
    col = (idx - 1) % 9
    
    found_v = 0
    v_start = tried[sptr] + 1
    if (v_start <= 9) {
        do for [v = v_start : 9] {
            iterations = iterations + 1
            if ($isValid(row, col, v)) {
                board[idx] = v
                tried[sptr] = v
                found_v = 1
                break
            }
        }
    }
    
    if (found_v) {
        if (sptr == num_empty) {
            solved = 1
        } else {
            sptr = sptr + 1
        }
    } else {
        # Backtrack
        board[idx] = 0
        tried[sptr] = 0
        sptr = sptr - 1
    }
}

# Output
if (solved) {
    print "Puzzle:"
    dummy = $printBoard()
    print ""
    print sprintf("Solved in Iterations=%d", iterations)
} else {
    print "No solution found."
}