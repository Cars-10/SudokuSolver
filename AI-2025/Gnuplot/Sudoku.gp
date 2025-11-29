# Gnuplot Sudoku Solver

# Gnuplot has arrays and functions, and recursion (in recent versions).
# It can do logic.

array puzzle[81]
count = 0

# Function to print puzzle
print_puzzle() = {
    print "\nPuzzle:"
    do for [r=0:8] {
        line = ""
        do for [c=0:8] {
            line = line . sprintf("%d ", puzzle[r*9 + c + 1])
        }
        print line
    }
}

# Check validity
is_possible(r, c, val) = {
    possible = 1
    # Row and Col
    do for [i=0:8] {
        if (puzzle[i*9 + c + 1] == val) { possible = 0 }
        if (puzzle[r*9 + i + 1] == val) { possible = 0 }
    }
    # Box
    r0 = floor(r / 3) * 3
    c0 = floor(c / 3) * 3
    do for [i=0:2] {
        do for [j=0:2] {
            if (puzzle[(r0+i)*9 + (c0+j) + 1] == val) { possible = 0 }
        }
    }
    return possible
}

# Recursive Solve
# Gnuplot recursion limit might be an issue.
solve(depth) = {
    # Find empty
    found_empty = 0
    er = -1
    ec = -1
    
    # We can't break easily from nested loops in function in older gnuplot, 
    # but let's try standard iteration
    do for [r=0:8] {
        do for [c=0:8] {
            if (found_empty == 0 && puzzle[r*9 + c + 1] == 0) {
                found_empty = 1
                er = r
                ec = c
            }
        }
    }
    
    if (found_empty == 0) {
        print_puzzle()
        print sprintf("\nSolved in Iterations=%d\n", count)
        return 1
    }
    
    do for [val=1:9] {
        count = count + 1
        if (is_possible(er, ec, val)) {
            puzzle[er*9 + ec + 1] = val
            if (solve(depth + 1)) { return 1 }
            puzzle[er*9 + ec + 1] = 0
        }
    }
    return 0
}

# Main script logic (passed via -e or loaded)
if (exists("filename")) {
    print filename
    
    # Read file
    # Gnuplot data reading is for plots. We need to parse manually or use system.
    # We can use `stats` to read data into variables if formatted correctly.
    # Or simpler: preprocess file to flat list of numbers.
    
    stats filename using (puzzle[$0+1] = $1) nooutput
    
    print_puzzle()
    count = 0
    x = solve(0)
}
