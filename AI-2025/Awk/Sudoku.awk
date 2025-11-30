#!/usr/bin/awk -f

BEGIN {
    # No initialization needed
}

function print_puzzle() {
    print "\nPuzzle:"
    for (r = 0; r < 9; r++) {
        line = ""
        for (c = 0; c < 9; c++) {
            line = line puzzle[r, c] " "
        }
        print line
    }
}

function is_possible(r, c, val,    i, j, r0, c0) {
    for (i = 0; i < 9; i++) {
        if (puzzle[i, c] == val) return 0
        if (puzzle[r, i] == val) return 0
    }
    
    r0 = int(r / 3) * 3
    c0 = int(c / 3) * 3
    
    for (i = 0; i < 3; i++) {
        for (j = 0; j < 3; j++) {
            if (puzzle[r0 + i, c0 + j] == val) return 0
        }
    }
    return 1
}

function solve(    r, c, val, found) {
    found = 0
    for (r = 0; r < 9; r++) {
        for (c = 0; c < 9; c++) {
            if (puzzle[r, c] == 0) {
                found = 1
                break
            }
        }
        if (found) break
    }
    
    if (!found) return 1
    
    for (val = 1; val <= 9; val++) {
        count++
        if (is_possible(r, c, val)) {
            puzzle[r, c] = val
            if (solve()) return 1
            puzzle[r, c] = 0
        }
    }
    return 0
}

function process_file(filename,    line, parts, r, c, i) {
    print filename
    r = 0
    while ((getline line < filename) > 0) {
        if (line ~ /^#/) continue
        if (length(line) < 2) continue
        
        split(line, parts, " ")
        if (length(parts) == 9) { # Awk split index starts at 1
            for (c = 0; c < 9; c++) {
                puzzle[r, c] = parts[c+1] + 0
            }
            r++
            if (r == 9) break
        }
    }
    close(filename)
    
    print_puzzle()
    count = 0
    if (solve()) {
        print_puzzle()
        print "Solved in Iterations=" count "\n"
    } else {
        print "No solution found"
    }
}

BEGIN {
    for (i = 1; i < ARGC; i++) {
        if (ARGV[i] ~ /\.matrix$/) {
            process_file(ARGV[i])
        }
    }
}
