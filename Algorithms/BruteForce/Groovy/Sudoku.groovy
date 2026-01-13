import groovy.transform.Field

// Sudoku puzzle grid [row][col]
@Field int[][] puzzle = new int[9][9]
@Field int count = 0  // Iteration counter

void printPuzzle() {
    println "\nPuzzle:"
    for (int j = 0; j < 9; j++) {
        for (int i = 0; i < 9; i++) {
            print "${puzzle[j][i]} "
        }
        println ""
    }
}

int readMatrixFile(String filename) {
    def file = new File(filename)
    if (!file.exists()) {
        System.err.println "Error opening file '${filename}'"
        return 1
    }

    // Normalize path for output (convert absolute to relative)
    if (filename.startsWith("/app/Matrices/")) {
        println "../${filename.substring(5)}"
    } else {
        println filename
    }

    int lineCount = 0
    file.eachLine { line ->
        String lineStr = line.trim()

        // Skip comments and empty lines
        if (lineStr.isEmpty() || lineStr.startsWith('#')) {
            return  // continue in Groovy closure
        }

        if (lineCount >= 9) return

        // Parse 9 integers from line
        def parts = lineStr.split(/\s+/)
        if (parts.length >= 9) {
            for (int i = 0; i < 9; i++) {
                puzzle[lineCount][i] = parts[i] as int
                print "${puzzle[lineCount][i]} "
            }
            println ""
            lineCount++
        }
    }

    return 0
}

// Check if placing val at (row, col) is valid
boolean isValid(int row, int col, int val) {
    // Check row
    for (int i = 0; i < 9; i++) {
        if (puzzle[row][i] == val) return false
    }

    // Check column
    for (int i = 0; i < 9; i++) {
        if (puzzle[i][col] == val) return false
    }

    // Check 3x3 box
    int boxRow = (int)(row / 3) * 3
    int boxCol = (int)(col / 3) * 3
    for (int i = 0; i < 3; i++) {
        for (int j = 0; j < 3; j++) {
            if (puzzle[boxRow + i][boxCol + j] == val) return false
        }
    }

    return true
}

// BRUTE-FORCE SOLVER
// Searches row-major order (top-to-bottom, left-to-right)
// Tries candidates 1-9 in ascending order
// Counts EVERY placement attempt (the algorithm fingerprint)
boolean solve() {
    // Find first empty cell (row-major order)
    int row = -1, col = -1
    for (int r = 0; r < 9 && row == -1; r++) {
        for (int c = 0; c < 9; c++) {
            if (puzzle[r][c] == 0) {
                row = r
                col = c
                break
            }
        }
    }

    // If no empty cell found, puzzle is solved
    if (row == -1) {
        printPuzzle()
        println "\nSolved in Iterations=${count}\n"
        return true  // Success
    }

    // Try values 1-9 in order
    for (int val = 1; val <= 9; val++) {
        count++  // COUNT EVERY ATTEMPT - this is the algorithm fingerprint

        if (isValid(row, col, val)) {
            puzzle[row][col] = val  // Place value

            if (solve()) {
                return true  // Solved
            }

            puzzle[row][col] = 0  // Backtrack
        }
    }

    return false  // No solution found
}

// Main
long startTime = System.nanoTime()

// Process each .matrix file from command line
args.each { filename ->
    if (filename.endsWith('.matrix')) {
        if (readMatrixFile(filename) != 0) {
            System.err.println "Error reading ${filename}"
            return
        }

        printPuzzle()
        count = 0
        solve()
    }
}

long elapsed = System.nanoTime() - startTime
printf "Seconds to process %.3f%n", elapsed / 1_000_000_000.0
