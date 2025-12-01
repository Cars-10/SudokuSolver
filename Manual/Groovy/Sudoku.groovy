class Sudoku {
    static int[][] puzzle
    static int iterations = 0

    static void main(String[] args) {
        if (args.length == 0) {
            println "Usage: groovy Sudoku.groovy <file1> <file2> ..."
            System.exit(1)
        }

        args.each { filename ->
            println "\nProcessing $filename"
            if (readBoard(filename)) {
                printBoard()
                iterations = 0
                try {
                    if (solve(0, 0)) {
                        printBoard()
                        println "\nSolved in Iterations=$iterations"
                    } else {
                        println "No solution found"
                    }
                } catch (Throwable t) {
                    println "Error solving: $t"
                    t.printStackTrace()
                }
            }
        }
    }

    static boolean readBoard(String filename) {
        puzzle = new int[9][9]
        try {
            def lines = new File(filename).readLines()
            int row = 0
            for (line in lines) {
                def trimmed = line.trim()
                if (trimmed && !trimmed.startsWith("#")) {
                    def parts = trimmed.split(/\s+/)
                    int col = 0
                    for (part in parts) {
                        if (col < 9) {
                            if (part.isInteger()) {
                                puzzle[row][col] = part.toInteger()
                                col++
                            }
                        }
                    }
                    row++
                    if (row == 9) return true
                }
            }
            return true
        } catch (Exception e) {
            println "Error reading file $filename: ${e.message}"
            return false
        }
    }

    static void printBoard() {
        println "Puzzle:"
        for (int i = 0; i < 9; i++) {
            for (int j = 0; j < 9; j++) {
                print "${puzzle[i][j]} "
            }
            println()
        }
    }

    static boolean isPossible(int row, int col, int num) {
        for (int i = 0; i < 9; i++) {
            if (puzzle[row][i] == num || puzzle[i][col] == num) return false
        }
        int startRow = row.intdiv(3) * 3
        int startCol = col.intdiv(3) * 3
        for (int i = 0; i < 3; i++) {
            for (int j = 0; j < 3; j++) {
                if (puzzle[startRow + i][startCol + j] == num) return false
            }
        }
        return true
    }

    static boolean solve(int row, int col) {
        if (row == 9) return true

        int nextRow = row
        int nextCol = col + 1
        if (nextCol == 9) {
            nextRow = row + 1
            nextCol = 0
        }

        if (puzzle[row][col] != 0) {
            return solve(nextRow, nextCol)
        }

        for (int num = 1; num <= 9; num++) {
            iterations++
            if (isPossible(row, col, num)) {
                puzzle[row][col] = num
                if (solve(nextRow, nextCol)) return true
                puzzle[row][col] = 0
            }
        }
        return false
    }
}
