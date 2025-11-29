class Sudoku {
    static int[][] puzzle = new int[9][9]
    static int count = 0
    static final int DEBUG = 0

    static void printPuzzle() {
        println "\nPuzzle:"
        for (int j = 0; j < 9; j++) {
            for (int i = 0; i < 9; i++) {
                print "${puzzle[j][i]} "
            }
            println()
        }
    }

    static void readMatrixFile(String filename) {
        println filename
        def file = new File(filename)
        def lines = file.readLines()
        int row = 0
        for (String line : lines) {
            if (line.startsWith("#") || line.trim().isEmpty()) continue
            def parts = line.trim().split(/\s+/)
            if (parts.size() == 9) {
                for (int col = 0; col < 9; col++) {
                    puzzle[row][col] = parts[col].toInteger()
                }
                row++
                if (row == 9) break
            }
        }
    }

    static boolean isPossible(int y, int x, int val) {
        for (int i = 0; i < 9; i++) {
            if (puzzle[i][x] == val) return false
            if (puzzle[y][i] == val) return false
        }

        int x0 = (int) (x / 3) * 3
        int y0 = (int) (y / 3) * 3

        for (int i = 0; i < 3; i++) {
            for (int j = 0; j < 3; j++) {
                if (puzzle[y0 + i][x0 + j] == val) return false
            }
        }
        return true
    }

    static int solve() {
        for (int j = 0; j < 9; j++) {
            for (int i = 0; i < 9; i++) {
                if (puzzle[j][i] == 0) {
                    for (int val = 1; val <= 9; val++) {
                        count++
                        if (isPossible(j, i, val)) {
                            puzzle[j][i] = val
                            if (solve() == 2) return 2
                            puzzle[j][i] = 0
                        }
                    }
                    return 0
                }
            }
        }
        printPuzzle()
        println "\nSolved in Iterations=$count\n"
        return 2
    }

    static void main(String[] args) {
        long start = System.nanoTime()
        for (String arg : args) {
            if (arg.endsWith(".matrix")) {
                readMatrixFile(arg)
                printPuzzle()
                count = 0
                solve()
            }
        }
        long end = System.nanoTime()
        printf "Seconds to process %.3f\n", (end - start) / 1_000_000_000.0
    }
}
