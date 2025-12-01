import java.io.File

var puzzle = Array(9) { IntArray(9) }
var iterations = 0

fun main(args: Array<String>) {
    if (args.isEmpty()) {
        println("Usage: kotlinc Sudoku.kt -include-runtime -d Sudoku.jar && java -jar Sudoku.jar <file1> <file2> ...")
        return
    }

    for (filename in args) {
        println("\nProcessing $filename")
        if (readBoard(filename)) {
            printBoard()
            iterations = 0
            if (solve(0, 0)) {
                printBoard()
                println("\nSolved in Iterations=$iterations")
            } else {
                println("No solution found")
            }
        }
    }
}

fun readBoard(filename: String): Boolean {
    try {
        val lines = File(filename).readLines()
        var row = 0
        for (line in lines) {
            val trimmed = line.trim()
            if (trimmed.isNotEmpty() && !trimmed.startsWith("#")) {
                val parts = trimmed.split(Regex("\\s+"))
                var col = 0
                for (part in parts) {
                    if (col < 9) {
                        puzzle[row][col] = part.toInt()
                        col++
                    }
                }
                row++
                if (row == 9) break
            }
        }
        return true
    } catch (e: Exception) {
        println("Error reading file $filename: ${e.message}")
        return false
    }
}

fun printBoard() {
    println("Puzzle:")
    for (i in 0 until 9) {
        for (j in 0 until 9) {
            print("${puzzle[i][j]} ")
        }
        println()
    }
}

fun isPossible(row: Int, col: Int, num: Int): Boolean {
    for (i in 0 until 9) {
        if (puzzle[row][i] == num || puzzle[i][col] == num) return false
    }
    val startRow = (row / 3) * 3
    val startCol = (col / 3) * 3
    for (i in 0 until 3) {
        for (j in 0 until 3) {
            if (puzzle[startRow + i][startCol + j] == num) return false
        }
    }
    return true
}

fun solve(row: Int, col: Int): Boolean {
    if (row == 9) return true

    var nextRow = row
    var nextCol = col + 1
    if (nextCol == 9) {
        nextRow = row + 1
        nextCol = 0
    }

    if (puzzle[row][col] != 0) {
        return solve(nextRow, nextCol)
    }

    for (num in 1..9) {
        iterations++
        if (isPossible(row, col, num)) {
            puzzle[row][col] = num
            if (solve(nextRow, nextCol)) return true
            puzzle[row][col] = 0
        }
    }
    return false
}
