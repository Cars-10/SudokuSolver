/**
 * Sudoku Solver - Kotlin Implementation
 * Brute-force backtracking algorithm matching C reference exactly.
 *
 * Algorithm:
 * - Row-major search for empty cells (top-to-bottom, left-to-right)
 * - Try values 1-9 in ascending order
 * - Count EVERY placement attempt (algorithm fingerprint)
 */

import java.io.File

var puzzle = Array(9) { IntArray(9) }
var count: Long = 0

fun main(args: Array<String>) {
    val startTime = System.nanoTime()

    for (arg in args) {
        if (!arg.endsWith(".matrix")) continue

        // Reset puzzle
        puzzle = Array(9) { IntArray(9) }

        readMatrixFile(arg)
        printPuzzle()
        count = 0
        solve()
    }

    val elapsed = (System.nanoTime() - startTime) / 1_000_000_000.0
    println("Seconds to process %.3f".format(elapsed))
}

fun readMatrixFile(filename: String) {
    // Normalize path for output (match C format)
    var displayPath = filename
    if (filename.startsWith("/app/Matrices/")) {
        displayPath = "../" + filename.substring(5)  // Skip "/app/" to get "Matrices/..."
    }
    println(displayPath)

    val file = File(filename)
    var lineCount = 0

    file.forEachLine { line ->
        if (lineCount >= 9) return@forEachLine
        val trimmed = line.trim()
        // Skip comments and empty lines
        if (trimmed.isEmpty() || trimmed.startsWith("#")) return@forEachLine

        // Parse 9 integers from line
        val parts = trimmed.split(Regex("\\s+"))
        if (parts.size == 9) {
            val sb = StringBuilder()
            for (j in 0 until 9) {
                puzzle[lineCount][j] = parts[j].toInt()
                sb.append(puzzle[lineCount][j]).append(" ")
            }
            println(sb.toString())
            lineCount++
        }
    }
}

fun printPuzzle() {
    println("\nPuzzle:")
    for (row in 0 until 9) {
        val sb = StringBuilder()
        for (col in 0 until 9) {
            sb.append(puzzle[row][col]).append(" ")
        }
        println(sb.toString())
    }
}

fun isValid(row: Int, col: Int, value: Int): Boolean {
    // Check row
    for (i in 0 until 9) {
        if (puzzle[row][i] == value) return false
    }

    // Check column
    for (i in 0 until 9) {
        if (puzzle[i][col] == value) return false
    }

    // Check 3x3 box
    val boxRow = (row / 3) * 3
    val boxCol = (col / 3) * 3
    for (i in 0 until 3) {
        for (j in 0 until 3) {
            if (puzzle[boxRow + i][boxCol + j] == value) return false
        }
    }

    return true
}

fun solve(): Boolean {
    // Find first empty cell (row-major order)
    var row = -1
    var col = -1
    outer@ for (r in 0 until 9) {
        for (c in 0 until 9) {
            if (puzzle[r][c] == 0) {
                row = r
                col = c
                break@outer
            }
        }
    }

    // If no empty cell found, puzzle is solved
    if (row == -1) {
        printPuzzle()
        println("\nSolved in Iterations=$count\n")
        return true
    }

    // Try values 1-9 in order
    for (value in 1..9) {
        count++  // COUNT EVERY ATTEMPT - this is the algorithm fingerprint

        if (isValid(row, col, value)) {
            puzzle[row][col] = value  // Place value

            if (solve()) {
                return true
            }

            puzzle[row][col] = 0  // Backtrack
        }
    }

    return false
}
