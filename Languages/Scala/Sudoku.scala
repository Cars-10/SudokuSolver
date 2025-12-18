/**
 * Sudoku Solver - Scala Implementation
 * Brute-force backtracking algorithm matching C reference exactly.
 *
 * Algorithm:
 * - Row-major search for empty cells (top-to-bottom, left-to-right)
 * - Try values 1-9 in ascending order
 * - Count EVERY placement attempt (algorithm fingerprint)
 *
 * Note: Uses imperative style (var, mutable Array) to match C algorithm exactly.
 */

import scala.io.Source
import java.io.File

object Sudoku {
  var puzzle: Array[Array[Int]] = Array.ofDim[Int](9, 9)
  var count: Long = 0

  def main(args: Array[String]): Unit = {
    val startTime = System.nanoTime()

    for (arg <- args) {
      if (arg.endsWith(".matrix")) {
        // Reset puzzle
        puzzle = Array.ofDim[Int](9, 9)

        readMatrixFile(arg)
        printPuzzle()
        count = 0
        solve()
      }
    }

    val elapsed = (System.nanoTime() - startTime) / 1000000000.0
    println(f"Seconds to process $elapsed%.3f")
  }

  def readMatrixFile(filename: String): Unit = {
    // Normalize path for output (match C format)
    var displayPath = filename
    if (filename.startsWith("/app/Matrices/")) {
      displayPath = "../" + filename.substring(5)  // Skip "/app/" to get "Matrices/..."
    }
    println(displayPath)

    val source = Source.fromFile(filename)
    var lineCount = 0

    try {
      for (line <- source.getLines() if lineCount < 9) {
        val trimmed = line.trim
        // Skip comments and empty lines
        if (trimmed.nonEmpty && !trimmed.startsWith("#")) {
          // Parse 9 integers from line
          val parts = trimmed.split("\\s+")
          if (parts.length == 9) {
            val sb = new StringBuilder
            for (j <- 0 until 9) {
              puzzle(lineCount)(j) = parts(j).toInt
              sb.append(puzzle(lineCount)(j)).append(" ")
            }
            println(sb.toString())
            lineCount += 1
          }
        }
      }
    } finally {
      source.close()
    }
  }

  def printPuzzle(): Unit = {
    println("\nPuzzle:")
    for (row <- 0 until 9) {
      val sb = new StringBuilder
      for (col <- 0 until 9) {
        sb.append(puzzle(row)(col)).append(" ")
      }
      println(sb.toString())
    }
  }

  def isValid(row: Int, col: Int, value: Int): Boolean = {
    // Check row
    for (i <- 0 until 9) {
      if (puzzle(row)(i) == value) return false
    }

    // Check column
    for (i <- 0 until 9) {
      if (puzzle(i)(col) == value) return false
    }

    // Check 3x3 box
    val boxRow = (row / 3) * 3
    val boxCol = (col / 3) * 3
    for (i <- 0 until 3) {
      for (j <- 0 until 3) {
        if (puzzle(boxRow + i)(boxCol + j) == value) return false
      }
    }

    true
  }

  def solve(): Boolean = {
    // Find first empty cell (row-major order)
    var row = -1
    var col = -1
    var found = false

    var r = 0
    while (r < 9 && !found) {
      var c = 0
      while (c < 9 && !found) {
        if (puzzle(r)(c) == 0) {
          row = r
          col = c
          found = true
        }
        c += 1
      }
      r += 1
    }

    // If no empty cell found, puzzle is solved
    if (row == -1) {
      printPuzzle()
      println(s"\nSolved in Iterations=$count\n")
      return true
    }

    // Try values 1-9 in order
    for (value <- 1 to 9) {
      count += 1  // COUNT EVERY ATTEMPT - this is the algorithm fingerprint

      if (isValid(row, col, value)) {
        puzzle(row)(col) = value  // Place value

        if (solve()) {
          return true
        }

        puzzle(row)(col) = 0  // Backtrack
      }
    }

    false
  }
}
