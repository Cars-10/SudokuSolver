import scala.io.Source
import java.io.File

var puzzle = Array.ofDim[Int](9, 9)
var iterations = 0

def readBoard(filename: String): Boolean = {
  try {
    val lines = Source.fromFile(filename).getLines()
    var row = 0
    while (lines.hasNext && row < 9) {
      val line = lines.next()
      val trimmed = line.trim
      if (trimmed.nonEmpty && !trimmed.startsWith("#")) {
        val parts = trimmed.split("\\s+")
        var col = 0
        for (part <- parts) {
          if (col < 9) {
            puzzle(row)(col) = part.toInt
            col += 1
          }
        }
        row += 1
      }
    }
    true
  } catch {
    case e: Exception =>
      println(s"Error reading file $filename: ${e.getMessage}")
      false
  }
}

def printBoard(): Unit = {
  println("Puzzle:")
  for (i <- 0 until 9) {
    for (j <- 0 until 9) {
      print(s"${puzzle(i)(j)} ")
    }
    println()
  }
}

def isPossible(row: Int, col: Int, num: Int): Boolean = {
  var possible = true
  var i = 0
  while (i < 9 && possible) {
    if (puzzle(row)(i) == num || puzzle(i)(col) == num) possible = false
    i += 1
  }
  
  if (possible) {
    val startRow = (row / 3) * 3
    val startCol = (col / 3) * 3
    i = 0
    while (i < 3 && possible) {
      var j = 0
      while (j < 3 && possible) {
        if (puzzle(startRow + i)(startCol + j) == num) possible = false
        j += 1
      }
      i += 1
    }
  }
  possible
}

def solve(row: Int, col: Int): Boolean = {
  if (row == 9) {
    true
  } else {
    var nextRow = row
    var nextCol = col + 1
    if (nextCol == 9) {
      nextRow = row + 1
      nextCol = 0
    }

    if (puzzle(row)(col) != 0) {
      solve(nextRow, nextCol)
    } else {
      var found = false
      var num = 1
      while (num <= 9 && !found) {
        iterations += 1
        if (isPossible(row, col, num)) {
          puzzle(row)(col) = num
          if (solve(nextRow, nextCol)) {
            found = true
          } else {
            puzzle(row)(col) = 0
          }
        }
        num += 1
      }
      found
    }
  }
}

@main def run(args: String*): Unit = {
  if (args.isEmpty) {
    println("Usage: scala Sudoku.scala <file1> <file2> ...")
    System.exit(1)
  }

  for (filename <- args) {
    println(s"\nProcessing $filename")
    if (readBoard(filename)) {
      printBoard()
      iterations = 0
      if (solve(0, 0)) {
        printBoard()
        println(s"\nSolved in Iterations=$iterations")
      } else {
        println("No solution found")
      }
    }
  }
}
