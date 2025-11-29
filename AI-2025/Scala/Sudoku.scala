import scala.io.Source
import java.io.File
import scala.util.control.Breaks._

object Sudoku {
  val puzzle = Array.ofDim[Int](9, 9)
  var count = 0

  def printPuzzle(): Unit = {
    println("\nPuzzle:")
    for (j <- 0 until 9) {
      for (i <- 0 until 9) {
        print(s"${puzzle(j)(i)} ")
      }
      println()
    }
  }

  def readMatrixFile(filename: String): Unit = {
    println(filename)
    val source = Source.fromFile(filename)
    val lines = source.getLines().filterNot(line => line.startsWith("#") || line.trim.isEmpty)
    var row = 0
    breakable {
      for (line <- lines) {
        val parts = line.trim.split("\\s+")
        if (parts.length == 9) {
          for (col <- 0 until 9) {
            puzzle(row)(col) = parts(col).toInt
          }
          row += 1
          if (row == 9) break
        }
      }
    }
    source.close()
  }

  def isPossible(y: Int, x: Int, value: Int): Boolean = {
    for (i <- 0 until 9) {
      if (puzzle(i)(x) == value) return false
      if (puzzle(y)(i) == value) return false
    }

    val x0 = (x / 3) * 3
    val y0 = (y / 3) * 3

    for (i <- 0 until 3) {
      for (j <- 0 until 3) {
        if (puzzle(y0 + i)(x0 + j) == value) return false
      }
    }
    true
  }

  def solve(): Int = {
    for (j <- 0 until 9) {
      for (i <- 0 until 9) {
        if (puzzle(j)(i) == 0) {
          for (value <- 1 to 9) {
            count += 1
            if (isPossible(j, i, value)) {
              puzzle(j)(i) = value
              if (solve() == 2) return 2
              puzzle(j)(i) = 0
            }
          }
          return 0
        }
      }
    }
    printPuzzle()
    println(s"\nSolved in Iterations=$count\n")
    return 2
  }

  def main(args: Array[String]): Unit = {
    val start = System.nanoTime()
    for (arg <- args) {
      if (arg.endsWith(".matrix")) {
        readMatrixFile(arg)
        printPuzzle()
        count = 0
        solve()
      }
    }
    val end = System.nanoTime()
    val seconds = (end - start) / 1000000000.0
    printf("Seconds to process %.3f\n", seconds)
  }
}
