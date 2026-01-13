/**
 * Dancing Links (DLX) Sudoku Solver - Scala Implementation
 *
 * Algorithm X with Dancing Links for exact cover problem.
 * Implements Knuth's Algorithm X using circular doubly-linked lists.
 *
 * Key characteristics:
 * - Uses mutable data structures (var fields) for in-place pointer manipulation
 * - Maintains exact algorithmic behavior from C reference
 * - Counts iterations at every search call (fingerprint: 43 for Matrix 1)
 */

import scala.io.Source
import java.io.File

// DLX Node class - mutable fields required for dancing links
class DlxNode(
  var left: DlxNode,
  var right: DlxNode,
  var up: DlxNode,
  var down: DlxNode,
  var column: DlxColumn,
  var rowId: Int
) {
  // Initialize with self-references (will be updated during construction)
  def this() = this(null, null, null, null, null, -1)
}

// DLX Column header - extends DlxNode
class DlxColumn(val name: String) extends DlxNode {
  var size: Int = 0

  // Initialize as circular list pointing to self
  left = this
  right = this
  up = this
  down = this
  column = this
  rowId = -1
}

// Row metadata to map DLX rows back to Sudoku
case class RowInfo(row: Int, col: Int, num: Int)

object DLX {
  var iterations = 0
  var puzzle: Array[Array[Int]] = Array.ofDim[Int](9, 9)

  // DLX matrix structures
  var root: DlxColumn = null
  val columns = new Array[DlxColumn](324)
  val rowInfo = new Array[RowInfo](729)
  val rowStarts = new Array[DlxNode](729)
  val nodes = new Array[DlxNode](729 * 4)  // Pre-allocate node pool
  var nodeCount = 0

  def main(args: Array[String]): Unit = {
    val startTime = System.nanoTime()

    for (arg <- args if arg.endsWith(".matrix")) {
      // Reset puzzle
      puzzle = Array.ofDim[Int](9, 9)

      readMatrixFile(arg)
      printPuzzle(puzzle)

      // Initialize DLX matrix
      initDlxMatrix()
      buildDlxMatrix()
      coverClues()

      // Solve
      iterations = 0
      val solution = new Array[Int](81)
      val solved = search(root, 0, solution)

      if (solved) {
        val solutionGrid = extractSolution(solution)
        printPuzzle(solutionGrid)
        println(s"\nSolved in Iterations=$iterations\n")
      } else {
        println("\nNo solution found\n")
      }
    }

    val elapsed = (System.nanoTime() - startTime) / 1000000000.0
    println(f"Seconds to process $elapsed%.3f")
  }

  // Read matrix file
  def readMatrixFile(filename: String): Unit = {
    // Normalize path for output
    var displayPath = filename
    if (filename.startsWith("/app/Matrices/")) {
      displayPath = "../" + filename.substring(5)
    }
    println(displayPath)

    val source = Source.fromFile(filename)
    var lineCount = 0

    try {
      for (line <- source.getLines() if lineCount < 9) {
        val trimmed = line.trim
        if (trimmed.nonEmpty && !trimmed.startsWith("#")) {
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

  // Print puzzle
  def printPuzzle(grid: Array[Array[Int]]): Unit = {
    println("\nPuzzle:")
    for (r <- 0 until 9) {
      val sb = new StringBuilder
      for (c <- 0 until 9) {
        sb.append(grid(r)(c)).append(" ")
      }
      println(sb.toString())
    }
  }

  // Calculate constraint column indices
  def getPositionCol(r: Int, c: Int): Int = r * 9 + c
  def getRowCol(r: Int, n: Int): Int = 81 + r * 9 + (n - 1)
  def getColCol(c: Int, n: Int): Int = 162 + c * 9 + (n - 1)
  def getBoxCol(r: Int, c: Int, n: Int): Int = {
    val box = (r / 3) * 3 + (c / 3)
    243 + box * 9 + (n - 1)
  }

  // Initialize DLX matrix structure
  def initDlxMatrix(): Unit = {
    // Create root column
    root = new DlxColumn("root")

    // Create 324 column headers
    for (i <- 0 until 324) {
      columns(i) = new DlxColumn(s"C$i")

      // Link into header list
      columns(i).left = root.left
      columns(i).right = root
      root.left.right = columns(i)
      root.left = columns(i)
    }

    nodeCount = 0
  }

  // Add a node to the DLX matrix
  def addNode(col: DlxColumn, rowId: Int): DlxNode = {
    val node = new DlxNode()
    nodes(nodeCount) = node
    nodeCount += 1

    node.column = col
    node.rowId = rowId

    // Insert at end of column's circular list
    node.down = col
    node.up = col.up
    col.up.down = node
    col.up = node
    col.size += 1

    node
  }

  // Build a DLX row for Sudoku cell (r,c) with value n
  def buildDlxRow(r: Int, c: Int, n: Int, rowId: Int): Unit = {
    // Store row metadata
    rowInfo(rowId) = RowInfo(r, c, n)

    // Create nodes for the 4 constraints
    val n1 = addNode(columns(getPositionCol(r, c)), rowId)
    val n2 = addNode(columns(getRowCol(r, n)), rowId)
    val n3 = addNode(columns(getColCol(c, n)), rowId)
    val n4 = addNode(columns(getBoxCol(r, c, n)), rowId)

    // Link nodes horizontally in circular list
    n1.right = n2
    n2.right = n3
    n3.right = n4
    n4.right = n1

    n1.left = n4
    n2.left = n1
    n3.left = n2
    n4.left = n3

    // Store first node for this row
    rowStarts(rowId) = n1
  }

  // Build the complete DLX matrix from the puzzle
  def buildDlxMatrix(): Unit = {
    var rowId = 0

    for (r <- 0 until 9; c <- 0 until 9) {
      if (puzzle(r)(c) != 0) {
        // Cell has a clue - create only one row
        buildDlxRow(r, c, puzzle(r)(c), rowId)
        rowId += 1
      } else {
        // Cell is empty - create rows for all possible values
        for (n <- 1 to 9) {
          buildDlxRow(r, c, n, rowId)
          rowId += 1
        }
      }
    }
  }

  // Cover a column in the DLX matrix
  def coverColumn(c: DlxColumn): Unit = {
    // Remove column header from the header list
    c.right.left = c.left
    c.left.right = c.right

    // For each row in this column
    var rowNode = c.down
    while (rowNode != c) {
      // For each node in this row
      var rightNode = rowNode.right
      while (rightNode != rowNode) {
        // Remove this node from its column
        rightNode.down.up = rightNode.up
        rightNode.up.down = rightNode.down
        rightNode.column.size -= 1
        rightNode = rightNode.right
      }
      rowNode = rowNode.down
    }
  }

  // Uncover a column (exact reverse of cover)
  def uncoverColumn(c: DlxColumn): Unit = {
    // For each row in this column (in reverse order)
    var rowNode = c.up
    while (rowNode != c) {
      // For each node in this row (in reverse order)
      var leftNode = rowNode.left
      while (leftNode != rowNode) {
        // Restore this node to its column
        leftNode.column.size += 1
        leftNode.down.up = leftNode
        leftNode.up.down = leftNode
        leftNode = leftNode.left
      }
      rowNode = rowNode.up
    }

    // Restore column header to the header list
    c.right.left = c
    c.left.right = c
  }

  // Choose column with minimum size (Knuth's S heuristic)
  def chooseColumn(): DlxColumn = {
    var best: DlxColumn = null
    var minSize = Int.MaxValue

    var colNode = root.right
    while (colNode != root) {
      val col = colNode.asInstanceOf[DlxColumn]
      if (col.size < minSize) {
        minSize = col.size
        best = col
      }
      colNode = colNode.right
    }

    best
  }

  // DLX Search - Algorithm X with Dancing Links
  def search(r: DlxColumn, k: Int, solution: Array[Int]): Boolean = {
    iterations += 1  // Count every search call

    // If matrix is empty, we found a solution
    if (r.right == r) {
      return true
    }

    // Choose column with minimum size
    val col = chooseColumn()

    // If column has no rows, no solution possible
    if (col.size == 0) {
      return false
    }

    // Cover this column
    coverColumn(col)

    // Try each row in this column
    var rowNode = col.down
    while (rowNode != col) {
      // Add row to partial solution
      solution(k) = rowNode.rowId

      // Cover all other columns in this row
      var rightNode = rowNode.right
      while (rightNode != rowNode) {
        coverColumn(rightNode.column)
        rightNode = rightNode.right
      }

      // Recurse
      if (search(r, k + 1, solution)) {
        return true
      }

      // Backtrack: uncover all columns in this row
      var leftNode = rowNode.left
      while (leftNode != rowNode) {
        uncoverColumn(leftNode.column)
        leftNode = leftNode.left
      }

      rowNode = rowNode.down
    }

    // Uncover column
    uncoverColumn(col)

    false
  }

  // Cover given clues (pre-selected rows)
  def coverClues(): Unit = {
    for (r <- 0 until 9; c <- 0 until 9) {
      if (puzzle(r)(c) != 0) {
        val n = puzzle(r)(c)

        // Find the row for this clue
        for (rowId <- 0 until 729) {
          if (rowStarts(rowId) != null &&
              rowInfo(rowId).row == r &&
              rowInfo(rowId).col == c &&
              rowInfo(rowId).num == n) {

            // Cover all columns in this row
            val node = rowStarts(rowId)
            var curr = node
            coverColumn(curr.column)
            curr = curr.right
            while (curr != node) {
              coverColumn(curr.column)
              curr = curr.right
            }
          }
        }
      }
    }
  }

  // Extract solution from DLX and populate solution grid
  def extractSolution(solution: Array[Int]): Array[Array[Int]] = {
    val solutionGrid = Array.ofDim[Int](9, 9)

    // Start with original puzzle (includes clues)
    for (r <- 0 until 9; c <- 0 until 9) {
      solutionGrid(r)(c) = puzzle(r)(c)
    }

    // Each solution entry is a row_id
    for (i <- 0 until 81) {
      val rowId = solution(i)
      if (rowId >= 0 && rowId < 729 && rowInfo(rowId) != null) {
        val info = rowInfo(rowId)
        solutionGrid(info.row)(info.col) = info.num
      }
    }

    solutionGrid
  }
}
