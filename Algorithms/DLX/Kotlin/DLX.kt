/**
 * Sudoku Solver - DLX (Dancing Links) Implementation in Kotlin
 *
 * Algorithm X with Dancing Links - exact cover approach to Sudoku.
 * Maintains circular doubly-linked list structure for efficient constraint handling.
 *
 * Expected iterations for Matrix 1: 43
 */

import java.io.File

// DLX Node structure
open class DlxNode(
    var left: DlxNode? = null,
    var right: DlxNode? = null,
    var up: DlxNode? = null,
    var down: DlxNode? = null,
    var column: DlxColumn? = null,
    var rowId: Int = -1
)

// DLX Column header (inherits from node)
class DlxColumn(
    val name: String,
    var size: Int = 0
) : DlxNode()

// Row metadata to map DLX rows back to Sudoku (row, col, num)
data class RowInfo(val row: Int, val col: Int, val num: Int)

// Global variables
var iterations = 0
val puzzle = Array(9) { IntArray(9) }
val solutionGrid = Array(9) { IntArray(9) }
lateinit var root: DlxColumn
val columns = Array<DlxColumn?>(324) { null }
val nodes = mutableListOf<DlxNode>()
val rowInfo = Array<RowInfo?>(729) { null }
val rowStarts = Array<DlxNode?>(729) { null }

fun main(args: Array<String>) {
    val startTime = System.nanoTime()

    for (arg in args) {
        if (!arg.endsWith(".matrix")) continue

        // Reset puzzle
        for (i in 0 until 9) {
            for (j in 0 until 9) {
                puzzle[i][j] = 0
            }
        }

        readMatrixFile(arg)
        printPuzzle(puzzle)

        // Initialize DLX matrix
        initDlxMatrix()

        // Build matrix from puzzle
        buildDlxMatrixFromPuzzle()

        // Cover pre-filled clues
        coverClues()

        // Solve using DLX
        iterations = 0
        val solution = IntArray(81)
        val result = dlxSearch(root, 0, solution)

        if (result) {
            extractSolution(solution, 81)
            printPuzzle(solutionGrid)
            println("\nSolved in Iterations=$iterations\n")
        } else {
            println("\nNo solution found\n")
        }
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

fun printPuzzle(grid: Array<IntArray>) {
    println("\nPuzzle:")
    for (row in 0 until 9) {
        val sb = StringBuilder()
        for (col in 0 until 9) {
            sb.append(grid[row][col]).append(" ")
        }
        println(sb.toString())
    }
}

// Calculate constraint column indices
fun getPositionCol(r: Int, c: Int): Int = r * 9 + c
fun getRowCol(r: Int, n: Int): Int = 81 + r * 9 + (n - 1)
fun getColCol(c: Int, n: Int): Int = 162 + c * 9 + (n - 1)
fun getBoxCol(r: Int, c: Int, n: Int): Int {
    val box = (r / 3) * 3 + (c / 3)
    return 243 + box * 9 + (n - 1)
}

// Initialize DLX matrix structure
fun initDlxMatrix() {
    // Allocate root column
    root = DlxColumn("root")
    root.left = root
    root.right = root
    root.up = root
    root.down = root
    root.column = root

    // Allocate 324 column headers
    for (i in 0 until 324) {
        columns[i] = DlxColumn("C$i")
        val col = columns[i]!!

        // Initialize as circular list
        col.up = col
        col.down = col
        col.column = col

        // Link into header list
        col.left = root.left
        col.right = root
        root.left!!.right = col
        root.left = col
    }

    nodes.clear()
}

// Add a node to the DLX matrix
fun addNode(col: DlxColumn, rowId: Int): DlxNode {
    val node = DlxNode()
    node.column = col
    node.rowId = rowId

    // Insert at end of column's circular list
    node.down = col
    node.up = col.up
    col.up!!.down = node
    col.up = node
    col.size++

    nodes.add(node)
    return node
}

// Build a DLX row for Sudoku cell (r,c) with value n
fun buildDlxRow(r: Int, c: Int, n: Int, rowId: Int) {
    // Store row metadata
    rowInfo[rowId] = RowInfo(r, c, n)

    // Create nodes for the 4 constraints
    val n1 = addNode(columns[getPositionCol(r, c)]!!, rowId)
    val n2 = addNode(columns[getRowCol(r, n)]!!, rowId)
    val n3 = addNode(columns[getColCol(c, n)]!!, rowId)
    val n4 = addNode(columns[getBoxCol(r, c, n)]!!, rowId)

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
    rowStarts[rowId] = n1
}

// Build the complete DLX matrix from the puzzle
fun buildDlxMatrixFromPuzzle() {
    var rowId = 0

    for (r in 0 until 9) {
        for (c in 0 until 9) {
            if (puzzle[r][c] != 0) {
                // Cell has a clue - create only one row for that value
                buildDlxRow(r, c, puzzle[r][c], rowId++)
            } else {
                // Cell is empty - create rows for all possible values
                for (n in 1..9) {
                    buildDlxRow(r, c, n, rowId++)
                }
            }
        }
    }
}

// Cover a column in the DLX matrix
fun coverColumn(c: DlxColumn) {
    // Remove column header from the header list
    c.right!!.left = c.left
    c.left!!.right = c.right

    // For each row in this column
    var rowNode = c.down
    while (rowNode != c) {
        // For each node in this row (excluding the column itself)
        var rightNode = rowNode!!.right
        while (rightNode != rowNode) {
            // Remove this node from its column
            rightNode!!.down!!.up = rightNode.up
            rightNode.up!!.down = rightNode.down
            rightNode.column!!.size--
            rightNode = rightNode.right
        }
        rowNode = rowNode.down
    }
}

// Uncover a column (exact reverse of cover)
fun uncoverColumn(c: DlxColumn) {
    // For each row in this column (in reverse order)
    var rowNode = c.up
    while (rowNode != c) {
        // For each node in this row (in reverse order)
        var leftNode = rowNode!!.left
        while (leftNode != rowNode) {
            // Restore this node to its column
            leftNode!!.column!!.size++
            leftNode.down!!.up = leftNode
            leftNode.up!!.down = leftNode
            leftNode = leftNode.left
        }
        rowNode = rowNode.up
    }

    // Restore column header to the header list
    c.right!!.left = c
    c.left!!.right = c
}

// Choose column with minimum size (Knuth's S heuristic)
fun chooseColumn(root: DlxColumn): DlxColumn? {
    var best: DlxColumn? = null
    var minSize = Int.MAX_VALUE

    var colNode = root.right
    while (colNode != root) {
        val col = colNode as DlxColumn
        if (col.size < minSize) {
            minSize = col.size
            best = col
        }
        colNode = colNode.right
    }

    return best
}

// DLX Search - Algorithm X with Dancing Links
fun dlxSearch(root: DlxColumn, k: Int, solution: IntArray): Boolean {
    iterations++  // Count every search call (analogous to brute-force iterations)

    // If matrix is empty, we found a solution
    if (root.right == root) {
        return true
    }

    // Choose column with minimum size
    val col = chooseColumn(root) ?: return false

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
        solution[k] = rowNode!!.rowId

        // Cover all other columns in this row
        var rightNode = rowNode.right
        while (rightNode != rowNode) {
            coverColumn(rightNode!!.column!!)
            rightNode = rightNode.right
        }

        // Recurse
        if (dlxSearch(root, k + 1, solution)) {
            return true  // Solution found
        }

        // Backtrack: uncover all columns in this row
        var leftNode = rowNode.left
        while (leftNode != rowNode) {
            uncoverColumn(leftNode!!.column!!)
            leftNode = leftNode.left
        }

        rowNode = rowNode.down
    }

    // Uncover column
    uncoverColumn(col)

    return false  // No solution found
}

// Extract solution from DLX and populate solutionGrid
fun extractSolution(solution: IntArray, solutionLen: Int) {
    // Initialize solution grid - start with the original puzzle (includes clues)
    for (r in 0 until 9) {
        for (c in 0 until 9) {
            solutionGrid[r][c] = puzzle[r][c]
        }
    }

    // Each solution entry is a row_id
    for (i in 0 until solutionLen) {
        val rowId = solution[i]
        if (rowId >= 0 && rowId < 729) {
            val info = rowInfo[rowId]
            if (info != null) {
                solutionGrid[info.row][info.col] = info.num
            }
        }
    }
}

// Cover given clues (pre-selected rows)
fun coverClues() {
    for (r in 0 until 9) {
        for (c in 0 until 9) {
            if (puzzle[r][c] != 0) {
                val n = puzzle[r][c]

                // Find the row for this clue
                for (rowId in 0 until 729) {
                    val info = rowInfo[rowId]
                    if (rowStarts[rowId] != null &&
                        info != null &&
                        info.row == r &&
                        info.col == c &&
                        info.num == n) {

                        // Cover all columns in this row
                        val node = rowStarts[rowId]!!
                        var curr = node
                        do {
                            coverColumn(curr.column!!)
                            curr = curr.right!!
                        } while (curr != node)
                        break
                    }
                }
            }
        }
    }
}
