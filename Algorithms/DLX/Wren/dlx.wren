import "os" for Process
import "io" for File

// DLX Node class - uses reference semantics for circular linked structure
class DlxNode {
    construct new() {
        _left = null
        _right = null
        _up = null
        _down = null
        _column = null
        _size = 0
        _rowId = -1
        _name = ""
    }

    left { _left }
    left=(value) { _left = value }

    right { _right }
    right=(value) { _right = value }

    up { _up }
    up=(value) { _up = value }

    down { _down }
    down=(value) { _down = value }

    column { _column }
    column=(value) { _column = value }

    size { _size }
    size=(value) { _size = value }

    rowId { _rowId }
    rowId=(value) { _rowId = value }

    name { _name }
    name=(value) { _name = value }
}

// DLX Solver class
class DlxSolver {
    construct new() {
        _iterations = 0
        _root = null
        _solution = []
        _columns = []
        _rowStarts = {}
        _rowInfo = {}
    }

    iterations { _iterations }

    // Cover a column in the DLX matrix
    coverColumn(col) {
        // Remove column header from the header list
        col.right.left = col.left
        col.left.right = col.right

        // For each row in this column
        var rowNode = col.down
        while (rowNode != col) {
            // For each node in this row (excluding the column itself)
            var rightNode = rowNode.right
            while (rightNode != rowNode) {
                // Remove this node from its column
                rightNode.down.up = rightNode.up
                rightNode.up.down = rightNode.down
                rightNode.column.size = rightNode.column.size - 1
                rightNode = rightNode.right
            }
            rowNode = rowNode.down
        }
    }

    // Uncover a column (exact reverse of cover)
    uncoverColumn(col) {
        // For each row in this column (in reverse order)
        var rowNode = col.up
        while (rowNode != col) {
            // For each node in this row (in reverse order)
            var leftNode = rowNode.left
            while (leftNode != rowNode) {
                // Restore this node to its column
                leftNode.column.size = leftNode.column.size + 1
                leftNode.down.up = leftNode
                leftNode.up.down = leftNode
                leftNode = leftNode.left
            }
            rowNode = rowNode.up
        }

        // Restore column header to the header list
        col.right.left = col
        col.left.right = col
    }

    // Choose column with minimum size (Knuth's S heuristic)
    chooseColumn() {
        var best = null
        var minSize = 999999

        var colNode = _root.right
        while (colNode != _root) {
            if (colNode.size < minSize) {
                minSize = colNode.size
                best = colNode
            }
            colNode = colNode.right
        }

        return best
    }

    // DLX Search - Algorithm X with Dancing Links
    search(k) {
        _iterations = _iterations + 1  // Count every search call

        // If matrix is empty, we found a solution
        if (_root.right == _root) {
            return true
        }

        // Choose column with minimum size
        var col = chooseColumn()

        // If column has no rows, no solution possible
        if (col == null || col.size == 0) {
            return false
        }

        // Cover this column
        coverColumn(col)

        // Try each row in this column
        var rowNode = col.down
        while (rowNode != col) {
            // Add row to partial solution
            _solution.add(rowNode.rowId)

            // Cover all other columns in this row
            var rightNode = rowNode.right
            while (rightNode != rowNode) {
                coverColumn(rightNode.column)
                rightNode = rightNode.right
            }

            // Recurse
            if (search(k + 1)) {
                return true  // Solution found
            }

            // Backtrack: remove row from solution
            _solution.removeAt(_solution.count - 1)

            // Uncover all columns in this row
            var leftNode = rowNode.left
            while (leftNode != rowNode) {
                uncoverColumn(leftNode.column)
                leftNode = leftNode.left
            }

            rowNode = rowNode.down
        }

        // Uncover column
        uncoverColumn(col)

        return false  // No solution found
    }

    // Build DLX matrix for Sudoku
    buildMatrix(puzzle) {
        // Create root node
        _root = DlxNode.new()
        _root.left = _root
        _root.right = _root
        _root.up = _root
        _root.down = _root

        // Create 324 column headers (81 cell + 81 row + 81 col + 81 box constraints)
        _columns = []
        for (i in 0...324) {
            var col = DlxNode.new()
            col.size = 0
            col.up = col
            col.down = col
            col.column = col
            col.name = "C%(i)"

            // Link to header list
            col.left = _root.left
            col.right = _root
            _root.left.right = col
            _root.left = col

            _columns.add(col)
        }

        // Track rows for clue covering
        _rowStarts = {}
        _rowInfo = {}

        // For each cell (row, col) and digit (1-9)
        var rowId = 0
        for (row in 0...9) {
            for (col in 0...9) {
                if (puzzle[row][col] != 0) {
                    // Cell has a clue - create only one row for that value
                    buildDlxRow(row, col, puzzle[row][col], rowId)
                    rowId = rowId + 1
                } else {
                    // Cell is empty - create rows for all possible values
                    for (digit in 1..9) {
                        buildDlxRow(row, col, digit, rowId)
                        rowId = rowId + 1
                    }
                }
            }
        }
    }

    // Build a single DLX row for cell (row, col) with value digit
    buildDlxRow(row, col, digit, rowId) {
        // Store row metadata
        _rowInfo[rowId] = {"row": row, "col": col, "digit": digit}

        // Constraint 1: Cell (row, col) has a value
        var cellIdx = row * 9 + col
        var node1 = DlxNode.new()
        node1.rowId = rowId
        node1.column = _columns[cellIdx]

        // Constraint 2: Row has digit
        var rowIdx = 81 + row * 9 + (digit - 1)
        var node2 = DlxNode.new()
        node2.rowId = rowId
        node2.column = _columns[rowIdx]

        // Constraint 3: Column has digit
        var colIdx = 162 + col * 9 + (digit - 1)
        var node3 = DlxNode.new()
        node3.rowId = rowId
        node3.column = _columns[colIdx]

        // Constraint 4: Box has digit
        var boxIdx = (row / 3).floor * 3 + (col / 3).floor
        var boxConstraintIdx = 243 + boxIdx * 9 + (digit - 1)
        var node4 = DlxNode.new()
        node4.rowId = rowId
        node4.column = _columns[boxConstraintIdx]

        // Link nodes horizontally (circular)
        node1.right = node2
        node2.right = node3
        node3.right = node4
        node4.right = node1

        node1.left = node4
        node2.left = node1
        node3.left = node2
        node4.left = node3

        // Link nodes vertically into their columns
        var nodes = [node1, node2, node3, node4]
        for (node in nodes) {
            node.up = node.column.up
            node.down = node.column
            node.column.up.down = node
            node.column.up = node
            node.column.size = node.column.size + 1
        }

        // Store first node for this row
        _rowStarts[rowId] = node1
    }

    // Cover given clues (pre-selected rows)
    coverClues(puzzle) {
        for (row in 0...9) {
            for (col in 0...9) {
                if (puzzle[row][col] != 0) {
                    var digit = puzzle[row][col]

                    // Find the row for this clue
                    for (rowId in _rowInfo.keys) {
                        var info = _rowInfo[rowId]
                        if (info["row"] == row && info["col"] == col && info["digit"] == digit) {
                            // Cover all columns in this row
                            var node = _rowStarts[rowId]
                            var curr = node
                            var first = true
                            while (first || curr != node) {
                                first = false
                                coverColumn(curr.column)
                                curr = curr.right
                            }
                            break
                        }
                    }
                }
            }
        }
    }

    // Convert solution to grid
    solutionToGrid(puzzle) {
        var grid = List.filled(9, null)
        for (i in 0...9) {
            grid[i] = List.filled(9, 0)
        }

        // Start with the puzzle (includes clues)
        for (row in 0...9) {
            for (col in 0...9) {
                grid[row][col] = puzzle[row][col]
            }
        }

        // Add solution cells
        for (rowId in _solution) {
            var info = _rowInfo[rowId]
            if (info != null) {
                grid[info["row"]][info["col"]] = info["digit"]
            }
        }

        return grid
    }

    // Print grid
    printGrid(grid) {
        System.print("\nPuzzle:")
        for (row in 0...9) {
            var line = ""
            for (col in 0...9) {
                line = line + grid[row][col].toString + " "
            }
            System.print(line)
        }
    }

    // Read matrix file
    readMatrixFile(filename) {
        var content = File.read(filename)
        var lines = content.split("\n")

        // Normalize path for output
        if (filename.startsWith("/app/Matrices/")) {
            System.print("../" + filename[5...filename.count])
        } else {
            System.print(filename)
        }

        var puzzle = List.filled(9, null)
        for (i in 0...9) {
            puzzle[i] = List.filled(9, 0)
        }

        var lineCount = 0
        for (line in lines) {
            line = line.trim()
            if (line == "" || line.startsWith("#")) continue

            var parts = line.split(" ").where { |s| s != "" }.toList
            if (parts.count == 9) {
                if (lineCount < 9) {
                    var outLine = ""
                    for (i in 0...9) {
                        var val = Num.fromString(parts[i])
                        puzzle[lineCount][i] = val
                        outLine = outLine + val.toString + " "
                    }
                    System.print(outLine)
                    lineCount = lineCount + 1
                }
            }
        }

        return puzzle
    }

    solve(filename) {
        var puzzle = readMatrixFile(filename)
        printGrid(puzzle)

        buildMatrix(puzzle)
        coverClues(puzzle)

        if (search(0)) {
            var solution = solutionToGrid(puzzle)
            printGrid(solution)
            System.print("\nSolved in Iterations=%(_iterations)\n")
            return true
        } else {
            System.print("\nNo solution found\n")
            return false
        }
    }
}

// Main execution
var start = System.clock
for (arg in Process.arguments) {
    var solver = DlxSolver.new()
    solver.solve(arg)
}
var end = System.clock
System.print("Seconds to process %(end - start)")
