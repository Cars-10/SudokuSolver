import Foundation

// DLX Node class - uses reference semantics for circular linked structure
class DlxNode {
    var left: DlxNode?
    var right: DlxNode?
    var up: DlxNode?
    var down: DlxNode?
    var column: DlxNode?  // Points to column header
    var size: Int = 0     // Only used by column headers
    var rowId: Int = -1   // Row ID for solution tracking
    var name: String = ""

    init() {}
}

// Global iteration counter
var dlxIterations = 0

// Cover a column in the DLX matrix
func coverColumn(_ col: DlxNode) {
    // Remove column header from the header list
    col.right?.left = col.left
    col.left?.right = col.right

    // For each row in this column
    var rowNode = col.down
    while rowNode !== col {
        // For each node in this row (excluding the column itself)
        var rightNode = rowNode?.right
        while rightNode !== rowNode {
            // Remove this node from its column
            rightNode?.down?.up = rightNode?.up
            rightNode?.up?.down = rightNode?.down
            if let colNode = rightNode?.column {
                colNode.size -= 1
            }
            rightNode = rightNode?.right
        }
        rowNode = rowNode?.down
    }
}

// Uncover a column (exact reverse of cover)
func uncoverColumn(_ col: DlxNode) {
    // For each row in this column (in reverse order)
    var rowNode = col.up
    while rowNode !== col {
        // For each node in this row (in reverse order)
        var leftNode = rowNode?.left
        while leftNode !== rowNode {
            // Restore this node to its column
            if let colNode = leftNode?.column {
                colNode.size += 1
            }
            leftNode?.down?.up = leftNode
            leftNode?.up?.down = leftNode
            leftNode = leftNode?.left
        }
        rowNode = rowNode?.up
    }

    // Restore column header to the header list
    col.right?.left = col
    col.left?.right = col
}

// Choose column with minimum size (Knuth's S heuristic)
func chooseColumn(_ root: DlxNode) -> DlxNode? {
    var best: DlxNode? = nil
    var minSize = Int.max

    var colNode = root.right
    while colNode !== root {
        if let col = colNode {
            if col.size < minSize {
                minSize = col.size
                best = col
            }
        }
        colNode = colNode?.right
    }

    return best
}

// DLX Search - Algorithm X with Dancing Links
func dlxSearch(_ root: DlxNode, _ k: Int, _ solution: inout [Int]) -> Bool {
    dlxIterations += 1  // Count every search call

    // If matrix is empty, we found a solution
    if root.right === root {
        return true
    }

    // Choose column with minimum size
    guard let col = chooseColumn(root) else {
        return false
    }

    // If column has no rows, no solution possible
    if col.size == 0 {
        return false
    }

    // Cover this column
    coverColumn(col)

    // Try each row in this column
    var rowNode = col.down
    while rowNode !== col {
        if let row = rowNode {
            // Add row to partial solution
            solution[k] = row.rowId

            // Cover all other columns in this row
            var rightNode = row.right
            while rightNode !== row {
                if let rn = rightNode, let colTocover = rn.column {
                    coverColumn(colTocover)
                }
                rightNode = rightNode?.right
            }

            // Recurse
            if dlxSearch(root, k + 1, &solution) {
                return true  // Solution found
            }

            // Backtrack: uncover all columns in this row
            var leftNode = row.left
            while leftNode !== row {
                if let ln = leftNode, let colToUncover = ln.column {
                    uncoverColumn(colToUncover)
                }
                leftNode = leftNode?.left
            }
        }

        rowNode = rowNode?.down
    }

    // Uncover column
    uncoverColumn(col)

    return false  // No solution found
}

// Sudoku-specific structures
struct RowInfo {
    var row: Int
    var col: Int
    var num: Int
}

var puzzle = Array(repeating: Array(repeating: 0, count: 9), count: 9)
var rowInfo = Array(repeating: RowInfo(row: 0, col: 0, num: 0), count: 729)
var rowStarts = Array<DlxNode?>(repeating: nil, count: 729)

// Calculate constraint column indices
func getPositionCol(_ r: Int, _ c: Int) -> Int {
    return r * 9 + c
}

func getRowCol(_ r: Int, _ n: Int) -> Int {
    return 81 + r * 9 + (n - 1)
}

func getColCol(_ c: Int, _ n: Int) -> Int {
    return 162 + c * 9 + (n - 1)
}

func getBoxCol(_ r: Int, _ c: Int, _ n: Int) -> Int {
    let box = (r / 3) * 3 + (c / 3)
    return 243 + box * 9 + (n - 1)
}

// Add a node to the DLX matrix
func addNode(_ col: DlxNode, _ rowId: Int, _ allNodes: inout [DlxNode]) -> DlxNode {
    let node = DlxNode()
    allNodes.append(node)

    node.column = col
    node.rowId = rowId

    // Insert at end of column's circular list
    node.down = col
    node.up = col.up
    col.up?.down = node
    col.up = node
    col.size += 1

    return node
}

// Build a DLX row for Sudoku cell (r,c) with value n
func buildDlxRow(_ r: Int, _ c: Int, _ n: Int, _ rowId: Int,
                 _ columns: [DlxNode], _ allNodes: inout [DlxNode]) {
    // Store row metadata
    rowInfo[rowId] = RowInfo(row: r, col: c, num: n)

    // Create nodes for the 4 constraints
    let n1 = addNode(columns[getPositionCol(r, c)], rowId, &allNodes)
    let n2 = addNode(columns[getRowCol(r, n)], rowId, &allNodes)
    let n3 = addNode(columns[getColCol(c, n)], rowId, &allNodes)
    let n4 = addNode(columns[getBoxCol(r, c, n)], rowId, &allNodes)

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

// Initialize DLX matrix structure
func initDlxMatrix() -> (DlxNode, [DlxNode]) {
    // Create root column
    let root = DlxNode()
    root.name = "root"
    root.left = root
    root.right = root
    root.up = root
    root.down = root
    root.column = root
    root.rowId = -1
    root.size = 0

    // Create 324 column headers
    var columns: [DlxNode] = []
    for i in 0..<324 {
        let col = DlxNode()
        col.name = "C\(i)"
        col.size = 0
        col.up = col
        col.down = col
        col.column = col
        col.rowId = -1

        // Link into header list
        col.left = root.left
        col.right = root
        root.left?.right = col
        root.left = col

        columns.append(col)
    }

    return (root, columns)
}

// Build the complete DLX matrix from the puzzle
func buildDlxMatrixFromPuzzle(_ columns: [DlxNode], _ allNodes: inout [DlxNode]) {
    var rowId = 0

    for r in 0..<9 {
        for c in 0..<9 {
            if puzzle[r][c] != 0 {
                // Cell has a clue - create only one row for that value
                buildDlxRow(r, c, puzzle[r][c], rowId, columns, &allNodes)
                rowId += 1
            } else {
                // Cell is empty - create rows for all possible values
                for n in 1...9 {
                    buildDlxRow(r, c, n, rowId, columns, &allNodes)
                    rowId += 1
                }
            }
        }
    }
}

// Cover given clues (pre-selected rows)
func coverClues(_ root: DlxNode) {
    for r in 0..<9 {
        for c in 0..<9 {
            if puzzle[r][c] != 0 {
                let n = puzzle[r][c]

                // Find the row for this clue
                for rowId in 0..<729 {
                    if let node = rowStarts[rowId] {
                        if rowInfo[rowId].row == r &&
                           rowInfo[rowId].col == c &&
                           rowInfo[rowId].num == n {
                            // Cover all columns in this row
                            var curr = node
                            repeat {
                                if let colToCover = curr.column {
                                    coverColumn(colToCover)
                                }
                                curr = curr.right!
                            } while curr !== node
                            break
                        }
                    }
                }
            }
        }
    }
}

// Extract solution from DLX
func extractSolution(_ solution: [Int]) -> [[Int]] {
    var solutionGrid = puzzle

    for i in 0..<81 {
        let rowId = solution[i]
        if rowId >= 0 && rowId < 729 {
            solutionGrid[rowInfo[rowId].row][rowInfo[rowId].col] = rowInfo[rowId].num
        }
    }

    return solutionGrid
}

// Print puzzle
func printPuzzle(_ grid: [[Int]]) {
    print("\nPuzzle:")
    for r in 0..<9 {
        for c in 0..<9 {
            print("\(grid[r][c]) ", terminator: "")
        }
        print("")
    }
}

// Read matrix file
func readMatrixFile(_ filename: String) -> Bool {
    guard let content = try? String(contentsOfFile: filename, encoding: .utf8) else {
        print("Error reading file \(filename)")
        return false
    }

    // Normalize path for output
    let displayPath: String
    if filename.hasPrefix("/app/Matrices/") {
        displayPath = "../" + String(filename.dropFirst(5))
    } else {
        displayPath = filename
    }
    print(displayPath)

    let lines = content.components(separatedBy: .newlines)
    var row = 0

    for line in lines {
        if row >= 9 { break }

        let trimmed = line.trimmingCharacters(in: .whitespaces)
        if trimmed.isEmpty || trimmed.hasPrefix("#") { continue }

        let parts = trimmed.components(separatedBy: .whitespaces).filter { !$0.isEmpty }

        if parts.count >= 9 {
            for col in 0..<9 {
                if let val = Int(parts[col]) {
                    puzzle[row][col] = val
                    print("\(val) ", terminator: "")
                }
            }
            print("")
            row += 1
        }
    }

    return row == 9
}

// Main execution
let args = CommandLine.arguments
if args.count < 2 {
    print("Usage: dlx_solver <matrix_file>")
    exit(1)
}

let startTime = Date()

// Process each matrix file
for i in 1..<args.count {
    let filename = args[i]

    if filename.hasSuffix(".matrix") {
        // Reset state
        dlxIterations = 0
        puzzle = Array(repeating: Array(repeating: 0, count: 9), count: 9)
        rowInfo = Array(repeating: RowInfo(row: 0, col: 0, num: 0), count: 729)
        rowStarts = Array<DlxNode?>(repeating: nil, count: 729)

        if readMatrixFile(filename) {
            printPuzzle(puzzle)

            // Initialize DLX matrix
            let (root, columns) = initDlxMatrix()
            var allNodes: [DlxNode] = []

            // Build matrix from puzzle
            buildDlxMatrixFromPuzzle(columns, &allNodes)

            // Cover pre-filled clues
            coverClues(root)

            // Solve using DLX
            var solution = Array(repeating: -1, count: 81)
            let result = dlxSearch(root, 0, &solution)

            if result {
                let solutionGrid = extractSolution(solution)
                printPuzzle(solutionGrid)
                print("\nSolved in Iterations=\(dlxIterations)\n")
            } else {
                print("\nNo solution found")
            }
        }
    }
}

let endTime = Date()
let elapsed = endTime.timeIntervalSince(startTime)
print(String(format: "Seconds to process %.3f", elapsed))
