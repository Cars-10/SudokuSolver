import groovy.transform.Field

// ============================================================================
// NODE STRUCTURES
// ============================================================================

class DlxNode {
    DlxNode left, right, up, down
    DlxColumn column
    int rowId
}

class DlxColumn extends DlxNode {
    String name
    int size = 0
}

// ============================================================================
// GLOBAL STATE
// ============================================================================

@Field int dlxIterations = 0
@Field int[][] puzzle = new int[9][9]
@Field int[][] solutionGrid = new int[9][9]
@Field DlxColumn root
@Field DlxColumn[] columns = new DlxColumn[324]
@Field List<DlxNode> nodePool = []
@Field Map<Integer, RowInfo> rowInfo = [:]

class RowInfo {
    int row, col, num
}

// ============================================================================
// DLX CORE ALGORITHM
// ============================================================================

void coverColumn(DlxColumn c) {
    DlxNode colNode = c

    // Remove column header from header list
    colNode.right.left = colNode.left
    colNode.left.right = colNode.right

    // For each row in this column
    DlxNode rowNode = colNode.down
    while (rowNode != colNode) {
        // For each node in this row
        DlxNode rightNode = rowNode.right
        while (rightNode != rowNode) {
            // Remove this node from its column
            rightNode.down.up = rightNode.up
            rightNode.up.down = rightNode.down
            rightNode.column.size--
            rightNode = rightNode.right
        }
        rowNode = rowNode.down
    }
}

void uncoverColumn(DlxColumn c) {
    DlxNode colNode = c

    // For each row in this column (in reverse order)
    DlxNode rowNode = colNode.up
    while (rowNode != colNode) {
        // For each node in this row (in reverse order)
        DlxNode leftNode = rowNode.left
        while (leftNode != rowNode) {
            // Restore this node to its column
            leftNode.column.size++
            leftNode.down.up = leftNode
            leftNode.up.down = leftNode
            leftNode = leftNode.left
        }
        rowNode = rowNode.up
    }

    // Restore column header to header list
    colNode.right.left = colNode
    colNode.left.right = colNode
}

DlxColumn chooseColumn() {
    DlxColumn best = null
    int minSize = Integer.MAX_VALUE

    DlxNode colNode = root.right
    while (colNode != root) {
        DlxColumn col = (DlxColumn)colNode
        if (col.size < minSize) {
            minSize = col.size
            best = col
        }
        colNode = colNode.right
    }

    return best
}

boolean search(int k, List<Integer> solution) {
    dlxIterations++  // Count every search call

    // If matrix is empty, we found a solution
    if (root.right == root) {
        return true
    }

    // Choose column with minimum size
    DlxColumn col = chooseColumn()

    // If column has no rows, no solution possible
    if (col.size == 0) {
        return false
    }

    // Cover this column
    coverColumn(col)

    // Try each row in this column
    DlxNode rowNode = col.down
    while (rowNode != col) {
        // Add row to partial solution
        solution[k] = rowNode.rowId

        // Cover all other columns in this row
        DlxNode rightNode = rowNode.right
        while (rightNode != rowNode) {
            coverColumn(rightNode.column)
            rightNode = rightNode.right
        }

        // Recurse
        if (search(k + 1, solution)) {
            return true  // Solution found
        }

        // Backtrack: uncover all columns in this row
        DlxNode leftNode = rowNode.left
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

// ============================================================================
// CONSTRAINT COLUMN CALCULATION
// ============================================================================

int getPositionCol(int r, int c) {
    return r * 9 + c
}

int getRowCol(int r, int n) {
    return 81 + r * 9 + (n - 1)
}

int getColCol(int c, int n) {
    return 162 + c * 9 + (n - 1)
}

int getBoxCol(int r, int c, int n) {
    int box = (int)(r / 3) * 3 + (int)(c / 3)
    return 243 + box * 9 + (n - 1)
}

// ============================================================================
// DLX MATRIX CONSTRUCTION
// ============================================================================

void initDlxMatrix() {
    // Create root column
    root = new DlxColumn(name: "root", size: 0)
    root.left = root
    root.right = root
    root.up = root
    root.down = root
    root.column = root
    root.rowId = -1

    // Create 324 column headers
    for (int i = 0; i < 324; i++) {
        columns[i] = new DlxColumn(name: "C${i}", size: 0)
        columns[i].up = columns[i]
        columns[i].down = columns[i]
        columns[i].column = columns[i]
        columns[i].rowId = -1

        // Link into header list
        columns[i].left = root.left
        columns[i].right = root
        root.left.right = columns[i]
        root.left = columns[i]
    }
}

DlxNode addNode(DlxColumn col, int rowId) {
    DlxNode node = new DlxNode(column: col, rowId: rowId)

    // Insert at end of column's circular list
    node.down = col
    node.up = col.up
    col.up.down = node
    col.up = node
    col.size++

    nodePool.add(node)
    return node
}

void buildDlxRow(int r, int c, int n, int rowId) {
    // Store row metadata
    rowInfo[rowId] = new RowInfo(row: r, col: c, num: n)

    // Create nodes for the 4 constraints
    DlxNode n1 = addNode(columns[getPositionCol(r, c)], rowId)
    DlxNode n2 = addNode(columns[getRowCol(r, n)], rowId)
    DlxNode n3 = addNode(columns[getColCol(c, n)], rowId)
    DlxNode n4 = addNode(columns[getBoxCol(r, c, n)], rowId)

    // Link nodes horizontally in circular list
    n1.right = n2
    n2.right = n3
    n3.right = n4
    n4.right = n1

    n1.left = n4
    n2.left = n1
    n3.left = n2
    n4.left = n3
}

void buildDlxMatrixFromPuzzle() {
    int rowId = 0

    for (int r = 0; r < 9; r++) {
        for (int c = 0; c < 9; c++) {
            if (puzzle[r][c] != 0) {
                // Cell has a clue - create only one row for that value
                buildDlxRow(r, c, puzzle[r][c], rowId++)
            } else {
                // Cell is empty - create rows for all possible values
                for (int n = 1; n <= 9; n++) {
                    buildDlxRow(r, c, n, rowId++)
                }
            }
        }
    }
}

void coverClues() {
    for (int r = 0; r < 9; r++) {
        for (int c = 0; c < 9; c++) {
            if (puzzle[r][c] != 0) {
                int n = puzzle[r][c]

                // Find the row for this clue and cover its columns
                for (Map.Entry<Integer, RowInfo> entry : rowInfo.entrySet()) {
                    RowInfo info = entry.value
                    if (info.row == r && info.col == c && info.num == n) {
                        // Find first node in this row
                        DlxNode node = nodePool.find { it.rowId == entry.key }
                        if (node) {
                            DlxNode curr = node
                            do {
                                coverColumn(curr.column)
                                curr = curr.right
                            } while (curr != node)
                        }
                        break
                    }
                }
            }
        }
    }
}

// ============================================================================
// SOLUTION EXTRACTION
// ============================================================================

void extractSolution(List<Integer> solution) {
    // Initialize solution grid with original puzzle
    for (int r = 0; r < 9; r++) {
        for (int c = 0; c < 9; c++) {
            solutionGrid[r][c] = puzzle[r][c]
        }
    }

    // Fill in solution values
    for (int rowId : solution) {
        if (rowId >= 0 && rowInfo.containsKey(rowId)) {
            RowInfo info = rowInfo[rowId]
            solutionGrid[info.row][info.col] = info.num
        }
    }
}

// ============================================================================
// PUZZLE I/O
// ============================================================================

void printPuzzle(int[][] grid) {
    println "\nPuzzle:"
    for (int r = 0; r < 9; r++) {
        for (int c = 0; c < 9; c++) {
            print "${grid[r][c]} "
        }
        println ""
    }
}

int readMatrixFile(String filename) {
    def file = new File(filename)
    if (!file.exists()) {
        System.err.println "Error opening file '${filename}'"
        return 1
    }

    // Normalize path for output
    if (filename.startsWith("/app/Matrices/")) {
        println "../${filename.substring(5)}"
    } else {
        println filename
    }

    int lineCount = 0
    file.eachLine { line ->
        String lineStr = line.trim()

        // Skip comments and empty lines
        if (lineStr.isEmpty() || lineStr.startsWith('#')) {
            return  // continue in Groovy closure
        }

        if (lineCount >= 9) return

        // Parse 9 integers from line
        def parts = lineStr.split(/\s+/)
        if (parts.length >= 9) {
            for (int i = 0; i < 9; i++) {
                puzzle[lineCount][i] = parts[i] as int
                print "${puzzle[lineCount][i]} "
            }
            println ""
            lineCount++
        }
    }

    return 0
}

// ============================================================================
// MAIN
// ============================================================================

long startTime = System.nanoTime()

// Process each .matrix file from command line
args.each { filename ->
    if (filename.endsWith('.matrix')) {
        if (readMatrixFile(filename) != 0) {
            System.err.println "Error reading ${filename}"
            return
        }

        printPuzzle(puzzle)

        // Initialize DLX matrix
        initDlxMatrix()

        // Build matrix from puzzle
        buildDlxMatrixFromPuzzle()

        // Cover pre-filled clues
        coverClues()

        // Solve using DLX
        dlxIterations = 0
        List<Integer> solution = new ArrayList<>(81)
        for (int i = 0; i < 81; i++) {
            solution.add(0)
        }

        boolean result = search(0, solution)

        if (result) {
            extractSolution(solution)
            printPuzzle(solutionGrid)
            println "\nSolved in Iterations=${dlxIterations}\n"
        } else {
            println "\nNo solution found"
        }
    }
}

long elapsed = System.nanoTime() - startTime
printf "Seconds to process %.3f%n", elapsed / 1_000_000_000.0
