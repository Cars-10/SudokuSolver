import "os" for Process
import "io" for File

// CP (Constraint Propagation) Solver for Sudoku
class CPSolver {
    construct new() {
        _gridValues = List.filled(9, null)
        _gridCandidates = List.filled(9, null)
        for (i in 0...9) {
            _gridValues[i] = List.filled(9, 0)
            _gridCandidates[i] = List.filled(9, 0)
        }
        _iterations = 0
    }

    iterations { _iterations }

    // Bitwise operations
    hasCandidate(set, digit) { (set & (1 << digit)) != 0 }
    removeCandidate(row, col, digit) { _gridCandidates[row][col] = _gridCandidates[row][col] & ~(1 << digit) }
    addCandidate(row, col, digit) { _gridCandidates[row][col] = _gridCandidates[row][col] | (1 << digit) }

    countCandidates(set) {
        var count = 0
        for (digit in 1..9) {
            if (hasCandidate(set, digit)) {
                count = count + 1
            }
        }
        return count
    }

    getFirstCandidate(set) {
        for (digit in 1..9) {
            if (hasCandidate(set, digit)) {
                return digit
            }
        }
        return 0
    }

    // Get all 20 peers for a cell (row, col, box)
    getPeers(row, col) {
        var peers = []

        // Same row (9 cells minus self = 8)
        for (c in 0...9) {
            if (c != col) {
                peers.add([row, c])
            }
        }

        // Same column (9 cells minus self = 8)
        for (r in 0...9) {
            if (r != row) {
                peers.add([r, col])
            }
        }

        // Same 3x3 box (9 cells minus self minus already counted = 4)
        var boxRow = (row / 3).floor * 3
        var boxCol = (col / 3).floor * 3
        for (r in boxRow...boxRow + 3) {
            for (c in boxCol...boxCol + 3) {
                if (r != row && c != col) {
                    peers.add([r, c])
                }
            }
        }

        return peers
    }

    // Initialize grid from puzzle
    initGrid(puzzle) {
        for (row in 0...9) {
            for (col in 0...9) {
                if (puzzle[row][col] == 0) {
                    // Empty cell: set all candidates 1-9 (bits 1-9 set)
                    _gridValues[row][col] = 0
                    _gridCandidates[row][col] = 0x3FE  // Binary: 0011 1111 1110 (bits 1-9)
                } else {
                    // Given clue: set single value
                    var digit = puzzle[row][col]
                    _gridValues[row][col] = digit
                    _gridCandidates[row][col] = (1 << digit)
                }
            }
        }
    }

    // Eliminate a digit from a cell's candidates
    eliminate(row, col, digit) {
        // Check if digit is already eliminated
        if (!hasCandidate(_gridCandidates[row][col], digit)) {
            return true  // Already eliminated, no change
        }

        // Remove digit from candidates
        removeCandidate(row, col, digit)

        // Check for contradiction (no candidates left)
        var remaining = countCandidates(_gridCandidates[row][col])
        if (remaining == 0) {
            return false  // Contradiction
        }

        // If only one candidate left, assign it (singleton elimination)
        if (remaining == 1 && _gridValues[row][col] == 0) {
            var lastDigit = getFirstCandidate(_gridCandidates[row][col])
            if (!assign(row, col, lastDigit)) {
                return false  // Assignment caused contradiction
            }
        }

        return true
    }

    // Assign a digit to a cell
    assign(row, col, digit) {
        // Increment iteration counter (this is our benchmark metric)
        _iterations = _iterations + 1

        // Set value
        _gridValues[row][col] = digit
        _gridCandidates[row][col] = (1 << digit)

        // Eliminate digit from all peers
        var peers = getPeers(row, col)

        for (peer in peers) {
            var peerRow = peer[0]
            var peerCol = peer[1]

            if (!eliminate(peerRow, peerCol, digit)) {
                return false  // Contradiction in peer elimination
            }
        }

        return true
    }

    // Propagate constraints
    propagate() {
        var changed = true

        while (changed) {
            changed = false

            // Strategy 1: Singleton elimination
            // If a cell has only one candidate, assign it
            for (row in 0...9) {
                for (col in 0...9) {
                    if (_gridValues[row][col] == 0) {
                        var numCandidates = countCandidates(_gridCandidates[row][col])
                        if (numCandidates == 0) {
                            return false  // Contradiction
                        }
                        if (numCandidates == 1) {
                            var digit = getFirstCandidate(_gridCandidates[row][col])
                            if (!assign(row, col, digit)) {
                                return false  // Assignment caused contradiction
                            }
                            changed = true
                        }
                    }
                }
            }

            // Strategy 2: Hidden singles
            // For each unit (row, col, box), if a digit appears in only one cell, assign it

            // Check rows
            for (row in 0...9) {
                for (digit in 1..9) {
                    var count = 0
                    var lastCol = -1
                    var alreadyAssigned = false

                    for (col in 0...9) {
                        if (_gridValues[row][col] == digit) {
                            alreadyAssigned = true
                            break
                        }
                        if (hasCandidate(_gridCandidates[row][col], digit)) {
                            count = count + 1
                            lastCol = col
                        }
                    }

                    if (!alreadyAssigned) {
                        if (count == 1) {
                            if (!assign(row, lastCol, digit)) {
                                return false
                            }
                            changed = true
                        } else if (count == 0) {
                            return false  // Digit cannot be placed anywhere in row
                        }
                    }
                }
            }

            // Check columns
            for (col in 0...9) {
                for (digit in 1..9) {
                    var count = 0
                    var lastRow = -1
                    var alreadyAssigned = false

                    for (row in 0...9) {
                        if (_gridValues[row][col] == digit) {
                            alreadyAssigned = true
                            break
                        }
                        if (hasCandidate(_gridCandidates[row][col], digit)) {
                            count = count + 1
                            lastRow = row
                        }
                    }

                    if (!alreadyAssigned) {
                        if (count == 1) {
                            if (!assign(lastRow, col, digit)) {
                                return false
                            }
                            changed = true
                        } else if (count == 0) {
                            return false  // Digit cannot be placed anywhere in column
                        }
                    }
                }
            }

            // Check boxes
            for (box in 0...9) {
                var boxRow = (box / 3).floor * 3
                var boxCol = (box % 3) * 3

                for (digit in 1..9) {
                    var count = 0
                    var lastR = -1
                    var lastC = -1
                    var alreadyAssigned = false

                    for (r in boxRow...boxRow + 3) {
                        for (c in boxCol...boxCol + 3) {
                            if (_gridValues[r][c] == digit) {
                                alreadyAssigned = true
                                break
                            }
                            if (hasCandidate(_gridCandidates[r][c], digit)) {
                                count = count + 1
                                lastR = r
                                lastC = c
                            }
                        }
                        if (alreadyAssigned) break
                    }

                    if (!alreadyAssigned) {
                        if (count == 1) {
                            if (!assign(lastR, lastC, digit)) {
                                return false
                            }
                            changed = true
                        } else if (count == 0) {
                            return false  // Digit cannot be placed anywhere in box
                        }
                    }
                }
            }
        }

        return true  // Success - reached fixpoint
    }

    // Find cell with minimum remaining values (MRV heuristic)
    findMrvCell() {
        var minCandidates = 10  // More than 9, so any cell will be smaller
        var mrvRow = -1
        var mrvCol = -1

        for (r in 0...9) {
            for (c in 0...9) {
                if (_gridValues[r][c] == 0) {
                    var numCandidates = countCandidates(_gridCandidates[r][c])
                    if (numCandidates < minCandidates) {
                        minCandidates = numCandidates
                        mrvRow = r
                        mrvCol = c
                    }
                }
            }
        }

        if (mrvRow == -1) {
            return null  // No empty cells - grid is complete
        }

        return [mrvRow, mrvCol]
    }

    // Deep copy grid state for backtracking
    copyGridState() {
        var state = {
            "values": [],
            "candidates": []
        }

        for (i in 0...9) {
            var valRow = []
            var candRow = []
            for (j in 0...9) {
                valRow.add(_gridValues[i][j])
                candRow.add(_gridCandidates[i][j])
            }
            state["values"].add(valRow)
            state["candidates"].add(candRow)
        }

        return state
    }

    // Restore grid state from backup
    restoreGridState(state) {
        for (i in 0...9) {
            for (j in 0...9) {
                _gridValues[i][j] = state["values"][i][j]
                _gridCandidates[i][j] = state["candidates"][i][j]
            }
        }
    }

    // Recursive search with constraint propagation
    search() {
        // Base case: check if grid is complete
        var mrvCell = findMrvCell()
        if (mrvCell == null) {
            return true  // Grid is complete - solved!
        }

        var mrvRow = mrvCell[0]
        var mrvCol = mrvCell[1]

        // Recursive case: try each candidate for the MRV cell
        var candidates = _gridCandidates[mrvRow][mrvCol]

        for (digit in 1..9) {
            if (hasCandidate(candidates, digit)) {
                // Save grid state for backtracking
                var gridCopy = copyGridState()

                // Try assigning this digit
                if (assign(mrvRow, mrvCol, digit)) {
                    // Assignment succeeded, propagate constraints
                    if (propagate()) {
                        // Propagation succeeded, recurse
                        if (search()) {
                            return true  // Found solution
                        }
                    }
                }

                // Failed - restore grid state and try next candidate
                restoreGridState(gridCopy)
            }
        }

        // All candidates exhausted - dead end
        return false
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

    // Main solve method
    solve(filename) {
        var puzzle = readMatrixFile(filename)
        printGrid(puzzle)

        // Initialize grid
        initGrid(puzzle)

        // Propagate initial constraints
        if (!propagate()) {
            System.print("\nInitial propagation failed\n")
            return false
        }

        // Search for solution
        if (search()) {
            printGrid(_gridValues)
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
    var solver = CPSolver.new()
    solver.solve(arg)
}
var end = System.clock
System.print("Seconds to process %(end - start)")
