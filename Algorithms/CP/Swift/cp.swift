import Foundation

// Candidate tracking using bitsets (bits 1-9 represent digits)
typealias CandidateSet = Int

// Grid structure
var gridValues = Array(repeating: Array(repeating: 0, count: 9), count: 9)
var gridCandidates = Array(repeating: Array(repeating: 0, count: 9), count: 9)

// Global iteration counter
var cpIterations: Int64 = 0

// Bitwise operations
func hasCandidate(_ set: CandidateSet, _ digit: Int) -> Bool {
    return (set & (1 << digit)) != 0
}

func removeCandidate(_ set: inout CandidateSet, _ digit: Int) {
    set &= ~(1 << digit)
}

func addCandidate(_ set: inout CandidateSet, _ digit: Int) {
    set |= (1 << digit)
}

func countCandidates(_ set: CandidateSet) -> Int {
    return set.nonzeroBitCount
}

func getFirstCandidate(_ set: CandidateSet) -> Int {
    for digit in 1...9 {
        if hasCandidate(set, digit) {
            return digit
        }
    }
    return 0
}

// Get all 20 peers for a cell (row, col, box)
func getPeers(_ row: Int, _ col: Int) -> [(Int, Int)] {
    var peers: [(Int, Int)] = []

    // Same row (9 cells minus self = 8)
    for c in 0..<9 {
        if c != col {
            peers.append((row, c))
        }
    }

    // Same column (9 cells minus self = 8)
    for r in 0..<9 {
        if r != row {
            peers.append((r, col))
        }
    }

    // Same 3x3 box (9 cells minus self minus already counted = 4)
    let boxRow = (row / 3) * 3
    let boxCol = (col / 3) * 3
    for r in boxRow..<boxRow + 3 {
        for c in boxCol..<boxCol + 3 {
            if r != row && c != col {
                peers.append((r, c))
            }
        }
    }

    return peers
}

// Initialize grid from puzzle
func initGrid(_ puzzle: [[Int]]) {
    for row in 0..<9 {
        for col in 0..<9 {
            if puzzle[row][col] == 0 {
                // Empty cell: set all candidates 1-9 (bits 1-9 set)
                gridValues[row][col] = 0
                gridCandidates[row][col] = 0x3FE  // Binary: 0011 1111 1110 (bits 1-9)
            } else {
                // Given clue: set single value
                let digit = puzzle[row][col]
                gridValues[row][col] = digit
                gridCandidates[row][col] = (1 << digit)
            }
        }
    }
}

// Eliminate a digit from a cell's candidates
func eliminate(_ row: Int, _ col: Int, _ digit: Int) -> Bool {
    // Check if digit is already eliminated
    if !hasCandidate(gridCandidates[row][col], digit) {
        return true  // Already eliminated, no change
    }

    // Remove digit from candidates
    removeCandidate(&gridCandidates[row][col], digit)

    // Check for contradiction (no candidates left)
    let remaining = countCandidates(gridCandidates[row][col])
    if remaining == 0 {
        return false  // Contradiction
    }

    // If only one candidate left, assign it (singleton elimination)
    if remaining == 1 && gridValues[row][col] == 0 {
        let lastDigit = getFirstCandidate(gridCandidates[row][col])
        if !assign(row, col, lastDigit) {
            return false  // Assignment caused contradiction
        }
    }

    return true
}

// Assign a digit to a cell
func assign(_ row: Int, _ col: Int, _ digit: Int) -> Bool {
    // Increment iteration counter (this is our benchmark metric)
    cpIterations += 1

    // Set value
    gridValues[row][col] = digit
    gridCandidates[row][col] = (1 << digit)

    // Eliminate digit from all peers
    let peers = getPeers(row, col)

    for (peerRow, peerCol) in peers {
        if !eliminate(peerRow, peerCol, digit) {
            return false  // Contradiction in peer elimination
        }
    }

    return true
}

// Propagate constraints
func propagate() -> Bool {
    var changed = true

    while changed {
        changed = false

        // Strategy 1: Singleton elimination
        // If a cell has only one candidate, assign it
        for row in 0..<9 {
            for col in 0..<9 {
                if gridValues[row][col] == 0 {
                    let numCandidates = countCandidates(gridCandidates[row][col])
                    if numCandidates == 0 {
                        return false  // Contradiction
                    }
                    if numCandidates == 1 {
                        let digit = getFirstCandidate(gridCandidates[row][col])
                        if !assign(row, col, digit) {
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
        for row in 0..<9 {
            for digit in 1...9 {
                var count = 0
                var lastCol = -1
                var alreadyAssigned = false

                for col in 0..<9 {
                    if gridValues[row][col] == digit {
                        alreadyAssigned = true
                        break
                    }
                    if hasCandidate(gridCandidates[row][col], digit) {
                        count += 1
                        lastCol = col
                    }
                }

                if !alreadyAssigned {
                    if count == 1 {
                        if !assign(row, lastCol, digit) {
                            return false
                        }
                        changed = true
                    } else if count == 0 {
                        return false  // Digit cannot be placed anywhere in row
                    }
                }
            }
        }

        // Check columns
        for col in 0..<9 {
            for digit in 1...9 {
                var count = 0
                var lastRow = -1
                var alreadyAssigned = false

                for row in 0..<9 {
                    if gridValues[row][col] == digit {
                        alreadyAssigned = true
                        break
                    }
                    if hasCandidate(gridCandidates[row][col], digit) {
                        count += 1
                        lastRow = row
                    }
                }

                if !alreadyAssigned {
                    if count == 1 {
                        if !assign(lastRow, col, digit) {
                            return false
                        }
                        changed = true
                    } else if count == 0 {
                        return false  // Digit cannot be placed anywhere in column
                    }
                }
            }
        }

        // Check boxes
        for box in 0..<9 {
            let boxRow = (box / 3) * 3
            let boxCol = (box % 3) * 3

            for digit in 1...9 {
                var count = 0
                var lastR = -1
                var lastC = -1
                var alreadyAssigned = false

                for r in boxRow..<boxRow + 3 {
                    for c in boxCol..<boxCol + 3 {
                        if gridValues[r][c] == digit {
                            alreadyAssigned = true
                            break
                        }
                        if hasCandidate(gridCandidates[r][c], digit) {
                            count += 1
                            lastR = r
                            lastC = c
                        }
                    }
                    if alreadyAssigned {
                        break
                    }
                }

                if !alreadyAssigned {
                    if count == 1 {
                        if !assign(lastR, lastC, digit) {
                            return false
                        }
                        changed = true
                    } else if count == 0 {
                        return false  // Digit cannot be placed anywhere in box
                    }
                }
            }
        }
    }

    return true  // Success - reached fixpoint
}

// Find cell with minimum remaining values (MRV heuristic)
func findMrvCell() -> (Int, Int)? {
    var minCandidates = 10
    var bestRow = -1
    var bestCol = -1

    for r in 0..<9 {
        for c in 0..<9 {
            if gridValues[r][c] == 0 {
                let numCandidates = countCandidates(gridCandidates[r][c])
                if numCandidates < minCandidates {
                    minCandidates = numCandidates
                    bestRow = r
                    bestCol = c
                }
            }
        }
    }

    if bestRow == -1 {
        return nil  // No empty cells
    }

    return (bestRow, bestCol)
}

// Save and restore grid state for backtracking
func saveGridState() -> ([[Int]], [[Int]]) {
    return (gridValues, gridCandidates)
}

func restoreGridState(_ state: ([[Int]], [[Int]])) {
    gridValues = state.0
    gridCandidates = state.1
}

// Search with backtracking
func cpSearch() -> [Int]? {
    // Base case: check if grid is complete
    guard let (mrvRow, mrvCol) = findMrvCell() else {
        // No empty cells - grid is complete, extract solution
        var solution: [Int] = []
        for r in 0..<9 {
            for c in 0..<9 {
                solution.append(gridValues[r][c])
            }
        }
        return solution
    }

    // Recursive case: try each candidate for the MRV cell
    let candidates = gridCandidates[mrvRow][mrvCol]

    for digit in 1...9 {
        if hasCandidate(candidates, digit) {
            // Save grid state for backtracking
            let savedState = saveGridState()

            // Try assigning this digit
            if assign(mrvRow, mrvCol, digit) {
                // Assignment succeeded, propagate constraints
                if propagate() {
                    // Propagation succeeded, recurse
                    if let solution = cpSearch() {
                        return solution  // Found solution
                    }
                }
            }

            // Failed - restore grid state and try next candidate
            restoreGridState(savedState)
        }
    }

    // All candidates exhausted - dead end
    return nil
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
func readMatrixFile(_ filename: String, _ puzzle: inout [[Int]]) -> Bool {
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
if args.count != 2 {
    print("Usage: cp_solver <matrix_file>")
    exit(1)
}

let startTime = Date()

let filename = args[1]
var puzzle = Array(repeating: Array(repeating: 0, count: 9), count: 9)

if !readMatrixFile(filename, &puzzle) {
    print("Failed to read matrix file")
    exit(1)
}

// Print initial puzzle
printPuzzle(puzzle)

// Initialize CP grid
initGrid(puzzle)

// Apply initial propagation
if !propagate() {
    print("\nNo solution found (contradiction during initial propagation)")
    let endTime = Date()
    let elapsed = endTime.timeIntervalSince(startTime)
    print(String(format: "Seconds to process %.3f", elapsed))
    exit(0)
}

// Run search
if let solution = cpSearch() {
    // Convert solution array back to 2D for printing
    var solutionGrid = Array(repeating: Array(repeating: 0, count: 9), count: 9)
    for r in 0..<9 {
        for c in 0..<9 {
            solutionGrid[r][c] = solution[r * 9 + c]
        }
    }

    printPuzzle(solutionGrid)
    print("\nSolved in Iterations=\(cpIterations)\n")
} else {
    print("\nNo solution found")
}

let endTime = Date()
let elapsed = endTime.timeIntervalSince(startTime)
print(String(format: "Seconds to process %.3f", elapsed))
