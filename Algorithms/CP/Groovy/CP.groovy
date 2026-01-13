import groovy.transform.Field

// ============================================================================
// DATA STRUCTURES
// ============================================================================

class CPGrid {
    int[][] values = new int[9][9]
    short[][] candidates = new short[9][9]
}

// ============================================================================
// GLOBAL STATE
// ============================================================================

@Field long cpIterations = 0

// ============================================================================
// CANDIDATE MANIPULATION
// ============================================================================

static short setBit(short bits, int bit) {
    return (bits | (1 << bit)) as short
}

static short clearBit(short bits, int bit) {
    return (bits & ~(1 << bit)) as short
}

static boolean testBit(short bits, int bit) {
    return (bits & (1 << bit)) != 0
}

static int countBits(short bits) {
    return Integer.bitCount(bits as int)
}

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

int getFirstCandidate(short cs) {
    for (int digit = 1; digit <= 9; digit++) {
        if (testBit(cs, digit)) {
            return digit
        }
    }
    return 0
}

List<int[]> getPeers(int row, int col) {
    List<int[]> peers = []

    // Same row (8 cells)
    for (int c = 0; c < 9; c++) {
        if (c != col) {
            peers.add([row, c] as int[])
        }
    }

    // Same column (8 cells)
    for (int r = 0; r < 9; r++) {
        if (r != row) {
            peers.add([r, col] as int[])
        }
    }

    // Same 3x3 box (4 cells not already counted)
    int boxRow = (int)(row / 3) * 3
    int boxCol = (int)(col / 3) * 3
    for (int r = boxRow; r < boxRow + 3; r++) {
        for (int c = boxCol; c < boxCol + 3; c++) {
            if (r != row && c != col) {
                peers.add([r, c] as int[])
            }
        }
    }

    return peers
}

// ============================================================================
// INITIALIZATION
// ============================================================================

void initGrid(CPGrid grid, int[][] puzzle) {
    for (int row = 0; row < 9; row++) {
        for (int col = 0; col < 9; col++) {
            if (puzzle[row][col] == 0) {
                // Empty cell: set all candidates 1-9
                grid.values[row][col] = 0
                grid.candidates[row][col] = 0x3FE as short  // Binary: 0011 1111 1110 (bits 1-9)
            } else {
                // Given clue: set single value
                int digit = puzzle[row][col]
                grid.values[row][col] = digit
                grid.candidates[row][col] = (1 << digit) as short
            }
        }
    }
}

// ============================================================================
// CONSTRAINT PROPAGATION
// ============================================================================

boolean eliminate(CPGrid grid, int row, int col, int digit) {
    // Check if digit is already eliminated
    if (!testBit(grid.candidates[row][col], digit)) {
        return true  // Already eliminated, no change
    }

    // Remove digit from candidates
    grid.candidates[row][col] = clearBit(grid.candidates[row][col], digit)

    // Check for contradiction
    int remaining = countBits(grid.candidates[row][col])
    if (remaining == 0) {
        return false  // Contradiction
    }

    // If only one candidate left, assign it (singleton elimination)
    if (remaining == 1 && grid.values[row][col] == 0) {
        int lastDigit = getFirstCandidate(grid.candidates[row][col])
        if (!assign(grid, row, col, lastDigit)) {
            return false  // Assignment caused contradiction
        }
    }

    return true
}

boolean assign(CPGrid grid, int row, int col, int digit) {
    // Increment iteration counter (benchmark metric)
    cpIterations++

    // Set value
    grid.values[row][col] = digit
    grid.candidates[row][col] = (1 << digit) as short

    // Eliminate digit from all peers
    List<int[]> peers = getPeers(row, col)

    for (int[] peer : peers) {
        int peerRow = peer[0]
        int peerCol = peer[1]

        if (!eliminate(grid, peerRow, peerCol, digit)) {
            return false  // Contradiction in peer elimination
        }
    }

    return true
}

boolean propagate(CPGrid grid) {
    boolean changed = true

    while (changed) {
        changed = false

        // Strategy 1: Singleton elimination
        for (int row = 0; row < 9; row++) {
            for (int col = 0; col < 9; col++) {
                if (grid.values[row][col] == 0) {
                    int numCandidates = countBits(grid.candidates[row][col])
                    if (numCandidates == 0) {
                        return false  // Contradiction
                    }
                    if (numCandidates == 1) {
                        int digit = getFirstCandidate(grid.candidates[row][col])
                        if (!assign(grid, row, col, digit)) {
                            return false
                        }
                        changed = true
                    }
                }
            }
        }

        // Strategy 2: Hidden singles - rows
        for (int row = 0; row < 9; row++) {
            for (int digit = 1; digit <= 9; digit++) {
                int count = 0
                int lastCol = -1
                for (int col = 0; col < 9; col++) {
                    if (grid.values[row][col] == digit) {
                        count = 0
                        break
                    }
                    if (testBit(grid.candidates[row][col], digit)) {
                        count++
                        lastCol = col
                    }
                }
                if (count == 1) {
                    if (!assign(grid, row, lastCol, digit)) {
                        return false
                    }
                    changed = true
                } else if (count == 0) {
                    // Check if digit is already assigned
                    boolean found = false
                    for (int col = 0; col < 9; col++) {
                        if (grid.values[row][col] == digit) {
                            found = true
                            break
                        }
                    }
                    if (!found) {
                        return false  // Contradiction
                    }
                }
            }
        }

        // Strategy 2: Hidden singles - columns
        for (int col = 0; col < 9; col++) {
            for (int digit = 1; digit <= 9; digit++) {
                int count = 0
                int lastRow = -1
                for (int row = 0; row < 9; row++) {
                    if (grid.values[row][col] == digit) {
                        count = 0
                        break
                    }
                    if (testBit(grid.candidates[row][col], digit)) {
                        count++
                        lastRow = row
                    }
                }
                if (count == 1) {
                    if (!assign(grid, lastRow, col, digit)) {
                        return false
                    }
                    changed = true
                } else if (count == 0) {
                    // Check if digit is already assigned
                    boolean found = false
                    for (int row = 0; row < 9; row++) {
                        if (grid.values[row][col] == digit) {
                            found = true
                            break
                        }
                    }
                    if (!found) {
                        return false  // Contradiction
                    }
                }
            }
        }

        // Strategy 2: Hidden singles - boxes
        for (int box = 0; box < 9; box++) {
            int boxRow = (int)(box / 3) * 3
            int boxCol = (box % 3) * 3

            for (int digit = 1; digit <= 9; digit++) {
                int count = 0
                int lastR = -1, lastC = -1

                boolean skipBox = false
                for (int r = boxRow; r < boxRow + 3; r++) {
                    for (int c = boxCol; c < boxCol + 3; c++) {
                        if (grid.values[r][c] == digit) {
                            count = 0
                            skipBox = true
                            break
                        }
                        if (testBit(grid.candidates[r][c], digit)) {
                            count++
                            lastR = r
                            lastC = c
                        }
                    }
                    if (skipBox) break
                }

                if (!skipBox) {
                    if (count == 1) {
                        if (!assign(grid, lastR, lastC, digit)) {
                            return false
                        }
                        changed = true
                    } else if (count == 0) {
                        // Check if digit is already assigned
                        boolean found = false
                        outerLoop:
                        for (int r = boxRow; r < boxRow + 3; r++) {
                            for (int c = boxCol; c < boxCol + 3; c++) {
                                if (grid.values[r][c] == digit) {
                                    found = true
                                    break outerLoop
                                }
                            }
                        }
                        if (!found) {
                            return false  // Contradiction
                        }
                    }
                }
            }
        }
    }

    return true  // Success
}

// ============================================================================
// SEARCH
// ============================================================================

Map findMRVCell(CPGrid grid) {
    int minCandidates = 10
    int bestRow = -1, bestCol = -1

    for (int r = 0; r < 9; r++) {
        for (int c = 0; c < 9; c++) {
            if (grid.values[r][c] == 0) {
                int numCandidates = countBits(grid.candidates[r][c])
                if (numCandidates < minCandidates) {
                    minCandidates = numCandidates
                    bestRow = r
                    bestCol = c
                }
            }
        }
    }

    if (bestRow == -1) {
        return null  // No empty cells - grid is complete
    }

    return [row: bestRow, col: bestCol]
}

boolean cpSearch(CPGrid grid, int[] solution) {
    // Base case: check if grid is complete
    Map mrvCell = findMRVCell(grid)
    if (mrvCell == null) {
        // No empty cells - extract solution
        for (int r = 0; r < 9; r++) {
            for (int c = 0; c < 9; c++) {
                solution[r * 9 + c] = grid.values[r][c]
            }
        }
        return true
    }

    // Recursive case: try each candidate for MRV cell
    int mrvRow = mrvCell.row
    int mrvCol = mrvCell.col
    short candidates = grid.candidates[mrvRow][mrvCol]

    for (int digit = 1; digit <= 9; digit++) {
        if (testBit(candidates, digit)) {
            // Save grid state for backtracking
            CPGrid gridCopy = new CPGrid()
            for (int r = 0; r < 9; r++) {
                for (int c = 0; c < 9; c++) {
                    gridCopy.values[r][c] = grid.values[r][c]
                    gridCopy.candidates[r][c] = grid.candidates[r][c]
                }
            }

            // Try assigning this digit
            if (assign(grid, mrvRow, mrvCol, digit)) {
                // Assignment succeeded, propagate constraints
                if (propagate(grid)) {
                    // Propagation succeeded, recurse
                    if (cpSearch(grid, solution)) {
                        return true  // Found solution
                    }
                }
            }

            // Failed - restore grid state
            for (int r = 0; r < 9; r++) {
                for (int c = 0; c < 9; c++) {
                    grid.values[r][c] = gridCopy.values[r][c]
                    grid.candidates[r][c] = gridCopy.candidates[r][c]
                }
            }
        }
    }

    // All candidates exhausted
    return false
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

boolean readMatrixFile(String filename, int[][] puzzle) {
    def file = new File(filename)
    if (!file.exists()) {
        System.err.println "Error opening file '${filename}'"
        return false
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

    return lineCount == 9
}

// ============================================================================
// MAIN
// ============================================================================

long startTime = System.nanoTime()

if (args.length != 1) {
    System.err.println "Usage: groovy CP.groovy <matrix_file>"
    System.exit(1)
}

// Read puzzle from file
int[][] puzzle = new int[9][9]
if (!readMatrixFile(args[0], puzzle)) {
    System.err.println "Failed to read matrix file"
    System.exit(1)
}

// Print initial puzzle
printPuzzle(puzzle)

// Initialize CP grid
CPGrid grid = new CPGrid()
initGrid(grid, puzzle)

// Apply initial propagation
if (!propagate(grid)) {
    println "\nNo solution found (contradiction during initial propagation)"
    long elapsed = System.nanoTime() - startTime
    printf "Seconds to process %.3f%n", elapsed / 1_000_000_000.0
    System.exit(0)
}

// Run search
cpIterations = 0
int[] solution = new int[81]
boolean solved = cpSearch(grid, solution)

if (solved) {
    // Convert solution array to 2D grid for printing
    int[][] solutionGrid = new int[9][9]
    for (int r = 0; r < 9; r++) {
        for (int c = 0; c < 9; c++) {
            solutionGrid[r][c] = solution[r * 9 + c]
        }
    }

    printPuzzle(solutionGrid)
    println "\nSolved in Iterations=${cpIterations}\n"
} else {
    println "\nNo solution found"
}

long elapsed = System.nanoTime() - startTime
printf "Seconds to process %.3f%n", elapsed / 1_000_000_000.0
