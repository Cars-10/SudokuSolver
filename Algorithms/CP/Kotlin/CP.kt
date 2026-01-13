/**
 * Sudoku Solver - CP (Constraint Propagation) Implementation in Kotlin
 *
 * Uses constraint propagation with MRV heuristic for efficient solving.
 * Candidate sets tracked using Short (16-bit) bitsets.
 *
 * Expected iterations for Matrix 1: 67
 */

import java.io.File

// Candidate tracking using bitsets (9 bits for digits 1-9)
typealias CandidateSet = Short

// Grid structure with assigned values and candidate tracking
data class CPGrid(
    val values: Array<IntArray>,               // Assigned values (0 = empty)
    val candidates: Array<ShortArray>          // Possible values per cell (bitset)
) {
    // Deep copy constructor
    fun copy(): CPGrid {
        val valuesCopy = Array(9) { i -> values[i].clone() }
        val candidatesCopy = Array(9) { i -> candidates[i].clone() }
        return CPGrid(valuesCopy, candidatesCopy)
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is CPGrid) return false
        return values.contentDeepEquals(other.values) &&
               candidates.contentDeepEquals(other.candidates)
    }

    override fun hashCode(): Int {
        return values.contentDeepHashCode() * 31 + candidates.contentDeepHashCode()
    }
}

// Extension functions for bitset operations on Short
inline fun Short.hasBit(bit: Int): Boolean = (this.toInt() and (1 shl bit)) != 0
inline fun Short.setBit(bit: Int): Short = (this.toInt() or (1 shl bit)).toShort()
inline fun Short.clearBit(bit: Int): Short = (this.toInt() and (1 shl bit).inv()).toShort()
inline fun Short.countBits(): Int = this.toInt().countOneBits()

// Global iteration counter
var cpIterations = 0L

fun main(args: Array<String>) {
    val startTime = System.nanoTime()

    if (args.isEmpty()) {
        System.err.println("Usage: CP <matrix_file>")
        return
    }

    // Read puzzle from file
    val puzzle = Array(9) { IntArray(9) }
    if (!readMatrixFile(args[0], puzzle)) {
        System.err.println("Failed to read matrix file")
        return
    }

    // Print initial puzzle
    printPuzzle(puzzle)

    // Initialize CP grid
    val grid = initGrid(puzzle)

    // Apply initial propagation
    if (!propagate(grid)) {
        println("\nNo solution found (contradiction during initial propagation)")
        val elapsed = (System.nanoTime() - startTime) / 1_000_000_000.0
        println("Seconds to process %.3f".format(elapsed))
        return
    }

    // Run search
    cpIterations = 0
    val solution = IntArray(81)
    val solved = cpSearch(grid, solution)

    if (solved) {
        // Convert solution array back to 2D for printing
        val solutionGrid = Array(9) { r ->
            IntArray(9) { c -> solution[r * 9 + c] }
        }

        printPuzzle(solutionGrid)
        println("\nSolved in Iterations=$cpIterations\n")
    } else {
        println("\nNo solution found")
    }

    val elapsed = (System.nanoTime() - startTime) / 1_000_000_000.0
    println("Seconds to process %.3f".format(elapsed))
}

fun readMatrixFile(filename: String, puzzle: Array<IntArray>): Boolean {
    val file = File(filename)
    if (!file.exists()) {
        System.err.println("Error opening file '$filename'")
        return false
    }

    // Normalize path for output (convert absolute to relative)
    val displayPath = if (filename.startsWith("/app/Matrices/")) {
        "../" + filename.substring(5)  // Skip "/app/" to get "Matrices/..."
    } else {
        filename
    }
    println(displayPath)

    var lineCount = 0
    var error = false

    file.useLines { lines ->
        for (line in lines) {
            if (lineCount >= 9) break

            // Skip comments and empty lines
            val trimmed = line.trim()
            if (trimmed.isEmpty() || trimmed.startsWith("#")) continue

            val parts = trimmed.split(Regex("\\s+"))
            if (parts.size == 9) {
                for (j in 0 until 9) {
                    puzzle[lineCount][j] = parts[j].toInt()
                    print("${puzzle[lineCount][j]} ")
                }
                println()
                lineCount++
            } else {
                System.err.println("Error: line does not contain 9 integers")
                error = true
                break
            }
        }
    }

    return !error && lineCount == 9
}

fun printPuzzle(puzzle: Array<IntArray>) {
    println("\nPuzzle:")
    for (r in 0 until 9) {
        for (c in 0 until 9) {
            print("${puzzle[r][c]} ")
        }
        println()
    }
}

// Get all 20 peers for a cell (row, col, box)
fun getPeers(row: Int, col: Int): List<Pair<Int, Int>> {
    val peers = mutableListOf<Pair<Int, Int>>()

    // Same row (9 cells minus self = 8)
    for (c in 0 until 9) {
        if (c != col) {
            peers.add(Pair(row, c))
        }
    }

    // Same column (9 cells minus self = 8)
    for (r in 0 until 9) {
        if (r != row) {
            peers.add(Pair(r, col))
        }
    }

    // Same 3x3 box (9 cells minus self minus already counted = 4)
    val boxRow = (row / 3) * 3
    val boxCol = (col / 3) * 3
    for (r in boxRow until boxRow + 3) {
        for (c in boxCol until boxCol + 3) {
            if (r != row && c != col) {
                peers.add(Pair(r, c))
            }
        }
    }

    return peers
}

// Initialize grid from puzzle
fun initGrid(puzzle: Array<IntArray>): CPGrid {
    val values = Array(9) { IntArray(9) }
    val candidates = Array(9) { ShortArray(9) }

    for (row in 0 until 9) {
        for (col in 0 until 9) {
            if (puzzle[row][col] == 0) {
                // Empty cell: set all candidates 1-9 (bits 1-9 set)
                values[row][col] = 0
                candidates[row][col] = 0x3FE.toShort()  // Binary: 0011 1111 1110 (bits 1-9)
            } else {
                // Given clue: set single value
                val digit = puzzle[row][col]
                values[row][col] = digit
                candidates[row][col] = (1 shl digit).toShort()
            }
        }
    }

    return CPGrid(values, candidates)
}

// Count number of candidates in a bitset
fun countCandidates(cs: CandidateSet): Int = cs.countBits()

// Get first candidate digit from bitset (1-9)
fun getFirstCandidate(cs: CandidateSet): Int {
    for (digit in 1..9) {
        if (cs.hasBit(digit)) {
            return digit
        }
    }
    return 0
}

// Eliminate a digit from a cell's candidates
fun eliminate(grid: CPGrid, row: Int, col: Int, digit: Int): Boolean {
    // Check if digit is already eliminated
    if (!grid.candidates[row][col].hasBit(digit)) {
        return true  // Already eliminated, no change
    }

    // Remove digit from candidates
    grid.candidates[row][col] = grid.candidates[row][col].clearBit(digit)

    // Check for contradiction (no candidates left)
    val remaining = countCandidates(grid.candidates[row][col])
    if (remaining == 0) {
        return false  // Contradiction
    }

    // If only one candidate left, assign it (singleton elimination)
    if (remaining == 1 && grid.values[row][col] == 0) {
        val lastDigit = getFirstCandidate(grid.candidates[row][col])
        if (!assign(grid, row, col, lastDigit)) {
            return false  // Assignment caused contradiction
        }
    }

    return true
}

// Assign a digit to a cell
fun assign(grid: CPGrid, row: Int, col: Int, digit: Int): Boolean {
    // Increment iteration counter (this is our benchmark metric)
    cpIterations++

    // Set value
    grid.values[row][col] = digit
    grid.candidates[row][col] = (1 shl digit).toShort()

    // Eliminate digit from all peers
    val peers = getPeers(row, col)
    for ((peerRow, peerCol) in peers) {
        if (!eliminate(grid, peerRow, peerCol, digit)) {
            return false  // Contradiction in peer elimination
        }
    }

    return true
}

// Constraint propagation
fun propagate(grid: CPGrid): Boolean {
    var changed = true

    while (changed) {
        changed = false

        // Strategy 1: Singleton elimination
        // If a cell has only one candidate, assign it
        for (row in 0 until 9) {
            for (col in 0 until 9) {
                if (grid.values[row][col] == 0) {
                    val numCandidates = countCandidates(grid.candidates[row][col])
                    if (numCandidates == 0) {
                        return false  // Contradiction
                    }
                    if (numCandidates == 1) {
                        val digit = getFirstCandidate(grid.candidates[row][col])
                        if (!assign(grid, row, col, digit)) {
                            return false  // Assignment caused contradiction
                        }
                        changed = true
                    }
                }
            }
        }

        // Strategy 2: Hidden singles - Check rows
        for (row in 0 until 9) {
            for (digit in 1..9) {
                var count = 0
                var lastCol = -1
                for (col in 0 until 9) {
                    if (grid.values[row][col] == digit) {
                        count = 0  // Already assigned
                        break
                    }
                    if (grid.candidates[row][col].hasBit(digit)) {
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
                    // Check if digit is already assigned in this row
                    val found = (0 until 9).any { col -> grid.values[row][col] == digit }
                    if (!found) {
                        return false  // Digit cannot be placed anywhere in row
                    }
                }
            }
        }

        // Check columns
        for (col in 0 until 9) {
            for (digit in 1..9) {
                var count = 0
                var lastRow = -1
                for (row in 0 until 9) {
                    if (grid.values[row][col] == digit) {
                        count = 0  // Already assigned
                        break
                    }
                    if (grid.candidates[row][col].hasBit(digit)) {
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
                    // Check if digit is already assigned in this column
                    val found = (0 until 9).any { row -> grid.values[row][col] == digit }
                    if (!found) {
                        return false  // Digit cannot be placed anywhere in column
                    }
                }
            }
        }

        // Check boxes
        for (box in 0 until 9) {
            val boxRow = (box / 3) * 3
            val boxCol = (box % 3) * 3

            for (digit in 1..9) {
                var count = 0
                var lastR = -1
                var lastC = -1
                var alreadyAssigned = false

                for (r in boxRow until boxRow + 3) {
                    for (c in boxCol until boxCol + 3) {
                        if (grid.values[r][c] == digit) {
                            alreadyAssigned = true
                            break
                        }
                        if (grid.candidates[r][c].hasBit(digit)) {
                            count++
                            lastR = r
                            lastC = c
                        }
                    }
                    if (alreadyAssigned) break
                }

                if (!alreadyAssigned) {
                    if (count == 1) {
                        if (!assign(grid, lastR, lastC, digit)) {
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
fun findMrvCell(grid: CPGrid): Pair<Int, Int>? {
    var minCandidates = 10  // More than 9, so any cell will be smaller
    var bestRow = -1
    var bestCol = -1

    for (r in 0 until 9) {
        for (c in 0 until 9) {
            if (grid.values[r][c] == 0) {
                val numCandidates = countCandidates(grid.candidates[r][c])
                if (numCandidates < minCandidates) {
                    minCandidates = numCandidates
                    bestRow = r
                    bestCol = c
                }
            }
        }
    }

    return if (bestRow == -1) null else Pair(bestRow, bestCol)
}

// Search with constraint propagation
fun cpSearch(grid: CPGrid, solution: IntArray): Boolean {
    // Base case: check if grid is complete
    val mrvCell = findMrvCell(grid)
    if (mrvCell == null) {
        // No empty cells - grid is complete, extract solution
        for (r in 0 until 9) {
            for (c in 0 until 9) {
                solution[r * 9 + c] = grid.values[r][c]
            }
        }
        return true  // Solved
    }

    // Recursive case: try each candidate for the MRV cell
    val (mrvRow, mrvCol) = mrvCell
    val candidates = grid.candidates[mrvRow][mrvCol]

    for (digit in 1..9) {
        if (candidates.hasBit(digit)) {
            // Save grid state for backtracking
            val gridCopy = grid.copy()

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

            // Failed - restore grid state and try next candidate
            // Copy back the saved state
            for (r in 0 until 9) {
                for (c in 0 until 9) {
                    grid.values[r][c] = gridCopy.values[r][c]
                    grid.candidates[r][c] = gridCopy.candidates[r][c]
                }
            }
        }
    }

    // All candidates exhausted - dead end
    return false
}
