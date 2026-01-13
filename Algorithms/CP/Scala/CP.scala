/**
 * Constraint Propagation (CP) Sudoku Solver - Scala Implementation
 *
 * Uses constraint propagation with backtracking and MRV (Minimum Remaining Values) heuristic.
 * Maintains candidate sets using 16-bit bitsets for memory efficiency.
 *
 * Key characteristics:
 * - Uses mutable Arrays for in-place updates during backtracking
 * - Short (16-bit) for candidate bitsets matching C reference
 * - Counts iterations at every assignment attempt (fingerprint: 67 for Matrix 1)
 */

import scala.io.Source
import java.io.File

// Implicit class for bitset operations on Short
implicit class BitOps(val bits: Short) extends AnyVal {
  def hasBit(bit: Int): Boolean = (bits & (1 << bit)) != 0
  def setBit(bit: Int): Short = (bits | (1 << bit)).toShort
  def clearBit(bit: Int): Short = (bits & ~(1 << bit)).toShort
  def countBits: Int = Integer.bitCount(bits & 0xFFFF)
}

// CP Grid structure
class CPGrid(val values: Array[Array[Int]], val candidates: Array[Array[Short]])

object CP {
  var iterations: Long = 0
  var puzzle: Array[Array[Int]] = Array.ofDim[Int](9, 9)

  def main(args: Array[String]): Unit = {
    val startTime = System.nanoTime()

    if (args.length != 1) {
      System.err.println("Usage: CP <matrix_file>")
      System.exit(1)
    }

    val filename = args(0)
    readMatrixFile(filename)
    printPuzzle(puzzle)

    // Initialize grid
    val grid = initGrid(puzzle)

    // Apply initial propagation
    if (!propagate(grid)) {
      println("\nNo solution found (contradiction during initial propagation)\n")
    } else {
      // Run search
      val solution = new Array[Int](81)
      val solved = search(grid, solution)

      if (solved) {
        val solutionGrid = Array.ofDim[Int](9, 9)
        for (r <- 0 until 9; c <- 0 until 9) {
          solutionGrid(r)(c) = solution(r * 9 + c)
        }
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

  // Initialize grid with candidates
  def initGrid(puzzleData: Array[Array[Int]]): CPGrid = {
    val values = Array.ofDim[Int](9, 9)
    val candidates = Array.ofDim[Short](9, 9)

    for (r <- 0 until 9; c <- 0 until 9) {
      if (puzzleData(r)(c) == 0) {
        // Empty cell: set all candidates 1-9 (bits 1-9 set)
        values(r)(c) = 0
        candidates(r)(c) = 0x3FE.toShort  // Binary: 0011 1111 1110 (bits 1-9)
      } else {
        // Given clue: set single value
        val digit = puzzleData(r)(c)
        values(r)(c) = digit
        candidates(r)(c) = (1 << digit).toShort
      }
    }

    new CPGrid(values, candidates)
  }

  // Get first candidate digit from bitset (1-9)
  def getFirstCandidate(cs: Short): Int = {
    for (digit <- 1 to 9) {
      if (cs.hasBit(digit)) return digit
    }
    0
  }

  // Get all 20 peers for a cell (row, col, box)
  def getPeers(row: Int, col: Int): Array[(Int, Int)] = {
    val peers = scala.collection.mutable.ArrayBuffer[(Int, Int)]()

    // Same row (8 cells)
    for (c <- 0 until 9 if c != col) {
      peers += ((row, c))
    }

    // Same column (8 cells)
    for (r <- 0 until 9 if r != row) {
      peers += ((r, col))
    }

    // Same 3x3 box (4 cells)
    val boxRow = (row / 3) * 3
    val boxCol = (col / 3) * 3
    for (r <- boxRow until boxRow + 3; c <- boxCol until boxCol + 3) {
      if (r != row && c != col) {
        peers += ((r, c))
      }
    }

    peers.toArray
  }

  // Eliminate digit from a cell's candidates
  def eliminate(grid: CPGrid, row: Int, col: Int, digit: Int): Boolean = {
    // Check if digit is already eliminated
    if (!grid.candidates(row)(col).hasBit(digit)) {
      return true  // Already eliminated
    }

    // Remove digit from candidates
    grid.candidates(row)(col) = grid.candidates(row)(col).clearBit(digit)

    // Check for contradiction
    val remaining = grid.candidates(row)(col).countBits
    if (remaining == 0) {
      return false  // Contradiction
    }

    // If only one candidate left, assign it (singleton elimination)
    if (remaining == 1 && grid.values(row)(col) == 0) {
      val lastDigit = getFirstCandidate(grid.candidates(row)(col))
      if (!assign(grid, row, col, lastDigit)) {
        return false
      }
    }

    true
  }

  // Assign a value to a cell
  def assign(grid: CPGrid, row: Int, col: Int, digit: Int): Boolean = {
    // Increment iteration counter
    iterations += 1

    // Set value
    grid.values(row)(col) = digit
    grid.candidates(row)(col) = (1 << digit).toShort

    // Eliminate digit from all peers
    val peers = getPeers(row, col)
    for ((peerRow, peerCol) <- peers) {
      if (!eliminate(grid, peerRow, peerCol, digit)) {
        return false  // Contradiction
      }
    }

    true
  }

  // Propagate constraints until fixpoint
  def propagate(grid: CPGrid): Boolean = {
    var changed = true

    while (changed) {
      changed = false

      // Strategy 1: Singleton elimination
      for (r <- 0 until 9; c <- 0 until 9) {
        if (grid.values(r)(c) == 0) {
          val numCandidates = grid.candidates(r)(c).countBits
          if (numCandidates == 0) {
            return false  // Contradiction
          }
          if (numCandidates == 1) {
            val digit = getFirstCandidate(grid.candidates(r)(c))
            if (!assign(grid, r, c, digit)) {
              return false
            }
            changed = true
          }
        }
      }

      // Strategy 2: Hidden singles in rows
      for (row <- 0 until 9; digit <- 1 to 9) {
        var count = 0
        var lastCol = -1
        var alreadyAssigned = false

        for (col <- 0 until 9) {
          if (grid.values(row)(col) == digit) {
            alreadyAssigned = true
          } else if (grid.candidates(row)(col).hasBit(digit)) {
            count += 1
            lastCol = col
          }
        }

        if (!alreadyAssigned) {
          if (count == 1) {
            if (!assign(grid, row, lastCol, digit)) {
              return false
            }
            changed = true
          } else if (count == 0) {
            return false  // Digit cannot be placed
          }
        }
      }

      // Strategy 3: Hidden singles in columns
      for (col <- 0 until 9; digit <- 1 to 9) {
        var count = 0
        var lastRow = -1
        var alreadyAssigned = false

        for (row <- 0 until 9) {
          if (grid.values(row)(col) == digit) {
            alreadyAssigned = true
          } else if (grid.candidates(row)(col).hasBit(digit)) {
            count += 1
            lastRow = row
          }
        }

        if (!alreadyAssigned) {
          if (count == 1) {
            if (!assign(grid, lastRow, col, digit)) {
              return false
            }
            changed = true
          } else if (count == 0) {
            return false  // Digit cannot be placed
          }
        }
      }

      // Strategy 4: Hidden singles in boxes
      for (box <- 0 until 9; digit <- 1 to 9) {
        val boxRow = (box / 3) * 3
        val boxCol = (box % 3) * 3

        var count = 0
        var lastR = -1
        var lastC = -1
        var alreadyAssigned = false

        for (r <- boxRow until boxRow + 3; c <- boxCol until boxCol + 3) {
          if (grid.values(r)(c) == digit) {
            alreadyAssigned = true
          } else if (grid.candidates(r)(c).hasBit(digit)) {
            count += 1
            lastR = r
            lastC = c
          }
        }

        if (!alreadyAssigned) {
          if (count == 1) {
            if (!assign(grid, lastR, lastC, digit)) {
              return false
            }
            changed = true
          } else if (count == 0) {
            return false  // Digit cannot be placed
          }
        }
      }
    }

    true  // Success - reached fixpoint
  }

  // Find cell with minimum remaining values (MRV heuristic)
  def findMRVCell(grid: CPGrid): Option[(Int, Int)] = {
    var minCandidates = 10
    var bestCell: Option[(Int, Int)] = None

    for (r <- 0 until 9; c <- 0 until 9) {
      if (grid.values(r)(c) == 0) {
        val numCandidates = grid.candidates(r)(c).countBits
        if (numCandidates < minCandidates) {
          minCandidates = numCandidates
          bestCell = Some((r, c))
        }
      }
    }

    bestCell
  }

  // Recursive search with backtracking
  def search(grid: CPGrid, solution: Array[Int]): Boolean = {
    // Find MRV cell
    findMRVCell(grid) match {
      case None =>
        // No empty cells - grid is complete
        for (r <- 0 until 9; c <- 0 until 9) {
          solution(r * 9 + c) = grid.values(r)(c)
        }
        true

      case Some((mrvRow, mrvCol)) =>
        // Try each candidate for the MRV cell
        val candidateSet = grid.candidates(mrvRow)(mrvCol)

        for (digit <- 1 to 9) {
          if (candidateSet.hasBit(digit)) {
            // Save grid state for backtracking
            val valuesCopy = grid.values.map(_.clone())
            val candidatesCopy = grid.candidates.map(_.clone())

            // Try assigning this digit
            if (assign(grid, mrvRow, mrvCol, digit)) {
              // Assignment succeeded, propagate constraints
              if (propagate(grid)) {
                // Propagation succeeded, recurse
                if (search(grid, solution)) {
                  return true  // Found solution
                }
              }
            }

            // Failed - restore grid state
            for (r <- 0 until 9) {
              grid.values(r) = valuesCopy(r)
              grid.candidates(r) = candidatesCopy(r)
            }
          }
        }

        false  // All candidates exhausted
    }
  }
}
