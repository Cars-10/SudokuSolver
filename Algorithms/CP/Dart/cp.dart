import 'dart:io';

// ============================================================================
// DATA STRUCTURES
// ============================================================================

// Candidate tracking using bitsets (bits 1-9 for digits 1-9)
typedef CandidateSet = int;

// Grid structure with assigned values and candidate tracking
class CPGrid {
  List<List<int>> values = List.generate(9, (_) => List.filled(9, 0));
  List<List<CandidateSet>> candidates = List.generate(9, (_) => List.filled(9, 0));

  CPGrid();

  // Deep copy constructor
  CPGrid.copy(CPGrid other) {
    for (int r = 0; r < 9; r++) {
      for (int c = 0; c < 9; c++) {
        values[r][c] = other.values[r][c];
        candidates[r][c] = other.candidates[r][c];
      }
    }
  }
}

// ============================================================================
// GLOBAL ITERATION COUNTER
// ============================================================================
int cpIterations = 0;

// ============================================================================
// CANDIDATE MANIPULATION
// ============================================================================

bool hasCandidate(CandidateSet set, int digit) {
  return (set & (1 << digit)) != 0;
}

CandidateSet addCandidate(CandidateSet set, int digit) {
  return set | (1 << digit);
}

CandidateSet removeCandidate(CandidateSet set, int digit) {
  return set & ~(1 << digit);
}

int countCandidates(CandidateSet set) {
  int count = 0;
  for (int i = 0; i < 16; i++) {
    if ((set & (1 << i)) != 0) {
      count++;
    }
  }
  return count;
}

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

// Get first candidate digit from bitset (1-9)
int getFirstCandidate(CandidateSet cs) {
  for (int digit = 1; digit <= 9; digit++) {
    if (hasCandidate(cs, digit)) {
      return digit;
    }
  }
  return 0;
}

// Get all 20 peers for a cell (row, col, box)
List<List<int>> getPeers(int row, int col) {
  List<List<int>> peers = [];

  // Same row (9 cells minus self = 8)
  for (int c = 0; c < 9; c++) {
    if (c != col) {
      peers.add([row, c]);
    }
  }

  // Same column (9 cells minus self = 8)
  for (int r = 0; r < 9; r++) {
    if (r != row) {
      peers.add([r, col]);
    }
  }

  // Same 3x3 box (9 cells minus self minus already counted = 4)
  int boxRow = (row ~/ 3) * 3;
  int boxCol = (col ~/ 3) * 3;
  for (int r = boxRow; r < boxRow + 3; r++) {
    for (int c = boxCol; c < boxCol + 3; c++) {
      if (r != row && c != col) {
        peers.add([r, c]);
      }
    }
  }

  return peers;
}

// ============================================================================
// INITIALIZATION
// ============================================================================

void initGrid(CPGrid grid, List<List<int>> puzzle) {
  for (int row = 0; row < 9; row++) {
    for (int col = 0; col < 9; col++) {
      if (puzzle[row][col] == 0) {
        // Empty cell: set all candidates 1-9 (bits 1-9 set)
        grid.values[row][col] = 0;
        grid.candidates[row][col] = 0x3FE;  // Binary: 0011 1111 1110 (bits 1-9)
      } else {
        // Given clue: set single value
        int digit = puzzle[row][col];
        grid.values[row][col] = digit;
        grid.candidates[row][col] = (1 << digit);
      }
    }
  }
}

// ============================================================================
// CONSTRAINT PROPAGATION
// ============================================================================

bool eliminate(CPGrid grid, int row, int col, int digit) {
  // Check if digit is already eliminated
  if (!hasCandidate(grid.candidates[row][col], digit)) {
    return true;  // Already eliminated, no change
  }

  // Remove digit from candidates
  grid.candidates[row][col] = removeCandidate(grid.candidates[row][col], digit);

  // Check for contradiction (no candidates left)
  int remaining = countCandidates(grid.candidates[row][col]);
  if (remaining == 0) {
    return false;  // Contradiction
  }

  // If only one candidate left, assign it (singleton elimination)
  if (remaining == 1 && grid.values[row][col] == 0) {
    int lastDigit = getFirstCandidate(grid.candidates[row][col]);
    if (!assign(grid, row, col, lastDigit)) {
      return false;  // Assignment caused contradiction
    }
  }

  return true;
}

bool assign(CPGrid grid, int row, int col, int digit) {
  // Increment iteration counter (this is our benchmark metric)
  cpIterations++;

  // Set value
  grid.values[row][col] = digit;
  grid.candidates[row][col] = (1 << digit);

  // Eliminate digit from all peers
  List<List<int>> peers = getPeers(row, col);

  for (var peer in peers) {
    int peerRow = peer[0];
    int peerCol = peer[1];

    if (!eliminate(grid, peerRow, peerCol, digit)) {
      return false;  // Contradiction in peer elimination
    }
  }

  return true;
}

bool propagate(CPGrid grid) {
  bool changed = true;

  while (changed) {
    changed = false;

    // Strategy 1: Singleton elimination
    // If a cell has only one candidate, assign it
    for (int row = 0; row < 9; row++) {
      for (int col = 0; col < 9; col++) {
        if (grid.values[row][col] == 0) {
          int numCandidates = countCandidates(grid.candidates[row][col]);
          if (numCandidates == 0) {
            return false;  // Contradiction
          }
          if (numCandidates == 1) {
            int digit = getFirstCandidate(grid.candidates[row][col]);
            if (!assign(grid, row, col, digit)) {
              return false;  // Assignment caused contradiction
            }
            changed = true;
          }
        }
      }
    }

    // Strategy 2: Hidden singles
    // For each unit (row, col, box), if a digit appears in only one cell, assign it

    // Check rows
    for (int row = 0; row < 9; row++) {
      for (int digit = 1; digit <= 9; digit++) {
        int count = 0;
        int lastCol = -1;
        bool alreadyAssigned = false;

        for (int col = 0; col < 9; col++) {
          if (grid.values[row][col] == digit) {
            alreadyAssigned = true;
            break;
          }
          if (hasCandidate(grid.candidates[row][col], digit)) {
            count++;
            lastCol = col;
          }
        }

        if (!alreadyAssigned) {
          if (count == 1) {
            if (!assign(grid, row, lastCol, digit)) {
              return false;
            }
            changed = true;
          } else if (count == 0) {
            return false;  // Digit cannot be placed anywhere in row
          }
        }
      }
    }

    // Check columns
    for (int col = 0; col < 9; col++) {
      for (int digit = 1; digit <= 9; digit++) {
        int count = 0;
        int lastRow = -1;
        bool alreadyAssigned = false;

        for (int row = 0; row < 9; row++) {
          if (grid.values[row][col] == digit) {
            alreadyAssigned = true;
            break;
          }
          if (hasCandidate(grid.candidates[row][col], digit)) {
            count++;
            lastRow = row;
          }
        }

        if (!alreadyAssigned) {
          if (count == 1) {
            if (!assign(grid, lastRow, col, digit)) {
              return false;
            }
            changed = true;
          } else if (count == 0) {
            return false;  // Digit cannot be placed anywhere in column
          }
        }
      }
    }

    // Check boxes
    for (int box = 0; box < 9; box++) {
      int boxRow = (box ~/ 3) * 3;
      int boxCol = (box % 3) * 3;

      for (int digit = 1; digit <= 9; digit++) {
        int count = 0;
        int lastR = -1, lastC = -1;
        bool alreadyAssigned = false;

        for (int r = boxRow; r < boxRow + 3; r++) {
          for (int c = boxCol; c < boxCol + 3; c++) {
            if (grid.values[r][c] == digit) {
              alreadyAssigned = true;
              break;
            }
            if (hasCandidate(grid.candidates[r][c], digit)) {
              count++;
              lastR = r;
              lastC = c;
            }
          }
          if (alreadyAssigned) break;
        }

        if (!alreadyAssigned) {
          if (count == 1) {
            if (!assign(grid, lastR, lastC, digit)) {
              return false;
            }
            changed = true;
          } else if (count == 0) {
            return false;  // Digit cannot be placed anywhere in box
          }
        }
      }
    }
  }

  return true;  // Success - reached fixpoint
}

// ============================================================================
// SEARCH
// ============================================================================

bool findMrvCell(CPGrid grid, List<int> result) {
  int minCandidates = 10;  // More than 9, so any cell will be smaller
  bool found = false;

  for (int r = 0; r < 9; r++) {
    for (int c = 0; c < 9; c++) {
      if (grid.values[r][c] == 0) {
        int numCandidates = countCandidates(grid.candidates[r][c]);
        if (numCandidates < minCandidates) {
          minCandidates = numCandidates;
          result[0] = r;
          result[1] = c;
          found = true;
        }
      }
    }
  }

  return found;  // false if no empty cells (grid complete), true if cell found
}

bool cpSearch(CPGrid grid, List<int> solution) {
  // Base case: check if grid is complete
  List<int> mrvCell = [0, 0];
  if (!findMrvCell(grid, mrvCell)) {
    // No empty cells - grid is complete, extract solution
    for (int r = 0; r < 9; r++) {
      for (int c = 0; c < 9; c++) {
        solution[r * 9 + c] = grid.values[r][c];
      }
    }
    return true;  // Solved
  }

  int mrvRow = mrvCell[0];
  int mrvCol = mrvCell[1];

  // Recursive case: try each candidate for the MRV cell
  CandidateSet candidates = grid.candidates[mrvRow][mrvCol];

  for (int digit = 1; digit <= 9; digit++) {
    if (hasCandidate(candidates, digit)) {
      // Save grid state for backtracking
      CPGrid gridCopy = CPGrid.copy(grid);

      // Try assigning this digit
      if (assign(grid, mrvRow, mrvCol, digit)) {
        // Assignment succeeded, propagate constraints
        if (propagate(grid)) {
          // Propagation succeeded, recurse
          if (cpSearch(grid, solution)) {
            return true;  // Found solution
          }
        }
      }

      // Failed - restore grid state and try next candidate
      for (int r = 0; r < 9; r++) {
        for (int c = 0; c < 9; c++) {
          grid.values[r][c] = gridCopy.values[r][c];
          grid.candidates[r][c] = gridCopy.candidates[r][c];
        }
      }
    }
  }

  // All candidates exhausted - dead end
  return false;
}

// ============================================================================
// PUZZLE I/O
// ============================================================================

int readMatrixFile(String filename, List<List<int>> puzzle) {
  File file = File(filename);
  if (!file.existsSync()) {
    stderr.writeln("Error opening file '$filename'");
    return 0;
  }

  // Normalize path for output
  if (filename.startsWith('/app/Matrices/')) {
    print('../${filename.substring(5)}');
  } else {
    print(filename);
  }

  int lineCount = 0;
  List<String> lines = file.readAsLinesSync();

  for (String line in lines) {
    String lineStr = line.trim();

    // Skip comments and empty lines
    if (lineStr.isEmpty || lineStr.startsWith('#')) {
      continue;
    }

    if (lineCount >= 9) break;

    // Parse 9 integers from line
    List<String> parts = lineStr.split(RegExp(r'\s+'));
    if (parts.length >= 9) {
      for (int i = 0; i < 9; i++) {
        puzzle[lineCount][i] = int.parse(parts[i]);
        stdout.write('${puzzle[lineCount][i]} ');
      }
      print('');
      lineCount++;
    } else {
      stderr.writeln('Error: line does not contain 9 integers');
      return 0;
    }
  }

  return (lineCount == 9) ? 1 : 0;
}

void printPuzzle(List<List<int>> puzzle) {
  print('\nPuzzle:');
  for (int r = 0; r < 9; r++) {
    for (int c = 0; c < 9; c++) {
      stdout.write('${puzzle[r][c]} ');
    }
    print('');
  }
}

// ============================================================================
// MAIN ENTRYPOINT
// ============================================================================

void main(List<String> args) {
  Stopwatch stopwatch = Stopwatch()..start();

  if (args.length != 1) {
    stderr.writeln('Usage: dart cp.dart <matrix_file>');
    exit(1);
  }

  // Read puzzle from file
  List<List<int>> puzzle = List.generate(9, (_) => List.filled(9, 0));
  if (readMatrixFile(args[0], puzzle) == 0) {
    stderr.writeln('Failed to read matrix file');
    exit(1);
  }

  // Print initial puzzle
  printPuzzle(puzzle);

  // Initialize CP grid
  CPGrid grid = CPGrid();
  initGrid(grid, puzzle);

  // Apply initial propagation
  if (!propagate(grid)) {
    print('\nNo solution found (contradiction during initial propagation)');
    stopwatch.stop();
    double elapsed = stopwatch.elapsedMicroseconds / 1000000.0;
    print('Seconds to process ${elapsed.toStringAsFixed(3)}');
    exit(0);
  }

  // Run search
  List<int> solution = List.filled(81, 0);
  bool solved = cpSearch(grid, solution);

  if (solved) {
    // Convert solution array back to 2D for printing
    List<List<int>> solutionGrid = List.generate(9, (_) => List.filled(9, 0));
    for (int r = 0; r < 9; r++) {
      for (int c = 0; c < 9; c++) {
        solutionGrid[r][c] = solution[r * 9 + c];
      }
    }

    printPuzzle(solutionGrid);
    print('\nSolved in Iterations=$cpIterations\n');
  } else {
    print('\nNo solution found');
  }

  stopwatch.stop();
  double elapsed = stopwatch.elapsedMicroseconds / 1000000.0;
  print('Seconds to process ${elapsed.toStringAsFixed(3)}');
}
