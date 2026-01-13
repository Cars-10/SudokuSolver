import std.stdio;
import std.algorithm;
import std.conv;
import std.file;
import std.string;
import core.time;
import std.datetime.stopwatch;

// Type alias for candidate sets (bitset)
alias CandidateSet = ushort;

// CP Grid structure
struct CPGrid {
    int[9][9] values;           // Cell values (0 = empty)
    CandidateSet[9][9] candidates;  // Candidate bitsets (bits 1-9)
}

// Global iteration counter
long cpIterations = 0;

// Helper: Check if candidate digit is in set
bool hasCand(CandidateSet cs, int digit) {
    return (cs & (1 << digit)) != 0;
}

// Helper: Count candidates in set
int countCand(CandidateSet cs) {
    int count = 0;
    for (int digit = 1; digit <= 9; digit++) {
        if (hasCand(cs, digit)) {
            count++;
        }
    }
    return count;
}

// Helper: Get first candidate digit
int getFirstCand(CandidateSet cs) {
    for (int digit = 1; digit <= 9; digit++) {
        if (hasCand(cs, digit)) {
            return digit;
        }
    }
    return 0;
}

// Get all 20 peers for a cell (row, col, box)
int[2][] getPeers(int row, int col) {
    int[2][] peers;
    peers.reserve(20);

    // Same row (9 cells minus self = 8)
    for (int c = 0; c < 9; c++) {
        if (c != col) {
            peers ~= [row, c];
        }
    }

    // Same column (9 cells minus self = 8)
    for (int r = 0; r < 9; r++) {
        if (r != row) {
            peers ~= [r, col];
        }
    }

    // Same 3x3 box (9 cells minus self minus already counted = 4)
    int boxRow = (row / 3) * 3;
    int boxCol = (col / 3) * 3;
    for (int r = boxRow; r < boxRow + 3; r++) {
        for (int c = boxCol; c < boxCol + 3; c++) {
            if (r != row && c != col) {
                peers ~= [r, c];
            }
        }
    }

    return peers;
}

// Initialize grid from puzzle
CPGrid initGrid(int[][] puzzle) {
    CPGrid grid;

    for (int row = 0; row < 9; row++) {
        for (int col = 0; col < 9; col++) {
            if (puzzle[row][col] == 0) {
                // Empty cell: set all candidates 1-9
                grid.values[row][col] = 0;
                grid.candidates[row][col] = 0x3FE;  // Bits 1-9 set
            } else {
                // Given clue: set single value
                int digit = puzzle[row][col];
                grid.values[row][col] = digit;
                grid.candidates[row][col] = cast(CandidateSet)(1 << digit);
            }
        }
    }

    return grid;
}

// Eliminate a digit from a cell's candidates
bool eliminate(ref CPGrid grid, int row, int col, int digit) {
    // Check if digit is already eliminated
    if (!hasCand(grid.candidates[row][col], digit)) {
        return true;  // Already eliminated
    }

    // Remove digit from candidates
    grid.candidates[row][col] &= cast(CandidateSet)(~(1 << digit));

    // Check for contradiction
    int remaining = countCand(grid.candidates[row][col]);
    if (remaining == 0) {
        return false;  // Contradiction
    }

    // If only one candidate left, assign it (singleton elimination)
    if (remaining == 1 && grid.values[row][col] == 0) {
        int lastDigit = getFirstCand(grid.candidates[row][col]);
        if (!assign(grid, row, col, lastDigit)) {
            return false;
        }
    }

    return true;
}

// Assign a digit to a cell and propagate
bool assign(ref CPGrid grid, int row, int col, int digit) {
    // Increment iteration counter
    cpIterations++;

    // Set value
    grid.values[row][col] = digit;
    grid.candidates[row][col] = cast(CandidateSet)(1 << digit);

    // Eliminate digit from all peers
    auto peers = getPeers(row, col);
    foreach (peer; peers) {
        if (!eliminate(grid, peer[0], peer[1], digit)) {
            return false;  // Contradiction
        }
    }

    return true;
}

// Propagate constraints (singleton elimination and hidden singles)
bool propagate(ref CPGrid grid) {
    bool changed = true;

    while (changed) {
        changed = false;

        // Strategy 1: Singleton elimination
        for (int row = 0; row < 9; row++) {
            for (int col = 0; col < 9; col++) {
                if (grid.values[row][col] == 0) {
                    int numCandidates = countCand(grid.candidates[row][col]);
                    if (numCandidates == 0) {
                        return false;  // Contradiction
                    }
                    if (numCandidates == 1) {
                        int digit = getFirstCand(grid.candidates[row][col]);
                        if (!assign(grid, row, col, digit)) {
                            return false;
                        }
                        changed = true;
                    }
                }
            }
        }

        // Strategy 2: Hidden singles - check rows
        for (int row = 0; row < 9; row++) {
            for (int digit = 1; digit <= 9; digit++) {
                int count = 0;
                int lastCol = -1;
                for (int col = 0; col < 9; col++) {
                    if (grid.values[row][col] == digit) {
                        count = 0;  // Already assigned
                        break;
                    }
                    if (hasCand(grid.candidates[row][col], digit)) {
                        count++;
                        lastCol = col;
                    }
                }
                if (count == 1) {
                    if (!assign(grid, row, lastCol, digit)) {
                        return false;
                    }
                    changed = true;
                } else if (count == 0) {
                    // Check if digit is already assigned
                    bool found = false;
                    for (int col = 0; col < 9; col++) {
                        if (grid.values[row][col] == digit) {
                            found = true;
                            break;
                        }
                    }
                    if (!found) {
                        return false;  // Contradiction
                    }
                }
            }
        }

        // Strategy 2: Hidden singles - check columns
        for (int col = 0; col < 9; col++) {
            for (int digit = 1; digit <= 9; digit++) {
                int count = 0;
                int lastRow = -1;
                for (int row = 0; row < 9; row++) {
                    if (grid.values[row][col] == digit) {
                        count = 0;  // Already assigned
                        break;
                    }
                    if (hasCand(grid.candidates[row][col], digit)) {
                        count++;
                        lastRow = row;
                    }
                }
                if (count == 1) {
                    if (!assign(grid, lastRow, col, digit)) {
                        return false;
                    }
                    changed = true;
                } else if (count == 0) {
                    // Check if digit is already assigned
                    bool found = false;
                    for (int row = 0; row < 9; row++) {
                        if (grid.values[row][col] == digit) {
                            found = true;
                            break;
                        }
                    }
                    if (!found) {
                        return false;  // Contradiction
                    }
                }
            }
        }

        // Strategy 2: Hidden singles - check boxes
        for (int box = 0; box < 9; box++) {
            int boxRow = (box / 3) * 3;
            int boxCol = (box % 3) * 3;

            for (int digit = 1; digit <= 9; digit++) {
                int count = 0;
                int lastR = -1, lastC = -1;

                for (int r = boxRow; r < boxRow + 3; r++) {
                    for (int c = boxCol; c < boxCol + 3; c++) {
                        if (grid.values[r][c] == digit) {
                            count = 0;  // Already assigned
                            goto nextBoxDigit;
                        }
                        if (hasCand(grid.candidates[r][c], digit)) {
                            count++;
                            lastR = r;
                            lastC = c;
                        }
                    }
                }

                if (count == 1) {
                    if (!assign(grid, lastR, lastC, digit)) {
                        return false;
                    }
                    changed = true;
                } else if (count == 0) {
                    // Check if digit is already assigned
                    bool found = false;
                    for (int r = boxRow; r < boxRow + 3; r++) {
                        for (int c = boxCol; c < boxCol + 3; c++) {
                            if (grid.values[r][c] == digit) {
                                found = true;
                                goto foundBoxDigit;
                            }
                        }
                    }
                    foundBoxDigit:
                    if (!found) {
                        return false;  // Contradiction
                    }
                }

                nextBoxDigit:
            }
        }
    }

    return true;  // Success
}

// Find cell with minimum remaining values (MRV heuristic)
bool findMRVCell(ref CPGrid grid, ref int row, ref int col) {
    int minCandidates = 10;
    bool found = false;

    for (int r = 0; r < 9; r++) {
        for (int c = 0; c < 9; c++) {
            if (grid.values[r][c] == 0) {
                int numCandidates = countCand(grid.candidates[r][c]);
                if (numCandidates < minCandidates) {
                    minCandidates = numCandidates;
                    row = r;
                    col = c;
                    found = true;
                }
            }
        }
    }

    return found;
}

// Deep copy grid for backtracking
CPGrid copyGrid(ref CPGrid grid) {
    CPGrid copy;
    copy.values = grid.values;
    copy.candidates = grid.candidates;
    return copy;
}

// CP Search with backtracking
bool search(ref CPGrid grid) {
    // Find MRV cell
    int mrvRow = 0, mrvCol = 0;
    if (!findMRVCell(grid, mrvRow, mrvCol)) {
        // No empty cells - grid is complete
        return true;
    }

    // Try each candidate for the MRV cell
    CandidateSet candidates = grid.candidates[mrvRow][mrvCol];

    for (int digit = 1; digit <= 9; digit++) {
        if (hasCand(candidates, digit)) {
            // Save grid state for backtracking
            CPGrid gridCopy = copyGrid(grid);

            // Try assigning this digit
            if (assign(grid, mrvRow, mrvCol, digit)) {
                // Propagate constraints
                if (propagate(grid)) {
                    // Recurse
                    if (search(grid)) {
                        return true;  // Found solution
                    }
                }
            }

            // Failed - restore grid state
            grid = gridCopy;
        }
    }

    // All candidates exhausted
    return false;
}

// Parse matrix file
int[][] parseMatrixFile(string filename) {
    int[][] puzzle = new int[][](9, 9);

    try {
        auto file = File(filename, "r");

        // Normalize path for output
        if (filename.length >= 14 && filename[0..14] == "/app/Matrices/") {
            writefln("../%s", filename[5..$]);
        } else {
            writeln(filename);
        }

        int lineCount = 0;
        foreach (line; file.byLine()) {
            string lineStr = line.idup.strip();

            // Skip comments and empty lines
            if (lineStr.length == 0 || lineStr[0] == '#') {
                continue;
            }

            if (lineCount >= 9) break;

            // Parse 9 integers from line
            auto parts = lineStr.split();
            if (parts.length >= 9) {
                for (int i = 0; i < 9; i++) {
                    puzzle[lineCount][i] = to!int(parts[i]);
                    writef("%d ", puzzle[lineCount][i]);
                }
                writeln();
                lineCount++;
            }
        }

        file.close();
    } catch (Exception e) {
        stderr.writefln("Error reading file '%s': %s", filename, e.msg);
        return null;
    }

    return puzzle;
}

// Print puzzle
void printPuzzle(int[][] puzzle, string title) {
    writeln();
    writeln(title);
    for (int row = 0; row < 9; row++) {
        for (int col = 0; col < 9; col++) {
            writef("%d ", puzzle[row][col]);
        }
        writeln();
    }
}

// Print grid values
void printGrid(ref CPGrid grid, string title) {
    writeln();
    writeln(title);
    for (int row = 0; row < 9; row++) {
        for (int col = 0; col < 9; col++) {
            writef("%d ", grid.values[row][col]);
        }
        writeln();
    }
}

void main(string[] args) {
    auto sw = StopWatch(AutoStart.yes);

    // Process each .matrix file from command line
    for (int i = 1; i < args.length; i++) {
        string filename = args[i];
        if (filename.length >= 7 && filename[$-7..$] == ".matrix") {
            int[][] puzzle = parseMatrixFile(filename);
            if (puzzle is null) {
                stderr.writefln("Error reading %s", filename);
                continue;
            }

            printPuzzle(puzzle, "Puzzle:");

            // Initialize grid
            CPGrid grid = initGrid(puzzle);

            // Propagate initial constraints
            cpIterations = 0;
            if (!propagate(grid)) {
                writeln("\nNo solution found (initial propagation failed)\n");
                continue;
            }

            // Search for solution
            if (search(grid)) {
                printGrid(grid, "Puzzle:");
                writefln("\nSolved in Iterations=%d\n", cpIterations);
            } else {
                writeln("\nNo solution found\n");
            }
        }
    }

    sw.stop();
    auto elapsed = sw.peek();
    writefln("Seconds to process %.3f", elapsed.total!"nsecs" / 1_000_000_000.0);
}
