import sys.io.File;
using StringTools;

// ============================================================================
// DATA STRUCTURES
// ============================================================================

// Grid structure with assigned values and candidate tracking
class CPGrid {
    public var values:Array<Array<Int>>;
    public var candidates:Array<Array<Int>>;  // Bitsets (bits 1-9 for digits 1-9)

    public function new() {
        values = [for (i in 0...9) [for (j in 0...9) 0]];
        candidates = [for (i in 0...9) [for (j in 0...9) 0]];
    }

    // Deep copy
    public function copy():CPGrid {
        var result = new CPGrid();
        for (r in 0...9) {
            for (c in 0...9) {
                result.values[r][c] = values[r][c];
                result.candidates[r][c] = candidates[r][c];
            }
        }
        return result;
    }
}

class CP {
    // ============================================================================
    // GLOBAL ITERATION COUNTER
    // ============================================================================
    static var cpIterations:Int = 0;

    // ============================================================================
    // CANDIDATE MANIPULATION
    // ============================================================================

    static inline function hasCandidate(set:Int, digit:Int):Bool {
        return (set & (1 << digit)) != 0;
    }

    static inline function removeCandidate(set:Int, digit:Int):Int {
        return set & ~(1 << digit);
    }

    static inline function addCandidate(set:Int, digit:Int):Int {
        return set | (1 << digit);
    }

    static function countCandidates(set:Int):Int {
        var count = 0;
        for (i in 0...16) {
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
    static function getFirstCandidate(cs:Int):Int {
        for (digit in 1...10) {
            if (hasCandidate(cs, digit)) {
                return digit;
            }
        }
        return 0;
    }

    // Get all 20 peers for a cell (row, col, box)
    static function getPeers(row:Int, col:Int):Array<Array<Int>> {
        var peers:Array<Array<Int>> = [];

        // Same row (9 cells minus self = 8)
        for (c in 0...9) {
            if (c != col) {
                peers.push([row, c]);
            }
        }

        // Same column (9 cells minus self = 8)
        for (r in 0...9) {
            if (r != row) {
                peers.push([r, col]);
            }
        }

        // Same 3x3 box (9 cells minus self minus already counted = 4)
        var boxRow = Std.int(row / 3) * 3;
        var boxCol = Std.int(col / 3) * 3;
        for (r in boxRow...(boxRow + 3)) {
            for (c in boxCol...(boxCol + 3)) {
                if (r != row && c != col) {
                    peers.push([r, c]);
                }
            }
        }

        return peers;
    }

    // ============================================================================
    // INITIALIZATION
    // ============================================================================

    static function initGrid(grid:CPGrid, puzzle:Array<Array<Int>>):Void {
        for (row in 0...9) {
            for (col in 0...9) {
                if (puzzle[row][col] == 0) {
                    // Empty cell: set all candidates 1-9 (bits 1-9 set)
                    grid.values[row][col] = 0;
                    grid.candidates[row][col] = 0x3FE;  // Binary: 0011 1111 1110 (bits 1-9)
                } else {
                    // Given clue: set single value
                    var digit = puzzle[row][col];
                    grid.values[row][col] = digit;
                    grid.candidates[row][col] = (1 << digit);
                }
            }
        }
    }

    // ============================================================================
    // CONSTRAINT PROPAGATION
    // ============================================================================

    static function eliminate(grid:CPGrid, row:Int, col:Int, digit:Int):Bool {
        // Check if digit is already eliminated
        if (!hasCandidate(grid.candidates[row][col], digit)) {
            return true;  // Already eliminated, no change
        }

        // Remove digit from candidates
        grid.candidates[row][col] = removeCandidate(grid.candidates[row][col], digit);

        // Check for contradiction (no candidates left)
        var remaining = countCandidates(grid.candidates[row][col]);
        if (remaining == 0) {
            return false;  // Contradiction
        }

        // If only one candidate left, assign it (singleton elimination)
        if (remaining == 1 && grid.values[row][col] == 0) {
            var lastDigit = getFirstCandidate(grid.candidates[row][col]);
            if (!assign(grid, row, col, lastDigit)) {
                return false;  // Assignment caused contradiction
            }
        }

        return true;
    }

    static function assign(grid:CPGrid, row:Int, col:Int, digit:Int):Bool {
        // Increment iteration counter (this is our benchmark metric)
        cpIterations++;

        // Set value
        grid.values[row][col] = digit;
        grid.candidates[row][col] = (1 << digit);

        // Eliminate digit from all peers
        var peers = getPeers(row, col);

        for (peer in peers) {
            var peerRow = peer[0];
            var peerCol = peer[1];

            if (!eliminate(grid, peerRow, peerCol, digit)) {
                return false;  // Contradiction in peer elimination
            }
        }

        return true;
    }

    static function propagate(grid:CPGrid):Bool {
        var changed = true;

        while (changed) {
            changed = false;

            // Strategy 1: Singleton elimination
            // If a cell has only one candidate, assign it
            for (row in 0...9) {
                for (col in 0...9) {
                    if (grid.values[row][col] == 0) {
                        var numCandidates = countCandidates(grid.candidates[row][col]);
                        if (numCandidates == 0) {
                            return false;  // Contradiction
                        }
                        if (numCandidates == 1) {
                            var digit = getFirstCandidate(grid.candidates[row][col]);
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
            for (row in 0...9) {
                for (digit in 1...10) {
                    var count = 0;
                    var lastCol = -1;
                    var alreadyAssigned = false;
                    for (col in 0...9) {
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
            for (col in 0...9) {
                for (digit in 1...10) {
                    var count = 0;
                    var lastRow = -1;
                    var alreadyAssigned = false;
                    for (row in 0...9) {
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
            for (box in 0...9) {
                var boxRow = Std.int(box / 3) * 3;
                var boxCol = (box % 3) * 3;

                for (digit in 1...10) {
                    var count = 0;
                    var lastR = -1;
                    var lastC = -1;
                    var alreadyAssigned = false;

                    for (r in boxRow...(boxRow + 3)) {
                        for (c in boxCol...(boxCol + 3)) {
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

    static function findMrvCell(grid:CPGrid):Null<{row:Int, col:Int}> {
        var minCandidates = 10;  // More than 9, so any cell will be smaller
        var bestRow = -1;
        var bestCol = -1;

        for (r in 0...9) {
            for (c in 0...9) {
                if (grid.values[r][c] == 0) {
                    var numCandidates = countCandidates(grid.candidates[r][c]);
                    if (numCandidates < minCandidates) {
                        minCandidates = numCandidates;
                        bestRow = r;
                        bestCol = c;
                    }
                }
            }
        }

        if (bestRow == -1) {
            return null;  // No empty cells (grid complete)
        }
        return {row: bestRow, col: bestCol};
    }

    static function cpSearch(grid:CPGrid, solution:Array<Int>):Bool {
        // Base case: check if grid is complete
        var mrvCell = findMrvCell(grid);
        if (mrvCell == null) {
            // No empty cells - grid is complete, extract solution
            for (r in 0...9) {
                for (c in 0...9) {
                    solution[r * 9 + c] = grid.values[r][c];
                }
            }
            return true;  // Solved
        }

        // Recursive case: try each candidate for the MRV cell
        var mrvRow = mrvCell.row;
        var mrvCol = mrvCell.col;
        var candidates = grid.candidates[mrvRow][mrvCol];

        for (digit in 1...10) {
            if (hasCandidate(candidates, digit)) {
                // Save grid state for backtracking
                var gridCopy = grid.copy();

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
                grid.values = gridCopy.values;
                grid.candidates = gridCopy.candidates;
            }
        }

        // All candidates exhausted - dead end
        return false;
    }

    // ============================================================================
    // INPUT/OUTPUT
    // ============================================================================

    // Print puzzle
    static function printPuzzle(grid:Array<Array<Int>>):Void {
        Sys.println('\nPuzzle:');
        for (r in 0...9) {
            var line = '';
            for (c in 0...9) {
                if (c > 0) line += ' ';
                line += Std.string(grid[r][c]);
            }
            Sys.println(line);
        }
    }

    // Read matrix file
    static function readMatrixFile(filename:String, puzzle:Array<Array<Int>>):Bool {
        try {
            var content = File.getContent(filename);
            var lines = content.split("\n");

            // Normalize path for output
            if (filename.indexOf('/app/Matrices/') == 0) {
                var displayPath = filename.substr(5);  // Skip "/app/" to get "Matrices/..."
                Sys.println('../${displayPath}');
            } else {
                Sys.println(filename);
            }

            var lineCount = 0;
            for (line in lines) {
                var lineStr = line.trim();

                // Skip comments and empty lines
                if (lineStr.length == 0 || lineStr.charAt(0) == '#') {
                    continue;
                }

                if (lineCount >= 9) break;

                // Parse 9 integers from line
                var parts = lineStr.split(' ');
                var validParts = [];
                for (part in parts) {
                    if (part.length > 0) {
                        validParts.push(part);
                    }
                }

                if (validParts.length >= 9) {
                    for (i in 0...9) {
                        var val = Std.parseInt(validParts[i]);
                        puzzle[lineCount][i] = val;
                        Sys.print(val);
                        if (i < 8) Sys.print(' ');
                    }
                    Sys.println('');
                    lineCount++;
                }
            }

            return true;
        } catch (e:Dynamic) {
            Sys.stderr().writeString('Error opening file \'${filename}\'\n');
            return false;
        }
    }

    // ============================================================================
    // MAIN
    // ============================================================================

    static function main():Void {
        var startTime = Sys.time();
        var args = Sys.args();

        // Process each .matrix file from command line
        for (filename in args) {
            if (filename.indexOf('.matrix') != -1) {
                var puzzle:Array<Array<Int>> = [for (i in 0...9) [for (j in 0...9) 0]];

                if (!readMatrixFile(filename, puzzle)) {
                    Sys.stderr().writeString('Error reading $filename\n');
                    continue;
                }

                printPuzzle(puzzle);

                // Initialize grid with puzzle
                var grid = new CPGrid();
                initGrid(grid, puzzle);

                // Propagate initial constraints
                if (!propagate(grid)) {
                    Sys.println('\nNo solution found (initial propagation failed)');
                    continue;
                }

                // Solve using CP search
                cpIterations = 0;
                var solution = [for (i in 0...81) 0];
                var result = cpSearch(grid, solution);

                if (result) {
                    var solutionGrid:Array<Array<Int>> = [for (i in 0...9) [for (j in 0...9) 0]];
                    for (r in 0...9) {
                        for (c in 0...9) {
                            solutionGrid[r][c] = solution[r * 9 + c];
                        }
                    }
                    printPuzzle(solutionGrid);
                    Sys.println('\nSolved in Iterations=$cpIterations\n');
                } else {
                    Sys.println('\nNo solution found');
                }
            }
        }

        var elapsed = Sys.time() - startTime;
        Sys.println('Seconds to process ${Std.string(elapsed).substr(0, 5)}');
    }
}
