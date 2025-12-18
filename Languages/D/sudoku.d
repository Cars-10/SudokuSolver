import std.stdio;
import std.file;
import std.string;
import std.conv;
import std.datetime.stopwatch;
import core.time;

// Sudoku puzzle grid [row][col]
int[9][9] puzzle;
int count = 0;  // Iteration counter

void printPuzzle() {
    writeln("\nPuzzle:");
    for (int j = 0; j < 9; j++) {
        for (int i = 0; i < 9; i++) {
            writef("%d ", puzzle[j][i]);
        }
        writeln();
    }
}

int readMatrixFile(string filename) {
    File file;
    try {
        file = File(filename, "r");
    } catch (Exception e) {
        stderr.writefln("Error opening file '%s'", filename);
        return 1;
    }

    // Normalize path for output (convert absolute to relative)
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
    return 0;
}

// Check if placing val at (row, col) is valid
bool isValid(int row, int col, int val) {
    // Check row
    for (int i = 0; i < 9; i++) {
        if (puzzle[row][i] == val)
            return false;
    }

    // Check column
    for (int i = 0; i < 9; i++) {
        if (puzzle[i][col] == val)
            return false;
    }

    // Check 3x3 box
    int boxRow = (row / 3) * 3;
    int boxCol = (col / 3) * 3;
    for (int i = 0; i < 3; i++) {
        for (int j = 0; j < 3; j++) {
            if (puzzle[boxRow + i][boxCol + j] == val)
                return false;
        }
    }

    return true;
}

// BRUTE-FORCE SOLVER
// Searches row-major order (top-to-bottom, left-to-right)
// Tries candidates 1-9 in ascending order
// Counts EVERY placement attempt (the algorithm fingerprint)
bool solve() {
    // Find first empty cell (row-major order)
    int row = -1, col = -1;
    outer: for (int r = 0; r < 9; r++) {
        for (int c = 0; c < 9; c++) {
            if (puzzle[r][c] == 0) {
                row = r;
                col = c;
                break outer;
            }
        }
    }

    // If no empty cell found, puzzle is solved
    if (row == -1) {
        printPuzzle();
        writefln("\nSolved in Iterations=%d\n", count);
        return true;  // Success
    }

    // Try values 1-9 in order
    for (int val = 1; val <= 9; val++) {
        count++;  // COUNT EVERY ATTEMPT - this is the algorithm fingerprint

        if (isValid(row, col, val)) {
            puzzle[row][col] = val;  // Place value

            if (solve()) {
                return true;  // Solved
            }

            puzzle[row][col] = 0;  // Backtrack
        }
    }

    return false;  // No solution found
}

void main(string[] args) {
    auto sw = StopWatch(AutoStart.yes);

    // Process each .matrix file from command line
    for (int i = 1; i < args.length; i++) {
        string filename = args[i];
        if (filename.length >= 7 && filename[$-7..$] == ".matrix") {
            if (readMatrixFile(filename) != 0) {
                stderr.writefln("Error reading %s", filename);
                continue;
            }

            printPuzzle();
            count = 0;
            solve();
        }
    }

    sw.stop();
    auto elapsed = sw.peek();
    writefln("Seconds to process %.3f", elapsed.total!"nsecs" / 1_000_000_000.0);
}
