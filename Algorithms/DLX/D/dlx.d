import std.stdio;
import std.algorithm;
import std.conv;
import std.file;
import std.string;
import core.time;
import std.datetime.stopwatch;

// DLX Node class for circular doubly-linked lists
class DlxNode {
    DlxNode left, right, up, down;
    DlxNode column;  // Points to column header
    int size;        // For column headers: count of nodes in column
    int rowId;       // Row identifier for solution tracking

    this() {
        left = right = up = down = this;
        column = this;
        size = 0;
        rowId = -1;
    }
}

// Row metadata to map DLX rows back to Sudoku (row, col, num)
struct RowInfo {
    int row;
    int col;
    int num;
}

RowInfo[729] rowInfo;  // 729 possible rows (9x9x9)
DlxNode[729] rowStarts;  // First node of each row

// Global iteration counter
int dlxIterations = 0;

// Cover a column in the DLX matrix
void coverColumn(DlxNode c) {
    // Remove column header from the header list
    c.right.left = c.left;
    c.left.right = c.right;

    // For each row in this column
    DlxNode rowNode = c.down;
    while (rowNode != c) {
        // For each node in this row (excluding the column itself)
        DlxNode rightNode = rowNode.right;
        while (rightNode != rowNode) {
            // Remove this node from its column
            rightNode.down.up = rightNode.up;
            rightNode.up.down = rightNode.down;
            rightNode.column.size--;
            rightNode = rightNode.right;
        }
        rowNode = rowNode.down;
    }
}

// Uncover a column (exact reverse of cover)
void uncoverColumn(DlxNode c) {
    // For each row in this column (in reverse order)
    DlxNode rowNode = c.up;
    while (rowNode != c) {
        // For each node in this row (in reverse order)
        DlxNode leftNode = rowNode.left;
        while (leftNode != rowNode) {
            // Restore this node to its column
            leftNode.column.size++;
            leftNode.down.up = leftNode;
            leftNode.up.down = leftNode;
            leftNode = leftNode.left;
        }
        rowNode = rowNode.up;
    }

    // Restore column header to the header list
    c.right.left = c;
    c.left.right = c;
}

// Choose column with minimum size (Knuth's S heuristic)
DlxNode chooseColumn(DlxNode root) {
    DlxNode best = null;
    int minSize = int.max;

    DlxNode colNode = root.right;
    while (colNode != root) {
        if (colNode.size < minSize) {
            minSize = colNode.size;
            best = colNode;
        }
        colNode = colNode.right;
    }

    return best;
}

// DLX Search - Algorithm X with Dancing Links
bool dlxSearch(DlxNode root, int k, int[] solution) {
    dlxIterations++;  // Count every search call

    // If matrix is empty, we found a solution
    if (root.right == root) {
        return true;
    }

    // Choose column with minimum size
    DlxNode col = chooseColumn(root);

    // If column has no rows, no solution possible
    if (col.size == 0) {
        return false;
    }

    // Cover this column
    coverColumn(col);

    // Try each row in this column
    DlxNode rowNode = col.down;
    while (rowNode != col) {
        // Add row to partial solution
        solution[k] = rowNode.rowId;

        // Cover all other columns in this row
        DlxNode rightNode = rowNode.right;
        while (rightNode != rowNode) {
            coverColumn(rightNode.column);
            rightNode = rightNode.right;
        }

        // Recurse
        if (dlxSearch(root, k + 1, solution)) {
            return true;  // Solution found
        }

        // Backtrack: uncover all columns in this row
        DlxNode leftNode = rowNode.left;
        while (leftNode != rowNode) {
            uncoverColumn(leftNode.column);
            leftNode = leftNode.left;
        }

        rowNode = rowNode.down;
    }

    // Uncover column
    uncoverColumn(col);

    return false;  // No solution found
}

// Build DLX matrix for Sudoku exact cover problem
DlxNode buildSudokuMatrix(int[][] puzzle) {
    // Sudoku has 324 constraints:
    // - 81 cell constraints (each cell must have a value)
    // - 81 row constraints (each row must have digits 1-9)
    // - 81 column constraints (each column must have digits 1-9)
    // - 81 box constraints (each 3x3 box must have digits 1-9)

    // Create root and 324 column headers
    DlxNode root = new DlxNode();
    DlxNode[] columns = new DlxNode[324];

    // Initialize column headers
    for (int i = 0; i < 324; i++) {
        columns[i] = new DlxNode();
        columns[i].size = 0;

        // Link into header list
        columns[i].left = root.left;
        columns[i].right = root;
        root.left.right = columns[i];
        root.left = columns[i];
    }

    // Add rows for each possible placement (row, col, digit)
    int rowId = 0;
    for (int row = 0; row < 9; row++) {
        for (int col = 0; col < 9; col++) {
            // If cell is given, only add that constraint
            int startDigit = (puzzle[row][col] != 0) ? puzzle[row][col] : 1;
            int endDigit = (puzzle[row][col] != 0) ? puzzle[row][col] : 9;

            for (int digit = startDigit; digit <= endDigit; digit++) {
                // Store row metadata
                rowInfo[rowId].row = row;
                rowInfo[rowId].col = col;
                rowInfo[rowId].num = digit;

                // Calculate constraint column indices
                int cellConstraint = row * 9 + col;
                int rowConstraint = 81 + row * 9 + (digit - 1);
                int colConstraint = 162 + col * 9 + (digit - 1);
                int boxConstraint = 243 + ((row / 3) * 3 + (col / 3)) * 9 + (digit - 1);

                int[] constraints = [cellConstraint, rowConstraint, colConstraint, boxConstraint];

                // Create nodes for this row
                DlxNode[] nodes = new DlxNode[4];
                for (int i = 0; i < 4; i++) {
                    nodes[i] = new DlxNode();
                    nodes[i].rowId = rowId;
                    nodes[i].column = columns[constraints[i]];

                    // Link into column
                    nodes[i].up = columns[constraints[i]].up;
                    nodes[i].down = columns[constraints[i]];
                    columns[constraints[i]].up.down = nodes[i];
                    columns[constraints[i]].up = nodes[i];
                    columns[constraints[i]].size++;
                }

                // Link nodes horizontally (circular)
                for (int i = 0; i < 4; i++) {
                    nodes[i].left = nodes[(i + 3) % 4];
                    nodes[i].right = nodes[(i + 1) % 4];
                }

                // Store first node for this row
                rowStarts[rowId] = nodes[0];

                rowId++;
            }
        }
    }

    return root;
}

// Cover given clues (pre-selected rows)
void coverClues(DlxNode root, int[][] puzzle) {
    for (int r = 0; r < 9; r++) {
        for (int c = 0; c < 9; c++) {
            if (puzzle[r][c] != 0) {
                int n = puzzle[r][c];

                // Find the row for this clue
                for (int rowId = 0; rowId < 729; rowId++) {
                    if (rowStarts[rowId] !is null &&
                        rowInfo[rowId].row == r &&
                        rowInfo[rowId].col == c &&
                        rowInfo[rowId].num == n) {

                        // Cover all columns in this row
                        DlxNode node = rowStarts[rowId];
                        DlxNode curr = node;
                        do {
                            coverColumn(curr.column);
                            curr = curr.right;
                        } while (curr != node);
                        break;
                    }
                }
            }
        }
    }
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

// Extract solution from DLX result
int[][] extractSolution(int[][] puzzle, int[] solutionRows) {
    int[][] result = new int[][](9, 9);

    // Copy original puzzle (includes clues)
    for (int r = 0; r < 9; r++) {
        for (int c = 0; c < 9; c++) {
            result[r][c] = puzzle[r][c];
        }
    }

    // Each solution entry is a row_id pointing to rowInfo
    for (int i = 0; i < 81; i++) {
        int rowId = solutionRows[i];
        if (rowId >= 0 && rowId < 729) {
            result[rowInfo[rowId].row][rowInfo[rowId].col] = rowInfo[rowId].num;
        }
    }

    return result;
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

            // Build DLX matrix
            DlxNode root = buildSudokuMatrix(puzzle);

            // Cover pre-filled clues
            coverClues(root, puzzle);

            // Solve
            dlxIterations = 0;
            int[] solution = new int[81];
            if (dlxSearch(root, 0, solution)) {
                int[][] solvedPuzzle = extractSolution(puzzle, solution);
                printPuzzle(solvedPuzzle, "Puzzle:");

                writefln("\nSolved in Iterations=%d\n", dlxIterations);
            } else {
                writeln("\nNo solution found\n");
            }
        }
    }

    sw.stop();
    auto elapsed = sw.peek();
    writefln("Seconds to process %.3f", elapsed.total!"nsecs" / 1_000_000_000.0);
}
