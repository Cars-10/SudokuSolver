import sys.io.File;
using StringTools;

// DLX Node class - circular doubly-linked structure
class DlxNode {
    public var up:Null<DlxNode>;
    public var down:Null<DlxNode>;
    public var left:Null<DlxNode>;
    public var right:Null<DlxNode>;
    public var column:Null<DlxNode>;
    public var size:Int = 0;
    public var rowId:Int = -1;
    public var colId:Int = -1;
    public var name:String = '';

    public function new() {}
}

class DLX {
    static var dlxIterations:Int = 0;

    // Sudoku puzzle grid [row][col]
    static var puzzle:Array<Array<Int>> = [];
    static var solutionGrid:Array<Array<Int>> = [];

    // DLX matrix structures
    static var root:DlxNode;
    static var columns:Array<DlxNode> = [];
    static var nodes:Array<DlxNode> = [];
    static var nodeCount:Int = 0;

    // Row metadata to map DLX rows back to Sudoku (row, col, num)
    static var rowInfo:Array<{row:Int, col:Int, num:Int}> = [];
    static var rowStarts:Array<Null<DlxNode>> = [];

    // Calculate constraint column indices
    static function getPositionCol(r:Int, c:Int):Int {
        return r * 9 + c;
    }

    static function getRowCol(r:Int, n:Int):Int {
        return 81 + r * 9 + (n - 1);
    }

    static function getColCol(c:Int, n:Int):Int {
        return 162 + c * 9 + (n - 1);
    }

    static function getBoxCol(r:Int, c:Int, n:Int):Int {
        var box = Std.int(r / 3) * 3 + Std.int(c / 3);
        return 243 + box * 9 + (n - 1);
    }

    // Cover a column in the DLX matrix
    static function coverColumn(col:DlxNode):Void {
        // Remove column header from the header list
        col.right.left = col.left;
        col.left.right = col.right;

        // For each row in this column
        var rowNode = col.down;
        while (rowNode != col) {
            // For each node in this row (excluding the column itself)
            var rightNode = rowNode.right;
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
    static function uncoverColumn(col:DlxNode):Void {
        // For each row in this column (in reverse order)
        var rowNode = col.up;
        while (rowNode != col) {
            // For each node in this row (in reverse order)
            var leftNode = rowNode.left;
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
        col.right.left = col;
        col.left.right = col;
    }

    // Choose column with minimum size (Knuth's S heuristic)
    static function chooseColumn(root:DlxNode):Null<DlxNode> {
        var best:Null<DlxNode> = null;
        var minSize = 0x7FFFFFFF;

        var colNode = root.right;
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
    static function dlxSearch(root:DlxNode, k:Int, solution:Array<Int>):Bool {
        dlxIterations++;  // Count every search call

        // If matrix is empty, we found a solution
        if (root.right == root) {
            return true;
        }

        // Choose column with minimum size
        var col = chooseColumn(root);

        // If column has no rows, no solution possible
        if (col == null || col.size == 0) {
            return false;
        }

        // Cover this column
        coverColumn(col);

        // Try each row in this column
        var rowNode = col.down;
        while (rowNode != col) {
            // Add row to partial solution
            solution[k] = rowNode.rowId;

            // Cover all other columns in this row
            var rightNode = rowNode.right;
            while (rightNode != rowNode) {
                coverColumn(rightNode.column);
                rightNode = rightNode.right;
            }

            // Recurse
            if (dlxSearch(root, k + 1, solution)) {
                return true;  // Solution found
            }

            // Backtrack: uncover all columns in this row
            var leftNode = rowNode.left;
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

    // Initialize DLX matrix structure
    static function initDlxMatrix():Void {
        // Allocate root column
        root = new DlxNode();
        root.name = 'root';
        root.left = root;
        root.right = root;
        root.up = root;
        root.down = root;
        root.column = root;
        root.rowId = -1;
        root.size = 0;

        // Allocate 324 column headers
        columns = [];
        for (i in 0...324) {
            var col = new DlxNode();
            col.name = 'C$i';
            col.size = 0;
            col.colId = i;

            // Initialize as circular list
            col.up = col;
            col.down = col;
            col.column = col;
            col.rowId = -1;

            // Link into header list
            col.left = root.left;
            col.right = root;
            root.left.right = col;
            root.left = col;

            columns.push(col);
        }

        // Initialize nodes and metadata
        nodes = [];
        rowInfo = [];
        rowStarts = [];
        for (i in 0...729) {
            rowInfo.push({row: 0, col: 0, num: 0});
            rowStarts.push(null);
        }
        nodeCount = 0;
    }

    // Add a node to the DLX matrix
    static function addNode(col:DlxNode, rowId:Int):DlxNode {
        var node = new DlxNode();
        node.column = col;
        node.rowId = rowId;

        // Insert at end of column's circular list
        node.down = col;
        node.up = col.up;
        col.up.down = node;
        col.up = node;
        col.size++;

        nodes.push(node);
        nodeCount++;

        return node;
    }

    // Build a DLX row for Sudoku cell (r,c) with value n
    static function buildDlxRow(r:Int, c:Int, n:Int, rowId:Int):Void {
        // Store row metadata
        rowInfo[rowId] = {row: r, col: c, num: n};

        // Create nodes for the 4 constraints
        var n1 = addNode(columns[getPositionCol(r, c)], rowId);
        var n2 = addNode(columns[getRowCol(r, n)], rowId);
        var n3 = addNode(columns[getColCol(c, n)], rowId);
        var n4 = addNode(columns[getBoxCol(r, c, n)], rowId);

        // Link nodes horizontally in circular list
        n1.right = n2;
        n2.right = n3;
        n3.right = n4;
        n4.right = n1;

        n1.left = n4;
        n2.left = n1;
        n3.left = n2;
        n4.left = n3;

        // Store first node for this row
        rowStarts[rowId] = n1;
    }

    // Build the complete DLX matrix from the puzzle
    static function buildDlxMatrixFromPuzzle():Void {
        var rowId = 0;

        for (r in 0...9) {
            for (c in 0...9) {
                if (puzzle[r][c] != 0) {
                    // Cell has a clue - create only one row for that value
                    buildDlxRow(r, c, puzzle[r][c], rowId++);
                } else {
                    // Cell is empty - create rows for all possible values
                    for (n in 1...10) {
                        buildDlxRow(r, c, n, rowId++);
                    }
                }
            }
        }
    }

    // Cover given clues (pre-selected rows)
    static function coverClues():Void {
        for (r in 0...9) {
            for (c in 0...9) {
                if (puzzle[r][c] != 0) {
                    var n = puzzle[r][c];

                    // Find the row for this clue
                    for (rowId in 0...729) {
                        if (rowStarts[rowId] != null &&
                            rowInfo[rowId].row == r &&
                            rowInfo[rowId].col == c &&
                            rowInfo[rowId].num == n) {
                            // Cover all columns in this row
                            var node = rowStarts[rowId];
                            var curr = node;
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

    // Extract solution from DLX and populate solutionGrid
    static function extractSolution(solution:Array<Int>, solutionLen:Int):Void {
        // Initialize solution grid - start with the original puzzle
        for (r in 0...9) {
            for (c in 0...9) {
                solutionGrid[r][c] = puzzle[r][c];
            }
        }

        // Each solution entry is a row_id
        for (i in 0...solutionLen) {
            var rowId = solution[i];
            if (rowId >= 0 && rowId < 729) {
                solutionGrid[rowInfo[rowId].row][rowInfo[rowId].col] = rowInfo[rowId].num;
            }
        }
    }

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
    static function readMatrixFile(filename:String):Int {
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
                    var row = [];
                    for (i in 0...9) {
                        var val = Std.parseInt(validParts[i]);
                        row.push(val);
                        Sys.print(val);
                        if (i < 8) Sys.print(' ');
                    }
                    Sys.println('');
                    puzzle.push(row);
                    lineCount++;
                }
            }

            return 0;
        } catch (e:Dynamic) {
            Sys.stderr().writeString('Error opening file \'${filename}\'\n');
            return 1;
        }
    }

    static function main():Void {
        var startTime = Sys.time();
        var args = Sys.args();

        // Initialize puzzle and solution grids
        puzzle = [];
        solutionGrid = [for (i in 0...9) [for (j in 0...9) 0]];

        // Process each .matrix file from command line
        for (filename in args) {
            if (filename.indexOf('.matrix') != -1) {
                if (readMatrixFile(filename) != 0) {
                    Sys.stderr().writeString('Error reading $filename\n');
                    continue;
                }

                printPuzzle(puzzle);

                // Initialize DLX matrix
                initDlxMatrix();

                // Build matrix from puzzle
                buildDlxMatrixFromPuzzle();

                // Cover pre-filled clues
                coverClues();

                // Solve using DLX
                dlxIterations = 0;
                var solution = [for (i in 0...81) 0];
                var result = dlxSearch(root, 0, solution);

                if (result) {
                    extractSolution(solution, 81);
                    printPuzzle(solutionGrid);
                    Sys.println('\nSolved in Iterations=$dlxIterations\n');
                } else {
                    Sys.println('\nNo solution found');
                }
            }
        }

        var elapsed = Sys.time() - startTime;
        Sys.println('Seconds to process ${Std.string(elapsed).substr(0, 5)}');
    }
}
