/**
 * Dancing Links (DLX) Sudoku Solver - Java Implementation
 * Port of Algorithm X with Dancing Links from C reference
 *
 * Algorithm X: Exact cover problem solver using Knuth's Dancing Links technique
 * Sudoku as Exact Cover: 324 constraints (81 cells, 81 rows, 81 cols, 81 boxes)
 * Expected iterations for Matrix 1: 43
 */

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class DLX {
    // ========================================================================
    // DATA STRUCTURES
    // ========================================================================

    /**
     * DlxNode - represents a 1 in the exact cover matrix
     * Circular doubly-linked in 4 directions (up, down, left, right)
     */
    static class DlxNode {
        DlxNode up, down, left, right;
        DlxColumn column;  // Pointer to column header
        int rowId;         // ID of the row this node belongs to

        DlxNode() {
            up = down = left = right = this;
        }
    }

    /**
     * DlxColumn - column header (extends DlxNode)
     * Tracks number of nodes in column and provides name for debugging
     */
    static class DlxColumn extends DlxNode {
        int size;       // Number of nodes in this column
        String name;    // For debugging/identification

        DlxColumn(String name) {
            super();
            this.name = name;
            this.size = 0;
            this.column = this;
        }
    }

    /**
     * RowInfo - metadata to map DLX rows back to Sudoku placements
     */
    static class RowInfo {
        int row, col, num;

        RowInfo(int row, int col, int num) {
            this.row = row;
            this.col = col;
            this.num = num;
        }
    }

    // ========================================================================
    // GLOBAL STATE
    // ========================================================================

    private static int[][] puzzle = new int[9][9];
    private static int[][] solutionGrid = new int[9][9];
    private static int dlxIterations = 0;

    private static DlxColumn root;
    private static DlxColumn[] columns = new DlxColumn[324];
    private static RowInfo[] rowInfo = new RowInfo[729];
    private static DlxNode[] rowStarts = new DlxNode[729];

    // ========================================================================
    // CONSTRAINT COLUMN CALCULATIONS
    // ========================================================================

    /**
     * Calculate constraint column indices for Sudoku cell (r,c) with value n:
     * - Position constraint: (r,c) must be filled -> column: r*9 + c
     * - Row constraint: row r must have number n -> column: 81 + r*9 + (n-1)
     * - Column constraint: column c must have number n -> column: 162 + c*9 + (n-1)
     * - Box constraint: box b must have number n -> column: 243 + b*9 + (n-1)
     */

    private static int getPositionCol(int r, int c) {
        return r * 9 + c;
    }

    private static int getRowCol(int r, int n) {
        return 81 + r * 9 + (n - 1);
    }

    private static int getColCol(int c, int n) {
        return 162 + c * 9 + (n - 1);
    }

    private static int getBoxCol(int r, int c, int n) {
        int box = (r / 3) * 3 + (c / 3);
        return 243 + box * 9 + (n - 1);
    }

    // ========================================================================
    // DLX MATRIX INITIALIZATION
    // ========================================================================

    private static void initDlxMatrix() {
        // Create root column
        root = new DlxColumn("root");
        root.rowId = -1;

        // Create 324 column headers
        for (int i = 0; i < 324; i++) {
            columns[i] = new DlxColumn("C" + i);
            columns[i].rowId = -1;

            // Link into header list (insert at end, before root)
            columns[i].left = root.left;
            columns[i].right = root;
            root.left.right = columns[i];
            root.left = columns[i];
        }
    }

    private static DlxNode addNode(DlxColumn col, int rowId) {
        DlxNode node = new DlxNode();
        node.column = col;
        node.rowId = rowId;

        // Insert at end of column's circular list (before column header)
        node.down = col;
        node.up = col.up;
        col.up.down = node;
        col.up = node;
        col.size++;

        return node;
    }

    /**
     * Build a DLX row for Sudoku cell (r,c) with value n
     * Creates 4 nodes (one for each constraint) linked horizontally
     */
    private static void buildDlxRow(int r, int c, int n, int rowId) {
        // Store row metadata
        rowInfo[rowId] = new RowInfo(r, c, n);

        // Create nodes for the 4 constraints
        DlxNode n1 = addNode(columns[getPositionCol(r, c)], rowId);
        DlxNode n2 = addNode(columns[getRowCol(r, n)], rowId);
        DlxNode n3 = addNode(columns[getColCol(c, n)], rowId);
        DlxNode n4 = addNode(columns[getBoxCol(r, c, n)], rowId);

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

    /**
     * Build complete DLX matrix from puzzle
     * For clue cells: create 1 row with that value
     * For empty cells: create 9 rows (one for each possible value)
     */
    private static void buildDlxMatrixFromPuzzle() {
        int rowId = 0;

        for (int r = 0; r < 9; r++) {
            for (int c = 0; c < 9; c++) {
                if (puzzle[r][c] != 0) {
                    // Cell has a clue - create only one row
                    buildDlxRow(r, c, puzzle[r][c], rowId++);
                } else {
                    // Cell is empty - create rows for all possible values
                    for (int n = 1; n <= 9; n++) {
                        buildDlxRow(r, c, n, rowId++);
                    }
                }
            }
        }
    }

    /**
     * Cover clues: pre-select rows corresponding to given values
     * Covers all 4 constraints for each clue
     */
    private static void coverClues() {
        for (int r = 0; r < 9; r++) {
            for (int c = 0; c < 9; c++) {
                if (puzzle[r][c] != 0) {
                    int n = puzzle[r][c];

                    // Find the row for this clue
                    for (int rowId = 0; rowId < 729; rowId++) {
                        if (rowStarts[rowId] != null &&
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

    // ========================================================================
    // DANCING LINKS OPERATIONS
    // ========================================================================

    /**
     * Cover a column in the DLX matrix
     * Remove column header and all rows containing this column
     * This is the "dancing" part - links can be restored exactly
     */
    private static void coverColumn(DlxColumn c) {
        // Remove column header from header list
        c.right.left = c.left;
        c.left.right = c.right;

        // For each row in this column
        for (DlxNode rowNode = c.down; rowNode != c; rowNode = rowNode.down) {
            // For each node in this row (excluding the column itself)
            for (DlxNode rightNode = rowNode.right; rightNode != rowNode; rightNode = rightNode.right) {
                // Remove this node from its column
                rightNode.down.up = rightNode.up;
                rightNode.up.down = rightNode.down;
                rightNode.column.size--;
            }
        }
    }

    /**
     * Uncover a column (exact reverse of cover)
     * Must be called in reverse order of cover for correctness
     */
    private static void uncoverColumn(DlxColumn c) {
        // For each row in this column (in reverse order)
        for (DlxNode rowNode = c.up; rowNode != c; rowNode = rowNode.up) {
            // For each node in this row (in reverse order)
            for (DlxNode leftNode = rowNode.left; leftNode != rowNode; leftNode = leftNode.left) {
                // Restore this node to its column
                leftNode.column.size++;
                leftNode.down.up = leftNode;
                leftNode.up.down = leftNode;
            }
        }

        // Restore column header to header list
        c.right.left = c;
        c.left.right = c;
    }

    /**
     * Choose column with minimum size (Knuth's S heuristic)
     * Minimizes branching factor
     */
    private static DlxColumn chooseColumn() {
        DlxColumn best = null;
        int minSize = Integer.MAX_VALUE;

        for (DlxNode colNode = root.right; colNode != root; colNode = colNode.right) {
            DlxColumn col = (DlxColumn) colNode;
            if (col.size < minSize) {
                minSize = col.size;
                best = col;
            }
        }

        return best;
    }

    /**
     * DLX Search - Algorithm X with Dancing Links
     * Recursive backtracking search for exact cover
     * Returns true if solution found, false otherwise
     */
    private static boolean dlxSearch(int k, int[] solution) {
        dlxIterations++;  // Count every search call

        // If matrix is empty, we found a solution
        if (root.right == root) {
            return true;
        }

        // Choose column with minimum size
        DlxColumn col = chooseColumn();

        // If column has no rows, no solution possible
        if (col.size == 0) {
            return false;
        }

        // Cover this column
        coverColumn(col);

        // Try each row in this column
        for (DlxNode rowNode = col.down; rowNode != col; rowNode = rowNode.down) {
            // Add row to partial solution
            solution[k] = rowNode.rowId;

            // Cover all other columns in this row
            for (DlxNode rightNode = rowNode.right; rightNode != rowNode; rightNode = rightNode.right) {
                coverColumn(rightNode.column);
            }

            // Recurse
            if (dlxSearch(k + 1, solution)) {
                return true;  // Solution found
            }

            // Backtrack: uncover all columns in this row (in reverse order)
            for (DlxNode leftNode = rowNode.left; leftNode != rowNode; leftNode = leftNode.left) {
                uncoverColumn(leftNode.column);
            }
        }

        // Uncover column
        uncoverColumn(col);

        return false;  // No solution found
    }

    // ========================================================================
    // SOLUTION EXTRACTION
    // ========================================================================

    /**
     * Extract solution from DLX and populate solutionGrid
     */
    private static void extractSolution(int[] solution, int solutionLen) {
        // Initialize solution grid with original puzzle (includes clues)
        for (int r = 0; r < 9; r++) {
            for (int c = 0; c < 9; c++) {
                solutionGrid[r][c] = puzzle[r][c];
            }
        }

        // Fill in solution values
        for (int i = 0; i < solutionLen; i++) {
            int rowId = solution[i];
            if (rowId >= 0 && rowId < 729 && rowInfo[rowId] != null) {
                RowInfo info = rowInfo[rowId];
                solutionGrid[info.row][info.col] = info.num;
            }
        }
    }

    // ========================================================================
    // PUZZLE I/O
    // ========================================================================

    private static void printPuzzle(int[][] grid) {
        System.out.println("\nPuzzle:");
        for (int r = 0; r < 9; r++) {
            StringBuilder sb = new StringBuilder();
            for (int c = 0; c < 9; c++) {
                sb.append(grid[r][c]).append(" ");
            }
            System.out.println(sb.toString());
        }
    }

    private static void readMatrixFile(String filename) {
        // Normalize path for output (match C format)
        String displayPath = filename;
        if (filename.startsWith("/app/Matrices/")) {
            displayPath = "../" + filename.substring(5);
        }
        System.out.println(displayPath);

        try (BufferedReader br = new BufferedReader(new FileReader(filename))) {
            String line;
            int lineCount = 0;

            while ((line = br.readLine()) != null && lineCount < 9) {
                line = line.trim();
                // Skip comments and empty lines
                if (line.isEmpty() || line.startsWith("#")) continue;

                // Parse 9 integers from line
                String[] parts = line.split("\\s+");
                if (parts.length == 9) {
                    StringBuilder sb = new StringBuilder();
                    for (int j = 0; j < 9; j++) {
                        puzzle[lineCount][j] = Integer.parseInt(parts[j]);
                        sb.append(puzzle[lineCount][j]).append(" ");
                    }
                    System.out.println(sb.toString());
                    lineCount++;
                }
            }
        } catch (IOException | NumberFormatException e) {
            System.err.println("Error reading file: " + e.getMessage());
            System.exit(1);
        }
    }

    // ========================================================================
    // MAIN ENTRYPOINT
    // ========================================================================

    public static void main(String[] args) {
        long startTime = System.nanoTime();

        // Process each .matrix file from command line
        for (String arg : args) {
            if (!arg.endsWith(".matrix")) continue;

            // Reset puzzle
            puzzle = new int[9][9];
            readMatrixFile(arg);
            printPuzzle(puzzle);

            // Initialize DLX matrix
            initDlxMatrix();

            // Build matrix from puzzle
            buildDlxMatrixFromPuzzle();

            // Cover pre-filled clues
            coverClues();

            // Solve using DLX
            dlxIterations = 0;
            int[] solution = new int[81];
            boolean result = dlxSearch(0, solution);

            if (result) {
                extractSolution(solution, 81);
                printPuzzle(solutionGrid);
                System.out.println("\nSolved in Iterations=" + dlxIterations + "\n");
            } else {
                System.out.println("\nNo solution found\n");
            }
        }

        double elapsed = (System.nanoTime() - startTime) / 1_000_000_000.0;
        System.out.printf("Seconds to process %.3f%n", elapsed);
    }
}
