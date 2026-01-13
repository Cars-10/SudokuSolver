using System;
using System.IO;

// Dancing Links (DLX) Sudoku Solver
// Direct port of C implementation to maintain algorithmic compatibility

class DlxNode {
    public DlxNode Up;
    public DlxNode Down;
    public DlxNode Left;
    public DlxNode Right;
    public DlxColumn Column;
    public int RowId;
}

class DlxColumn : DlxNode {
    public int Size;
    public string Name;

    public DlxColumn() {
        Column = this;
    }
}

class RowInfo {
    public int Row;
    public int Col;
    public int Num;
}

class DlxSolver {
    // Global iteration counter
    static int dlxIterations = 0;

    // DLX matrix structures
    DlxColumn root;
    DlxColumn[] columns;
    DlxNode[] nodes;
    int nodeCount;
    int maxNodes;

    // Row metadata
    RowInfo[] rowInfo;
    DlxNode[] rowStarts;

    // Puzzle state
    int[,] puzzle;
    int[,] solutionGrid;

    public DlxSolver() {
        puzzle = new int[9, 9];
        solutionGrid = new int[9, 9];
        columns = new DlxColumn[324];
        maxNodes = 729 * 4;
        nodes = new DlxNode[maxNodes];
        nodeCount = 0;
        rowInfo = new RowInfo[729];
        rowStarts = new DlxNode[729];

        for (int i = 0; i < 729; i++) {
            rowInfo[i] = new RowInfo();
        }
    }

    // Calculate constraint column indices
    int GetPositionCol(int r, int c) {
        return r * 9 + c;
    }

    int GetRowCol(int r, int n) {
        return 81 + r * 9 + (n - 1);
    }

    int GetColCol(int c, int n) {
        return 162 + c * 9 + (n - 1);
    }

    int GetBoxCol(int r, int c, int n) {
        int box = (r / 3) * 3 + (c / 3);
        return 243 + box * 9 + (n - 1);
    }

    // Initialize DLX matrix structure
    void InitDlxMatrix() {
        // Allocate root column
        root = new DlxColumn();
        root.Name = "root";
        root.Left = root;
        root.Right = root;
        root.Up = root;
        root.Down = root;
        root.RowId = -1;
        root.Size = 0;

        // Allocate 324 column headers
        for (int i = 0; i < 324; i++) {
            columns[i] = new DlxColumn();
            columns[i].Name = $"C{i}";
            columns[i].Size = 0;

            // Initialize as circular list
            columns[i].Up = columns[i];
            columns[i].Down = columns[i];
            columns[i].RowId = -1;

            // Link into header list
            columns[i].Left = root.Left;
            columns[i].Right = root;
            root.Left.Right = columns[i];
            root.Left = columns[i];
        }

        // Initialize node pool
        for (int i = 0; i < maxNodes; i++) {
            nodes[i] = new DlxNode();
        }
        nodeCount = 0;
    }

    // Add a node to the DLX matrix
    DlxNode AddNode(DlxColumn col, int rowId) {
        if (nodeCount >= maxNodes) {
            throw new Exception("Exceeded maximum node count");
        }

        DlxNode node = nodes[nodeCount++];
        node.Column = col;
        node.RowId = rowId;

        // Insert at end of column's circular list
        node.Down = col;
        node.Up = col.Up;
        col.Up.Down = node;
        col.Up = node;
        col.Size++;

        return node;
    }

    // Build a DLX row for Sudoku cell (r,c) with value n
    void BuildDlxRow(int r, int c, int n, int rowId) {
        // Store row metadata
        rowInfo[rowId].Row = r;
        rowInfo[rowId].Col = c;
        rowInfo[rowId].Num = n;

        // Create nodes for the 4 constraints
        DlxNode n1 = AddNode(columns[GetPositionCol(r, c)], rowId);
        DlxNode n2 = AddNode(columns[GetRowCol(r, n)], rowId);
        DlxNode n3 = AddNode(columns[GetColCol(c, n)], rowId);
        DlxNode n4 = AddNode(columns[GetBoxCol(r, c, n)], rowId);

        // Link nodes horizontally in circular list
        n1.Right = n2;
        n2.Right = n3;
        n3.Right = n4;
        n4.Right = n1;

        n1.Left = n4;
        n2.Left = n1;
        n3.Left = n2;
        n4.Left = n3;

        // Store first node for this row
        rowStarts[rowId] = n1;
    }

    // Build the complete DLX matrix from the puzzle
    void BuildDlxMatrixFromPuzzle() {
        int rowId = 0;

        for (int r = 0; r < 9; r++) {
            for (int c = 0; c < 9; c++) {
                if (puzzle[r, c] != 0) {
                    // Cell has a clue - create only one row for that value
                    BuildDlxRow(r, c, puzzle[r, c], rowId++);
                } else {
                    // Cell is empty - create rows for all possible values
                    for (int n = 1; n <= 9; n++) {
                        BuildDlxRow(r, c, n, rowId++);
                    }
                }
            }
        }
    }

    // Cover a column in the DLX matrix
    void CoverColumn(DlxColumn c) {
        // Remove column header from the header list
        c.Right.Left = c.Left;
        c.Left.Right = c.Right;

        // For each row in this column
        DlxNode rowNode = c.Down;
        while (rowNode != c) {
            // For each node in this row (excluding the column itself)
            DlxNode rightNode = rowNode.Right;
            while (rightNode != rowNode) {
                // Remove this node from its column
                rightNode.Down.Up = rightNode.Up;
                rightNode.Up.Down = rightNode.Down;
                rightNode.Column.Size--;
                rightNode = rightNode.Right;
            }
            rowNode = rowNode.Down;
        }
    }

    // Uncover a column (exact reverse of cover)
    void UncoverColumn(DlxColumn c) {
        // For each row in this column (in reverse order)
        DlxNode rowNode = c.Up;
        while (rowNode != c) {
            // For each node in this row (in reverse order)
            DlxNode leftNode = rowNode.Left;
            while (leftNode != rowNode) {
                // Restore this node to its column
                leftNode.Column.Size++;
                leftNode.Down.Up = leftNode;
                leftNode.Up.Down = leftNode;
                leftNode = leftNode.Left;
            }
            rowNode = rowNode.Up;
        }

        // Restore column header to the header list
        c.Right.Left = c;
        c.Left.Right = c;
    }

    // Choose column with minimum size (Knuth's S heuristic)
    DlxColumn ChooseColumn() {
        DlxColumn best = null;
        int minSize = int.MaxValue;

        DlxNode colNode = root.Right;
        while (colNode != root) {
            DlxColumn col = (DlxColumn)colNode;
            if (col.Size < minSize) {
                minSize = col.Size;
                best = col;
            }
            colNode = colNode.Right;
        }

        return best;
    }

    // DLX Search - Algorithm X with Dancing Links
    bool DlxSearch(int k, int[] solution) {
        dlxIterations++;  // Count every search call

        // If matrix is empty, we found a solution
        if (root.Right == root) {
            return true;
        }

        // Choose column with minimum size
        DlxColumn col = ChooseColumn();

        // If column has no rows, no solution possible
        if (col.Size == 0) {
            return false;
        }

        // Cover this column
        CoverColumn(col);

        // Try each row in this column
        DlxNode rowNode = col.Down;
        while (rowNode != col) {
            // Add row to partial solution
            solution[k] = rowNode.RowId;

            // Cover all other columns in this row
            DlxNode rightNode = rowNode.Right;
            while (rightNode != rowNode) {
                CoverColumn(rightNode.Column);
                rightNode = rightNode.Right;
            }

            // Recurse
            if (DlxSearch(k + 1, solution)) {
                return true;  // Solution found
            }

            // Backtrack: uncover all columns in this row
            DlxNode leftNode = rowNode.Left;
            while (leftNode != rowNode) {
                UncoverColumn(leftNode.Column);
                leftNode = leftNode.Left;
            }

            rowNode = rowNode.Down;
        }

        // Uncover column
        UncoverColumn(col);

        return false;  // No solution found
    }

    // Cover given clues (pre-selected rows)
    void CoverClues() {
        for (int r = 0; r < 9; r++) {
            for (int c = 0; c < 9; c++) {
                if (puzzle[r, c] != 0) {
                    int n = puzzle[r, c];

                    // Find the row for this clue
                    for (int rowId = 0; rowId < 729; rowId++) {
                        if (rowStarts[rowId] != null &&
                            rowInfo[rowId].Row == r &&
                            rowInfo[rowId].Col == c &&
                            rowInfo[rowId].Num == n) {

                            // Cover all columns in this row
                            DlxNode node = rowStarts[rowId];
                            DlxNode curr = node;
                            do {
                                CoverColumn(curr.Column);
                                curr = curr.Right;
                            } while (curr != node);
                            break;
                        }
                    }
                }
            }
        }
    }

    // Extract solution from DLX and populate solution grid
    void ExtractSolution(int[] solution, int solutionLen) {
        // Initialize solution grid - start with the original puzzle
        Array.Copy(puzzle, solutionGrid, puzzle.Length);

        // Each solution entry is a row_id
        for (int i = 0; i < solutionLen; i++) {
            int rowId = solution[i];
            if (rowId >= 0 && rowId < 729) {
                solutionGrid[rowInfo[rowId].Row, rowInfo[rowId].Col] = rowInfo[rowId].Num;
            }
        }
    }

    // Print puzzle
    void PrintPuzzle(int[,] grid) {
        Console.WriteLine();
        Console.WriteLine("Puzzle:");
        for (int r = 0; r < 9; r++) {
            for (int c = 0; c < 9; c++) {
                Console.Write($"{grid[r, c]} ");
            }
            Console.WriteLine();
        }
    }

    // Read matrix file
    bool ReadMatrixFile(string filename) {
        if (!File.Exists(filename)) {
            Console.Error.WriteLine($"Error opening file '{filename}'");
            return false;
        }

        // Normalize path for output
        string displayPath = filename;
        if (filename.StartsWith("/app/Matrices/")) {
            displayPath = filename.Substring(5);
            Console.WriteLine($"../{displayPath}");
        } else {
            Console.WriteLine(filename);
        }

        int lineCount = 0;
        foreach (string line in File.ReadLines(filename)) {
            // Skip comments and empty lines
            if (string.IsNullOrWhiteSpace(line) || line.StartsWith("#")) {
                continue;
            }

            string[] parts = line.Split(new char[] { ' ', '\t' }, StringSplitOptions.RemoveEmptyEntries);
            if (parts.Length == 9 && lineCount < 9) {
                for (int i = 0; i < 9; i++) {
                    puzzle[lineCount, i] = int.Parse(parts[i]);
                    Console.Write($"{puzzle[lineCount, i]} ");
                }
                Console.WriteLine();
                lineCount++;
            }
        }

        return lineCount == 9;
    }

    public void Solve(string filename) {
        if (!ReadMatrixFile(filename)) {
            Console.Error.WriteLine($"Error reading {filename}");
            return;
        }

        PrintPuzzle(puzzle);

        // Initialize DLX matrix
        InitDlxMatrix();

        // Build matrix from puzzle
        BuildDlxMatrixFromPuzzle();

        // Cover pre-filled clues
        CoverClues();

        // Solve using DLX
        dlxIterations = 0;
        int[] solution = new int[81];
        bool result = DlxSearch(0, solution);

        if (result) {
            ExtractSolution(solution, 81);
            PrintPuzzle(solutionGrid);
            Console.WriteLine($"\nSolved in Iterations={dlxIterations}\n");
        } else {
            Console.WriteLine("\nNo solution found");
        }
    }

    static void Main(string[] args) {
        var startTime = DateTime.Now;

        // Process each .matrix file from command line
        foreach (string arg in args) {
            if (arg.EndsWith(".matrix")) {
                var solver = new DlxSolver();
                solver.Solve(arg);
            }
        }

        var elapsed = DateTime.Now - startTime;
        Console.WriteLine($"Seconds to process {elapsed.TotalSeconds:F3}");
    }
}
