import 'dart:io';

// DLX Node class - circular doubly-linked structure
class DlxNode {
  DlxNode? up;
  DlxNode? down;
  DlxNode? left;
  DlxNode? right;
  DlxNode? column;
  int size = 0;
  int rowId = -1;
  int colId = -1;
  String name = '';

  DlxNode();
}

// Global iteration counter (analogous to brute-force count)
int dlxIterations = 0;

// Sudoku puzzle grid [row][col]
List<List<int>> puzzle = List.generate(9, (_) => List.filled(9, 0));
List<List<int>> solutionGrid = List.generate(9, (_) => List.filled(9, 0));

// DLX matrix structures
late DlxNode root;
List<DlxNode> columns = [];
List<DlxNode> nodes = [];
int nodeCount = 0;

// Row metadata to map DLX rows back to Sudoku (row, col, num)
class RowInfo {
  int row;
  int col;
  int num;

  RowInfo(this.row, this.col, this.num);
}

List<RowInfo> rowInfo = [];
List<DlxNode?> rowStarts = List.filled(729, null);

// Calculate constraint column indices
int getPositionCol(int r, int c) => r * 9 + c;
int getRowCol(int r, int n) => 81 + r * 9 + (n - 1);
int getColCol(int c, int n) => 162 + c * 9 + (n - 1);
int getBoxCol(int r, int c, int n) {
  int box = (r ~/ 3) * 3 + (c ~/ 3);
  return 243 + box * 9 + (n - 1);
}

// Cover a column in the DLX matrix
void coverColumn(DlxNode col) {
  // Remove column header from the header list
  col.right!.left = col.left;
  col.left!.right = col.right;

  // For each row in this column
  DlxNode? rowNode = col.down;
  while (rowNode != col) {
    // For each node in this row (excluding the column itself)
    DlxNode? rightNode = rowNode!.right;
    while (rightNode != rowNode) {
      // Remove this node from its column
      rightNode!.down!.up = rightNode.up;
      rightNode.up!.down = rightNode.down;
      rightNode.column!.size--;
      rightNode = rightNode.right;
    }
    rowNode = rowNode.down;
  }
}

// Uncover a column (exact reverse of cover)
void uncoverColumn(DlxNode col) {
  // For each row in this column (in reverse order)
  DlxNode? rowNode = col.up;
  while (rowNode != col) {
    // For each node in this row (in reverse order)
    DlxNode? leftNode = rowNode!.left;
    while (leftNode != rowNode) {
      // Restore this node to its column
      leftNode!.column!.size++;
      leftNode.down!.up = leftNode;
      leftNode.up!.down = leftNode;
      leftNode = leftNode.left;
    }
    rowNode = rowNode.up;
  }

  // Restore column header to the header list
  col.right!.left = col;
  col.left!.right = col;
}

// Choose column with minimum size (Knuth's S heuristic)
DlxNode? chooseColumn(DlxNode root) {
  DlxNode? best;
  int minSize = 0x7FFFFFFF;

  DlxNode? colNode = root.right;
  while (colNode != root) {
    if (colNode!.size < minSize) {
      minSize = colNode.size;
      best = colNode;
    }
    colNode = colNode.right;
  }

  return best;
}

// DLX Search - Algorithm X with Dancing Links
bool dlxSearch(DlxNode root, int k, List<int> solution) {
  dlxIterations++;  // Count every search call

  // If matrix is empty, we found a solution
  if (root.right == root) {
    return true;
  }

  // Choose column with minimum size
  DlxNode? col = chooseColumn(root);

  // If column has no rows, no solution possible
  if (col == null || col.size == 0) {
    return false;
  }

  // Cover this column
  coverColumn(col);

  // Try each row in this column
  DlxNode? rowNode = col.down;
  while (rowNode != col) {
    // Add row to partial solution
    solution[k] = rowNode!.rowId;

    // Cover all other columns in this row
    DlxNode? rightNode = rowNode.right;
    while (rightNode != rowNode) {
      coverColumn(rightNode!.column!);
      rightNode = rightNode.right;
    }

    // Recurse
    if (dlxSearch(root, k + 1, solution)) {
      return true;  // Solution found
    }

    // Backtrack: uncover all columns in this row
    DlxNode? leftNode = rowNode.left;
    while (leftNode != rowNode) {
      uncoverColumn(leftNode!.column!);
      leftNode = leftNode.left;
    }

    rowNode = rowNode.down;
  }

  // Uncover column
  uncoverColumn(col);

  return false;  // No solution found
}

// Initialize DLX matrix structure
void initDlxMatrix() {
  // Allocate root column
  root = DlxNode();
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
  for (int i = 0; i < 324; i++) {
    DlxNode col = DlxNode();
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
    root.left!.right = col;
    root.left = col;

    columns.add(col);
  }

  // Initialize nodes and metadata
  nodes = [];
  rowInfo = [];
  for (int i = 0; i < 729; i++) {
    rowInfo.add(RowInfo(0, 0, 0));
  }
  nodeCount = 0;
}

// Add a node to the DLX matrix
DlxNode addNode(DlxNode col, int rowId) {
  DlxNode node = DlxNode();
  node.column = col;
  node.rowId = rowId;

  // Insert at end of column's circular list
  node.down = col;
  node.up = col.up;
  col.up!.down = node;
  col.up = node;
  col.size++;

  nodes.add(node);
  nodeCount++;

  return node;
}

// Build a DLX row for Sudoku cell (r,c) with value n
void buildDlxRow(int r, int c, int n, int rowId) {
  // Store row metadata
  rowInfo[rowId].row = r;
  rowInfo[rowId].col = c;
  rowInfo[rowId].num = n;

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

// Build the complete DLX matrix from the puzzle
void buildDlxMatrixFromPuzzle() {
  int rowId = 0;

  for (int r = 0; r < 9; r++) {
    for (int c = 0; c < 9; c++) {
      if (puzzle[r][c] != 0) {
        // Cell has a clue - create only one row for that value
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

// Cover given clues (pre-selected rows)
void coverClues() {
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
            DlxNode? node = rowStarts[rowId];
            DlxNode? curr = node;
            do {
              coverColumn(curr!.column!);
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
void extractSolution(List<int> solution, int solutionLen) {
  // Initialize solution grid - start with the original puzzle
  for (int r = 0; r < 9; r++) {
    for (int c = 0; c < 9; c++) {
      solutionGrid[r][c] = puzzle[r][c];
    }
  }

  // Each solution entry is a row_id
  for (int i = 0; i < solutionLen; i++) {
    int rowId = solution[i];
    if (rowId >= 0 && rowId < 729) {
      solutionGrid[rowInfo[rowId].row][rowInfo[rowId].col] = rowInfo[rowId].num;
    }
  }
}

// Print puzzle
void printPuzzle(List<List<int>> grid) {
  print('\nPuzzle:');
  for (int r = 0; r < 9; r++) {
    for (int c = 0; c < 9; c++) {
      stdout.write('${grid[r][c]} ');
    }
    print('');
  }
}

// Read matrix file
int readMatrixFile(String filename) {
  File file = File(filename);
  if (!file.existsSync()) {
    stderr.writeln("Error opening file '$filename'");
    return 1;
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
    }
  }

  return 0;
}

void main(List<String> args) {
  Stopwatch stopwatch = Stopwatch()..start();

  // Process each .matrix file from command line
  for (String filename in args) {
    if (filename.endsWith('.matrix')) {
      if (readMatrixFile(filename) != 0) {
        stderr.writeln('Error reading $filename');
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
      List<int> solution = List.filled(81, 0);
      bool result = dlxSearch(root, 0, solution);

      if (result) {
        extractSolution(solution, 81);
        printPuzzle(solutionGrid);
        print('\nSolved in Iterations=$dlxIterations\n');
      } else {
        print('\nNo solution found');
      }
    }
  }

  stopwatch.stop();
  double elapsed = stopwatch.elapsedMicroseconds / 1000000.0;
  print('Seconds to process ${elapsed.toStringAsFixed(3)}');
}
