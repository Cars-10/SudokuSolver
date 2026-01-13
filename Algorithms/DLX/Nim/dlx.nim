import os, strutils, sequtils

# DLX Node - ref object for GC-managed mutable references
type
  DlxNode = ref object
    left, right, up, down, column: DlxNode
    size, rowId, colId: int

# Global iteration counter
var dlxIterations = 0

# Row metadata to map DLX rows back to Sudoku
type RowInfo = object
  row, col, num: int

var rowInfos: seq[RowInfo] = @[]
var rowStarts: seq[DlxNode] = @[]

# Cover a column in the DLX matrix
proc coverColumn(c: DlxNode) =
  # Remove column header from the header list
  c.right.left = c.left
  c.left.right = c.right

  # For each row in this column
  var rowNode = c.down
  while rowNode != c:
    # For each node in this row (excluding the column itself)
    var rightNode = rowNode.right
    while rightNode != rowNode:
      # Remove this node from its column
      rightNode.down.up = rightNode.up
      rightNode.up.down = rightNode.down
      rightNode.column.size -= 1
      rightNode = rightNode.right
    rowNode = rowNode.down

# Uncover a column (exact reverse of cover)
proc uncoverColumn(c: DlxNode) =
  # For each row in this column (in reverse order)
  var rowNode = c.up
  while rowNode != c:
    # For each node in this row (in reverse order)
    var leftNode = rowNode.left
    while leftNode != rowNode:
      # Restore this node to its column
      leftNode.column.size += 1
      leftNode.down.up = leftNode
      leftNode.up.down = leftNode
      leftNode = leftNode.left
    rowNode = rowNode.up

  # Restore column header to the header list
  c.right.left = c
  c.left.right = c

# Choose column with minimum size (Knuth's S heuristic)
proc chooseColumn(root: DlxNode): DlxNode =
  var best: DlxNode = nil
  var minSize = high(int)

  var colNode = root.right
  while colNode != root:
    if colNode.size < minSize:
      minSize = colNode.size
      best = colNode
    colNode = colNode.right

  return best

# DLX Search - Algorithm X with Dancing Links
proc dlxSearch(root: DlxNode, k: int, solution: var seq[int]): bool =
  dlxIterations += 1  # Count every search call

  # If matrix is empty, we found a solution
  if root.right == root:
    return true

  # Choose column with minimum size
  let col = chooseColumn(root)

  # If column has no rows, no solution possible
  if col.size == 0:
    return false

  # Cover this column
  coverColumn(col)

  # Try each row in this column
  var rowNode = col.down
  while rowNode != col:
    # Add row to partial solution
    if k >= solution.len:
      solution.add(rowNode.rowId)
    else:
      solution[k] = rowNode.rowId

    # Cover all other columns in this row
    var rightNode = rowNode.right
    while rightNode != rowNode:
      coverColumn(rightNode.column)
      rightNode = rightNode.right

    # Recurse
    if dlxSearch(root, k + 1, solution):
      return true  # Solution found

    # Backtrack: uncover all columns in this row
    var leftNode = rowNode.left
    while leftNode != rowNode:
      uncoverColumn(leftNode.column)
      leftNode = leftNode.left

    rowNode = rowNode.down

  # Uncover column
  uncoverColumn(col)

  return false  # No solution found

# Build the Sudoku exact cover matrix for DLX
proc buildSudokuMatrix(): DlxNode =
  # Create root node
  let root = DlxNode(size: 0, rowId: -1, colId: -1)
  root.left = root
  root.right = root
  root.up = root
  root.down = root

  # Create 324 column headers
  var columns: seq[DlxNode] = @[]
  for i in 0 ..< 324:
    let col = DlxNode(size: 0, rowId: -1, colId: i)
    col.left = root.left
    col.right = root
    col.up = col
    col.down = col
    col.column = col
    root.left.right = col
    root.left = col
    columns.add(col)

  return root

# Add a constraint row to the DLX matrix
proc addConstraintRow(columns: seq[DlxNode], rowId: int, cols: seq[int]) =
  if cols.len == 0:
    rowStarts.add(nil)
    return

  var rowNodes: seq[DlxNode] = @[]

  # Create nodes for this row
  for colIdx in cols:
    let node = DlxNode(size: 0, rowId: rowId, colId: colIdx)
    node.column = columns[colIdx]

    # Insert into column (at bottom)
    node.up = node.column.up
    node.down = node.column
    node.column.up.down = node
    node.column.up = node
    node.column.size += 1

    rowNodes.add(node)

  # Link row nodes circularly
  for i in 0 ..< rowNodes.len:
    rowNodes[i].left = rowNodes[(i - 1 + rowNodes.len) mod rowNodes.len]
    rowNodes[i].right = rowNodes[(i + 1) mod rowNodes.len]

  # Store first node of this row
  rowStarts.add(rowNodes[0])

# Build the complete Sudoku DLX matrix
proc buildSudokuDLXMatrix(puzzle: seq[seq[int]]): (DlxNode, seq[DlxNode]) =
  let root = buildSudokuMatrix()

  # Get columns array
  var columns: seq[DlxNode] = @[]
  var col = root.right
  while col != root:
    columns.add(col)
    col = col.right

  # Add constraint rows for all possible placements
  var rowId = 0
  for row in 0 ..< 9:
    for col in 0 ..< 9:
      # For clues: create only one row with that value
      # For empty cells: create rows for all values 1-9
      let digits = if puzzle[row][col] != 0: @[puzzle[row][col]]
                   else: @[1, 2, 3, 4, 5, 6, 7, 8, 9]

      for digit in digits:
        # Store row metadata
        rowInfos.add(RowInfo(row: row, col: col, num: digit))

        # Build the 4 constraint columns for this placement
        let posConstraint = row * 9 + col
        let rowConstraint = 81 + row * 9 + (digit - 1)
        let colConstraint = 162 + col * 9 + (digit - 1)
        let boxConstraint = 243 + ((row div 3) * 3 + (col div 3)) * 9 + (digit - 1)

        addConstraintRow(columns, rowId, @[posConstraint, rowConstraint, colConstraint, boxConstraint])
        rowId += 1

  return (root, columns)

# Parse matrix file
proc parseMatrixFile(filename: string): seq[seq[int]] =
  var puzzle: seq[seq[int]] = @[]

  # Normalize path for output
  if filename.len >= 14 and filename[0..13] == "/app/Matrices/":
    echo "../", filename[5..^1]
  else:
    echo filename

  let file = open(filename, fmRead)
  defer: file.close()

  var lineCount = 0
  for line in file.lines:
    let lineStr = line.strip()

    # Skip comments and empty lines
    if lineStr.len == 0 or lineStr[0] == '#':
      continue

    if lineCount >= 9:
      break

    # Parse 9 integers from line
    let parts = lineStr.splitWhitespace()
    if parts.len >= 9:
      var row: seq[int] = @[]
      for i in 0 ..< 9:
        row.add(parseInt(parts[i]))
        stdout.write(parts[i], " ")
      echo ""
      puzzle.add(row)
      lineCount += 1

  return puzzle

# Print puzzle
proc printPuzzle(puzzle: seq[seq[int]]) =
  echo "\nPuzzle:"
  for row in puzzle:
    for val in row:
      stdout.write(val, " ")
    echo ""

# Decode DLX solution to Sudoku grid
proc decodeSolution(puzzle: seq[seq[int]], solution: seq[int]): seq[seq[int]] =
  # Start with original puzzle (includes clues)
  var grid = puzzle

  # Fill in the solution
  for rowId in solution:
    if rowId >= 0 and rowId < rowInfos.len:
      let info = rowInfos[rowId]
      grid[info.row][info.col] = info.num

  return grid

# Cover clues before solving
proc coverClues(root: DlxNode, puzzle: seq[seq[int]]) =
  for r in 0 ..< 9:
    for c in 0 ..< 9:
      if puzzle[r][c] != 0:
        let n = puzzle[r][c]

        # Find the row for this clue
        for rowId in 0 ..< rowInfos.len:
          if rowInfos[rowId].row == r and
             rowInfos[rowId].col == c and
             rowInfos[rowId].num == n:

            # Cover all columns in this row
            if rowId < rowStarts.len and rowStarts[rowId] != nil:
              var node = rowStarts[rowId]
              var curr = node
              while true:
                coverColumn(curr.column)
                curr = curr.right
                if curr == node:
                  break
            break

# Main execution
when isMainModule:
  let args = commandLineParams()

  if args.len == 0:
    stderr.writeLine("Usage: dlx <matrix-file>")
    quit(1)

  for filename in args:
    if filename.len >= 7 and filename[^7..^1] == ".matrix":
      # Clear global state
      rowInfos = @[]
      rowStarts = @[]

      # Parse puzzle
      let puzzle = parseMatrixFile(filename)
      printPuzzle(puzzle)

      # Build DLX matrix
      let (root, columns) = buildSudokuDLXMatrix(puzzle)

      # Cover clues
      coverClues(root, puzzle)

      # Solve
      dlxIterations = 0
      var solution: seq[int] = @[]

      if dlxSearch(root, 0, solution):
        # Decode and print solution
        let solved = decodeSolution(puzzle, solution)
        printPuzzle(solved)
        echo "\nSolved in Iterations=", dlxIterations, "\n"
      else:
        echo "\nNo solution found"
        echo "Iterations=", dlxIterations, "\n"
