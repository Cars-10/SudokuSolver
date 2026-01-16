#!/usr/bin/env coffee
# DLX (Dancing Links) Sudoku Solver - CoffeeScript Implementation
# Direct port of JavaScript implementation using Algorithm X with Dancing Links

fs = require 'fs'

# Data Structures
class DlxNode
  constructor: ->
    @left = null
    @right = null
    @up = null
    @down = null
    @column = null
    @rowId = -1

class DlxColumn
  constructor: (name) ->
    @node = new DlxNode()
    @size = 0
    @name = name or ""
    
    # Initialize column node as circular list
    @node.left = @node
    @node.right = @node
    @node.up = @node
    @node.down = @node
    @node.column = @

# Global State
dlxIterations = 0
puzzle = (Array(9).fill(0) for i in [0...9])
solutionGrid = (Array(9).fill(0) for i in [0...9])

# DLX matrix structures
root = null
columns = []
rowInfo = []
rowStarts = []

# Constraint Column Indices
getPositionCol = (r, c) -> r * 9 + c
getRowCol = (r, n) -> 81 + r * 9 + (n - 1)
getColCol = (c, n) -> 162 + c * 9 + (n - 1)
getBoxCol = (r, c, n) ->
  box = Math.floor(r / 3) * 3 + Math.floor(c / 3)
  243 + box * 9 + (n - 1)

# DLX Core Operations
coverColumn = (c) ->
  colNode = c.node
  
  # Remove column header from the header list
  colNode.right.left = colNode.left
  colNode.left.right = colNode.right
  
  # For each row in this column
  rowNode = colNode.down
  while rowNode isnt colNode
    # For each node in this row
    rightNode = rowNode.right
    while rightNode isnt rowNode
      # Remove this node from its column
      rightNode.down.up = rightNode.up
      rightNode.up.down = rightNode.down
      rightNode.column.size--
      rightNode = rightNode.right
    rowNode = rowNode.down

uncoverColumn = (c) ->
  colNode = c.node
  
  # For each row in this column (in reverse order)
  rowNode = colNode.up
  while rowNode isnt colNode
    # For each node in this row (in reverse order)
    leftNode = rowNode.left
    while leftNode isnt rowNode
      # Restore this node to its column
      leftNode.column.size++
      leftNode.down.up = leftNode
      leftNode.up.down = leftNode
      leftNode = leftNode.left
    rowNode = rowNode.up
  
  # Restore column header to the header list
  colNode.right.left = colNode
  colNode.left.right = colNode

chooseColumn = ->
  rootNode = root.node
  best = null
  minSize = Number.MAX_SAFE_INTEGER
  
  colNode = rootNode.right
  while colNode isnt rootNode
    if colNode.column.size < minSize
      minSize = colNode.column.size
      best = colNode.column
    colNode = colNode.right
  
  best

dlxSearch = (k, solution) ->
  dlxIterations++
  
  rootNode = root.node
  
  # If matrix is empty, we found a solution
  return true if rootNode.right is rootNode
  
  # Choose column with minimum size
  col = chooseColumn()
  
  # If column has no rows, no solution possible
  return false if col.size is 0
  
  # Cover this column
  coverColumn col
  
  # Try each row in this column
  rowNode = col.node.down
  while rowNode isnt col.node
    # Add row to partial solution
    solution[k] = rowNode.rowId
    
    # Cover all other columns in this row
    rightNode = rowNode.right
    while rightNode isnt rowNode
      coverColumn rightNode.column
      rightNode = rightNode.right
    
    # Recurse
    return true if dlxSearch k + 1, solution
    
    # Backtrack: uncover all columns in this row
    leftNode = rowNode.left
    while leftNode isnt rowNode
      uncoverColumn leftNode.column
      leftNode = leftNode.left
    
    rowNode = rowNode.down
  
  # Uncover column
  uncoverColumn col
  
  false

# DLX Matrix Construction
initDlxMatrix = ->
  # Create root column
  root = new DlxColumn "root"
  
  # Create 324 column headers
  columns = []
  for i in [0...324]
    col = new DlxColumn "C#{i}"
    
    # Link into header list
    col.node.left = root.node.left
    col.node.right = root.node
    root.node.left.right = col.node
    root.node.left = col.node
    
    columns.push col
  
  rowInfo = []
  rowStarts = []

addNode = (col, rowId) ->
  node = new DlxNode()
  node.column = col
  node.rowId = rowId
  
  # Insert at end of column's circular list
  node.down = col.node
  node.up = col.node.up
  col.node.up.down = node
  col.node.up = node
  col.size++
  
  node

buildDlxRow = (r, c, n, rowId) ->
  # Store row metadata
  rowInfo[rowId] = { row: r, col: c, num: n }
  
  # Create nodes for the 4 constraints
  n1 = addNode columns[getPositionCol(r, c)], rowId
  n2 = addNode columns[getRowCol(r, n)], rowId
  n3 = addNode columns[getColCol(c, n)], rowId
  n4 = addNode columns[getBoxCol(r, c, n)], rowId
  
  # Link nodes horizontally in circular list
  n1.right = n2
  n2.right = n3
  n3.right = n4
  n4.right = n1
  
  n1.left = n4
  n2.left = n1
  n3.left = n2
  n4.left = n3
  
  # Store first node for this row
  rowStarts[rowId] = n1

buildDlxMatrixFromPuzzle = ->
  rowId = 0
  
  for r in [0...9]
    for c in [0...9]
      if puzzle[r][c] isnt 0
        # Cell has a clue - create only one row
        buildDlxRow r, c, puzzle[r][c], rowId++
      else
        # Cell is empty - create rows for all possible values
        for n in [1..9]
          buildDlxRow r, c, n, rowId++

coverClues = ->
  for r in [0...9]
    for c in [0...9]
      if puzzle[r][c] isnt 0
        n = puzzle[r][c]
        
        # Find the row for this clue
        for rowId in [0...rowStarts.length]
          if rowStarts[rowId] and
             rowInfo[rowId].row is r and
             rowInfo[rowId].col is c and
             rowInfo[rowId].num is n
            
            # Cover all columns in this row
            node = rowStarts[rowId]
            curr = node
            loop
              coverColumn curr.column
              curr = curr.right
              break if curr is node
            break

extractSolution = (solution, solutionLen) ->
  # Initialize solution grid with original puzzle
  for r in [0...9]
    for c in [0...9]
      solutionGrid[r][c] = puzzle[r][c]
  
  # Each solution entry is a row_id
  for i in [0...solutionLen]
    rowId = solution[i]
    if rowId >= 0 and rowId < 729 and rowInfo[rowId]
      solutionGrid[rowInfo[rowId].row][rowInfo[rowId].col] = rowInfo[rowId].num

# Puzzle I/O
printPuzzle = (grid) ->
  console.log "\nPuzzle:"
  for r in [0...9]
    console.log grid[r].join(" ") + " "

readMatrixFile = (filename) ->
  # Normalize path for output
  displayPath = filename
  if filename.indexOf("/app/Matrices/") is 0
    displayPath = "../" + filename.substring(5)
  console.log displayPath
  
  data = fs.readFileSync filename, 'utf8'
  lines = data.split '\n'
  lineCount = 0
  
  for line in lines
    trimmed = line.trim()
    # Skip comments and empty lines
    continue if trimmed is '' or trimmed[0] is '#'
    
    # Parse 9 integers from line
    values = trimmed.split(/\s+/).map(Number)
    if values.length is 9 and lineCount < 9
      puzzle[lineCount] = values
      console.log values.join(" ") + " "
      lineCount++
  
  lineCount is 9

# Main Execution
startTime = Date.now()

# Process command line arguments
for arg in process.argv[2..]
  continue unless arg.endsWith ".matrix"
  
  # Reset puzzle
  puzzle = (Array(9).fill(0) for i in [0...9])
  solutionGrid = (Array(9).fill(0) for i in [0...9])
  
  unless readMatrixFile arg
    console.error "Error reading #{arg}"
    continue
  
  printPuzzle puzzle
  
  # Initialize DLX matrix
  initDlxMatrix()
  
  # Build matrix from puzzle
  buildDlxMatrixFromPuzzle()
  
  # Cover pre-filled clues
  coverClues()
  
  # Solve using DLX
  dlxIterations = 0
  solution = new Array 81
  result = dlxSearch 0, solution
  
  if result
    extractSolution solution, 81
    printPuzzle solutionGrid
    console.log "\nSolved in Iterations=#{dlxIterations}\n"
  else
    console.log "\nNo solution found\n"

elapsed = (Date.now() - startTime) / 1000
console.log "Seconds to process #{elapsed.toFixed(3)}"
