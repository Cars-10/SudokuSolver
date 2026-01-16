#!/usr/bin/env coffee
# CP (Constraint Propagation) Sudoku Solver - CoffeeScript Implementation
# Direct port of JavaScript implementation with MRV heuristic and propagation

fs = require 'fs'

# Data Structures
class CPGrid
  constructor: ->
    @values = (Array(9).fill(0) for i in [0...9])
    @candidates = (Array(9).fill(0) for i in [0...9])
  
  copy: ->
    grid = new CPGrid()
    for r in [0...9]
      for c in [0...9]
        grid.values[r][c] = @values[r][c]
        grid.candidates[r][c] = @candidates[r][c]
    grid

# Global State
cpIterations = 0
puzzle = (Array(9).fill(0) for i in [0...9])

# Bitset Helpers
hasCandidate = (set, digit) -> (set & (1 << digit)) != 0
addCandidate = (set, digit) -> set | (1 << digit)
removeCandidate = (set, digit) -> set & ~(1 << digit)

countCandidates = (set) ->
  count = 0
  while set
    count += set & 1
    set >>= 1
  count

getFirstCandidate = (set) ->
  for digit in [1..9]
    return digit if hasCandidate set, digit
  0

# Helper Functions
getPeers = (row, col) ->
  peers = []
  
  # Same row
  for c in [0...9]
    peers.push [row, c] if c != col
  
  # Same column
  for r in [0...9]
    peers.push [r, col] if r != row
  
  # Same 3x3 box
  boxRow = Math.floor(row / 3) * 3
  boxCol = Math.floor(col / 3) * 3
  for r in [boxRow...boxRow + 3]
    for c in [boxCol...boxCol + 3]
      peers.push [r, c] if r != row and c != col
  
  peers

# Initialization
initGrid = (grid, puzzle) ->
  for row in [0...9]
    for col in [0...9]
      if puzzle[row][col] is 0
        # Empty cell: set all candidates 1-9
        grid.values[row][col] = 0
        grid.candidates[row][col] = 0x3FE
      else
        # Given clue: set single value
        digit = puzzle[row][col]
        grid.values[row][col] = digit
        grid.candidates[row][col] = (1 << digit)

# Constraint Propagation
eliminate = (grid, row, col, digit) ->
  # Check if digit is already eliminated
  return true unless hasCandidate grid.candidates[row][col], digit
  
  # Remove digit from candidates
  grid.candidates[row][col] = removeCandidate grid.candidates[row][col], digit
  
  # Check for contradiction
  remaining = countCandidates grid.candidates[row][col]
  return false if remaining is 0
  
  # If only one candidate left, assign it
  if remaining is 1 and grid.values[row][col] is 0
    lastDigit = getFirstCandidate grid.candidates[row][col]
    return false unless assign grid, row, col, lastDigit
  
  true

assign = (grid, row, col, digit) ->
  # Increment iteration counter
  cpIterations++
  
  # Set value
  grid.values[row][col] = digit
  grid.candidates[row][col] = (1 << digit)
  
  # Eliminate digit from all peers
  peers = getPeers row, col
  
  for [peerRow, peerCol] in peers
    return false unless eliminate grid, peerRow, peerCol, digit
  
  true

propagate = (grid) ->
  changed = true
  
  while changed
    changed = false
    
    # Strategy 1: Singleton elimination
    for row in [0...9]
      for col in [0...9]
        if grid.values[row][col] is 0
          numCandidates = countCandidates grid.candidates[row][col]
          return false if numCandidates is 0
          
          if numCandidates is 1
            digit = getFirstCandidate grid.candidates[row][col]
            return false unless assign grid, row, col, digit
            changed = true
    
    # Strategy 2: Hidden singles - Rows
    for row in [0...9]
      for digit in [1..9]
        count = 0
        lastCol = -1
        
        for col in [0...9]
          if grid.values[row][col] is digit
            count = 0
            break
          if hasCandidate grid.candidates[row][col], digit
            count++
            lastCol = col
        
        if count is 1
          return false unless assign grid, row, lastCol, digit
          changed = true
        else if count is 0
          found = false
          for col in [0...9]
            if grid.values[row][col] is digit
              found = true
              break
          return false unless found
    
    # Hidden singles - Columns
    for col in [0...9]
      for digit in [1..9]
        count = 0
        lastRow = -1
        
        for row in [0...9]
          if grid.values[row][col] is digit
            count = 0
            break
          if hasCandidate grid.candidates[row][col], digit
            count++
            lastRow = row
        
        if count is 1
          return false unless assign grid, lastRow, col, digit
          changed = true
        else if count is 0
          found = false
          for row in [0...9]
            if grid.values[row][col] is digit
              found = true
              break
          return false unless found
    
    # Hidden singles - Boxes
    for box in [0...9]
      boxRow = Math.floor(box / 3) * 3
      boxCol = (box % 3) * 3
      
      for digit in [1..9]
        count = 0
        lastR = -1
        lastC = -1
        foundAssigned = false
        
        for r in [boxRow...boxRow + 3]
          for c in [boxCol...boxCol + 3]
            if grid.values[r][c] is digit
              foundAssigned = true
              count = 0
              break
            if hasCandidate grid.candidates[r][c], digit
              count++
              lastR = r
              lastC = c
          break if foundAssigned
        
        if count is 1
          return false unless assign grid, lastR, lastC, digit
          changed = true
        else if count is 0
          found = false
          for r in [boxRow...boxRow + 3]
            for c in [boxCol...boxCol + 3]
              if grid.values[r][c] is digit
                found = true
                break
            break if found
          return false unless found
  
  true

# Search
findMrvCell = (grid) ->
  minCandidates = 10
  mrvRow = -1
  mrvCol = -1
  
  for r in [0...9]
    for c in [0...9]
      if grid.values[r][c] is 0
        numCandidates = countCandidates grid.candidates[r][c]
        if numCandidates < minCandidates
          minCandidates = numCandidates
          mrvRow = r
          mrvCol = c
  
  if mrvRow is -1 then null else {row: mrvRow, col: mrvCol}

cpSearch = (grid, solution) ->
  # Base case: check if grid is complete
  mrvCell = findMrvCell grid
  unless mrvCell
    # No empty cells - extract solution
    for r in [0...9]
      for c in [0...9]
        solution[r * 9 + c] = grid.values[r][c]
    return true
  
  # Try each candidate for MRV cell
  candidates = grid.candidates[mrvCell.row][mrvCell.col]
  
  for digit in [1..9]
    if hasCandidate candidates, digit
      # Save grid state for backtracking
      gridCopy = grid.copy()
      
      # Try assigning this digit
      if assign grid, mrvCell.row, mrvCell.col, digit
        # Propagate constraints
        if propagate grid
          # Recurse
          return true if cpSearch grid, solution
      
      # Failed - restore grid state
      grid.values = gridCopy.values
      grid.candidates = gridCopy.candidates
  
  false

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
    continue if trimmed is '' or trimmed[0] is '#'
    
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
  
  unless readMatrixFile arg
    console.error "Failed to read matrix file #{arg}"
    continue
  
  # Print initial puzzle
  printPuzzle puzzle
  
  # Initialize CP grid
  grid = new CPGrid()
  initGrid grid, puzzle
  
  # Apply initial propagation
  unless propagate grid
    console.log "\nNo solution found (contradiction during initial propagation)\n"
    continue
  
  # Run search
  solution = new Array 81
  cpIterations = 0
  solved = cpSearch grid, solution
  
  if solved
    # Convert solution array back to 2D
    solutionGrid = (Array(9).fill(0) for i in [0...9])
    for r in [0...9]
      for c in [0...9]
        solutionGrid[r][c] = solution[r * 9 + c]
    
    printPuzzle solutionGrid
    console.log "\nSolved in Iterations=#{cpIterations}\n"
  else
    console.log "\nNo solution found\n"

elapsed = (Date.now() - startTime) / 1000
console.log "Seconds to process #{elapsed.toFixed(3)}"
