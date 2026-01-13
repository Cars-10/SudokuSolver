import os, strutils, times

# Type definitions
type
  CandidateSet = uint16

  CPGrid = object
    values: array[9, array[9, int]]
    candidates: array[9, array[9, CandidateSet]]

# Global iteration counter
var cpIterations: int64 = 0

# Candidate set operations
proc hasCand(cs: CandidateSet, digit: int): bool {.inline.} =
  (cs and (1'u16 shl digit)) != 0

proc removeCand(cs: var CandidateSet, digit: int) {.inline.} =
  cs = cs and not (1'u16 shl digit)

proc countCand(cs: CandidateSet): int =
  var count = 0
  for digit in 1 .. 9:
    if hasCand(cs, digit):
      count += 1
  return count

proc getFirstCand(cs: CandidateSet): int =
  for digit in 1 .. 9:
    if hasCand(cs, digit):
      return digit
  return 0

# Get all 20 peers for a cell (row, col, box)
proc getPeers(row, col: int): seq[(int, int)] =
  var peers: seq[(int, int)] = @[]

  # Same row (8 cells)
  for c in 0 ..< 9:
    if c != col:
      peers.add((row, c))

  # Same column (8 cells)
  for r in 0 ..< 9:
    if r != row:
      peers.add((r, col))

  # Same 3x3 box (4 additional cells)
  let boxRow = (row div 3) * 3
  let boxCol = (col div 3) * 3
  for r in boxRow ..< boxRow + 3:
    for c in boxCol ..< boxCol + 3:
      if r != row and c != col:
        peers.add((r, c))

  return peers

# Forward declaration
proc assign(grid: var CPGrid, row, col, digit: int): bool

# Initialize grid from puzzle
proc initGrid(puzzle: seq[seq[int]]): CPGrid =
  var grid: CPGrid

  for row in 0 ..< 9:
    for col in 0 ..< 9:
      if puzzle[row][col] == 0:
        # Empty cell: set all candidates 1-9 (bits 1-9 set)
        grid.values[row][col] = 0
        grid.candidates[row][col] = 0x3FE'u16  # Binary: 0011 1111 1110 (bits 1-9)
      else:
        # Given clue: set single value
        let digit = puzzle[row][col]
        grid.values[row][col] = digit
        grid.candidates[row][col] = (1'u16 shl digit)

  return grid

# Eliminate a candidate from a cell
proc eliminate(grid: var CPGrid, row, col, digit: int): bool =
  # Check if digit is already eliminated
  if not hasCand(grid.candidates[row][col], digit):
    return true  # Already eliminated, no change

  # Remove digit from candidates
  removeCand(grid.candidates[row][col], digit)

  # Check for contradiction (no candidates left)
  let remaining = countCand(grid.candidates[row][col])
  if remaining == 0:
    return false  # Contradiction

  # If only one candidate left, assign it (singleton elimination)
  if remaining == 1 and grid.values[row][col] == 0:
    let lastDigit = getFirstCand(grid.candidates[row][col])
    if not assign(grid, row, col, lastDigit):
      return false  # Assignment caused contradiction

  return true

# Assign a value to a cell and propagate
proc assign(grid: var CPGrid, row, col, digit: int): bool =
  # Increment iteration counter (this is our benchmark metric)
  cpIterations += 1

  # Set value
  grid.values[row][col] = digit
  grid.candidates[row][col] = (1'u16 shl digit)

  # Eliminate digit from all peers
  let peers = getPeers(row, col)

  for peer in peers:
    let (peerRow, peerCol) = peer
    if not eliminate(grid, peerRow, peerCol, digit):
      return false  # Contradiction in peer elimination

  return true

# Constraint propagation
proc propagate(grid: var CPGrid): bool =
  var changed = true

  while changed:
    changed = false

    # Strategy 1: Singleton elimination
    # If a cell has only one candidate, assign it
    for row in 0 ..< 9:
      for col in 0 ..< 9:
        if grid.values[row][col] == 0:
          let numCandidates = countCand(grid.candidates[row][col])
          if numCandidates == 0:
            return false  # Contradiction
          if numCandidates == 1:
            let digit = getFirstCand(grid.candidates[row][col])
            if not assign(grid, row, col, digit):
              return false  # Assignment caused contradiction
            changed = true

    # Strategy 2: Hidden singles
    # For each unit (row, col, box), if a digit appears in only one cell, assign it

    # Check rows
    for row in 0 ..< 9:
      for digit in 1 .. 9:
        var count = 0
        var lastCol = -1
        var alreadyAssigned = false

        for col in 0 ..< 9:
          if grid.values[row][col] == digit:
            alreadyAssigned = true
            break
          if hasCand(grid.candidates[row][col], digit):
            count += 1
            lastCol = col

        if not alreadyAssigned:
          if count == 1:
            if not assign(grid, row, lastCol, digit):
              return false
            changed = true
          elif count == 0:
            return false  # Digit cannot be placed anywhere in row

    # Check columns
    for col in 0 ..< 9:
      for digit in 1 .. 9:
        var count = 0
        var lastRow = -1
        var alreadyAssigned = false

        for row in 0 ..< 9:
          if grid.values[row][col] == digit:
            alreadyAssigned = true
            break
          if hasCand(grid.candidates[row][col], digit):
            count += 1
            lastRow = row

        if not alreadyAssigned:
          if count == 1:
            if not assign(grid, lastRow, col, digit):
              return false
            changed = true
          elif count == 0:
            return false  # Digit cannot be placed anywhere in column

    # Check boxes
    for box in 0 ..< 9:
      let boxRow = (box div 3) * 3
      let boxCol = (box mod 3) * 3

      for digit in 1 .. 9:
        var count = 0
        var lastR = -1
        var lastC = -1
        var alreadyAssigned = false

        for r in boxRow ..< boxRow + 3:
          for c in boxCol ..< boxCol + 3:
            if grid.values[r][c] == digit:
              alreadyAssigned = true
              break
            if hasCand(grid.candidates[r][c], digit):
              count += 1
              lastR = r
              lastC = c
          if alreadyAssigned:
            break

        if not alreadyAssigned:
          if count == 1:
            if not assign(grid, lastR, lastC, digit):
              return false
            changed = true
          elif count == 0:
            return false  # Digit cannot be placed anywhere in box

  return true  # Success - reached fixpoint

# Find cell with Minimum Remaining Values (MRV)
proc findMRVCell(grid: CPGrid): (int, int, bool) =
  var minCandidates = 10  # More than 9
  var mrvRow = -1
  var mrvCol = -1
  var found = false

  for r in 0 ..< 9:
    for c in 0 ..< 9:
      if grid.values[r][c] == 0:
        let numCandidates = countCand(grid.candidates[r][c])
        if numCandidates < minCandidates:
          minCandidates = numCandidates
          mrvRow = r
          mrvCol = c
          found = true

  return (mrvRow, mrvCol, found)

# Deep copy grid for backtracking
proc copyGrid(grid: CPGrid): CPGrid =
  var newGrid: CPGrid
  for row in 0 ..< 9:
    for col in 0 ..< 9:
      newGrid.values[row][col] = grid.values[row][col]
      newGrid.candidates[row][col] = grid.candidates[row][col]
  return newGrid

# Search with backtracking
proc cpSearch(grid: var CPGrid): bool =
  # Find MRV cell
  let (mrvRow, mrvCol, found) = findMRVCell(grid)

  # If no empty cells, puzzle is solved
  if not found:
    return true

  # Try each candidate for the MRV cell
  let candidates = grid.candidates[mrvRow][mrvCol]

  for digit in 1 .. 9:
    if hasCand(candidates, digit):
      # Save grid state for backtracking
      var gridCopy = copyGrid(grid)

      # Try assigning this digit
      if assign(grid, mrvRow, mrvCol, digit):
        # Assignment succeeded, propagate constraints
        if propagate(grid):
          # Propagation succeeded, recurse
          if cpSearch(grid):
            return true  # Found solution

      # Failed - restore grid state and try next candidate
      grid = gridCopy

  # All candidates exhausted - dead end
  return false

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

# Print grid
proc printGrid(grid: CPGrid) =
  echo "\nPuzzle:"
  for row in 0 ..< 9:
    for col in 0 ..< 9:
      stdout.write(grid.values[row][col], " ")
    echo ""

# Main execution
when isMainModule:
  let args = commandLineParams()

  if args.len == 0:
    stderr.writeLine("Usage: cp <matrix-file>")
    quit(1)

  let startTime = cpuTime()

  for filename in args:
    if filename.len >= 7 and filename[^7..^1] == ".matrix":
      # Parse puzzle
      let puzzle = parseMatrixFile(filename)
      printPuzzle(puzzle)

      # Initialize grid
      var grid = initGrid(puzzle)

      # Solve
      cpIterations = 0

      if cpSearch(grid):
        # Print solution
        printGrid(grid)
        echo "\nSolved in Iterations=", cpIterations, "\n"
      else:
        echo "\nNo solution found"
        echo "Iterations=", cpIterations, "\n"

  let elapsed = cpuTime() - startTime
  echo "Seconds to process ", formatFloat(elapsed, ffDecimal, 3)
