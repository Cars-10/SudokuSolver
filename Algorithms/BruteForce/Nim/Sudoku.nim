import os, strutils, times

# Sudoku puzzle grid [row][col]
var puzzle: array[9, array[9, int]]
var count: int = 0  # Iteration counter

proc printPuzzle() =
  echo "\nPuzzle:"
  for j in 0 ..< 9:
    for i in 0 ..< 9:
      stdout.write(puzzle[j][i], " ")
    echo ""

proc readMatrixFile(filename: string): int =
  var file: File
  if not open(file, filename, fmRead):
    stderr.writeLine("Error opening file '", filename, "'")
    return 1

  # Normalize path for output (convert absolute to relative)
  if filename.len >= 14 and filename[0..13] == "/app/Matrices/":
    echo "../", filename[5..^1]
  else:
    echo filename

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
      for i in 0 ..< 9:
        puzzle[lineCount][i] = parseInt(parts[i])
        stdout.write(puzzle[lineCount][i], " ")
      echo ""
      lineCount += 1

  file.close()
  return 0

# Check if placing val at (row, col) is valid
proc isValid(row, col, val: int): bool =
  # Check row
  for i in 0 ..< 9:
    if puzzle[row][i] == val:
      return false

  # Check column
  for i in 0 ..< 9:
    if puzzle[i][col] == val:
      return false

  # Check 3x3 box
  let boxRow = (row div 3) * 3
  let boxCol = (col div 3) * 3
  for i in 0 ..< 3:
    for j in 0 ..< 3:
      if puzzle[boxRow + i][boxCol + j] == val:
        return false

  return true

# BRUTE-FORCE SOLVER
# Searches row-major order (top-to-bottom, left-to-right)
# Tries candidates 1-9 in ascending order
# Counts EVERY placement attempt (the algorithm fingerprint)
proc solve(): bool =
  # Find first empty cell (row-major order)
  var row = -1
  var col = -1
  block findEmpty:
    for r in 0 ..< 9:
      for c in 0 ..< 9:
        if puzzle[r][c] == 0:
          row = r
          col = c
          break findEmpty

  # If no empty cell found, puzzle is solved
  if row == -1:
    printPuzzle()
    echo "\nSolved in Iterations=", count, "\n"
    return true  # Success

  # Try values 1-9 in order
  for val in 1 .. 9:
    count += 1  # COUNT EVERY ATTEMPT - this is the algorithm fingerprint

    if isValid(row, col, val):
      puzzle[row][col] = val  # Place value

      if solve():
        return true  # Solved

      puzzle[row][col] = 0  # Backtrack

  return false  # No solution found

proc main() =
  let startTime = cpuTime()

  # Process each .matrix file from command line
  let args = commandLineParams()
  for i in 0 ..< args.len:
    let filename = args[i]
    if filename.len >= 7 and filename[^7..^1] == ".matrix":
      if readMatrixFile(filename) != 0:
        stderr.writeLine("Error reading ", filename)
        continue

      printPuzzle()
      count = 0
      discard solve()

  let elapsed = cpuTime() - startTime
  echo "Seconds to process ", formatFloat(elapsed, ffDecimal, 3)

main()
