import os, strutils

var puzzle: array[9, array[9, int]]
var iterations = 0

proc printBoard() =
  echo "Puzzle:"
  for i in 0..8:
    for j in 0..8:
      stdout.write($puzzle[i][j] & " ")
    echo ""

proc isPossible(row, col, num: int): bool =
  for i in 0..8:
    if puzzle[row][i] == num or puzzle[i][col] == num:
      return false
  
  let startRow = (row div 3) * 3
  let startCol = (col div 3) * 3
  for i in 0..2:
    for j in 0..2:
      if puzzle[startRow + i][startCol + j] == num:
        return false
  return true

proc solve(row, col: int): bool =
  if row == 9:
    return true
  
  var nextRow = row
  var nextCol = col + 1
  if nextCol == 9:
    nextRow = row + 1
    nextCol = 0
  
  if puzzle[row][col] != 0:
    return solve(nextRow, nextCol)
  
  for num in 1..9:
    inc(iterations)
    if isPossible(row, col, num):
      puzzle[row][col] = num
      if solve(nextRow, nextCol):
        return true
      puzzle[row][col] = 0
  
  return false

proc readBoard(filename: string): bool =
  try:
    let content = readFile(filename)
    let lines = content.splitLines()
    var row = 0
    for line in lines:
      let trimmed = line.strip()
      if trimmed.len > 0 and not trimmed.startsWith("#"):
        let parts = trimmed.splitWhitespace()
        var col = 0
        for part in parts:
          if col < 9:
            puzzle[row][col] = parseInt(part)
            inc(col)
        inc(row)
        if row == 9:
          return true
    return true
  except:
    echo "Error reading file ", filename
    return false

if paramCount() == 0:
  echo "Usage: ./Sudoku <file1> <file2> ..."
else:
  for filename in commandLineParams():
    echo "\nProcessing ", filename
    if readBoard(filename):
      printBoard()
      iterations = 0
      if solve(0, 0):
        printBoard()
        echo "\nSolved in Iterations=", iterations
      else:
        echo "No solution found"
