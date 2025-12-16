import os, strutils, strformat

type
  Grid = array[9, array[9, int]]

var iterations: int = 0

proc printGrid(grid: Grid) =
  for row in grid:
    for i, cell in row:
      stdout.write($cell)
      if i < 8: stdout.write(" ")
    stdout.write("\n")

proc isValid(grid: Grid, row, col, num: int): bool =
  # Check row and column
  for i in 0..8:
    if grid[row][i] == num: return false
    if grid[i][col] == num: return false

  # Check 3x3 box
  let startRow = row - (row mod 3)
  let startCol = col - (col mod 3)
  for i in 0..2:
    for j in 0..2:
      if grid[startRow + i][startCol + j] == num: return false

  return true

proc solve(grid: var Grid): bool =
  iterations += 1
  
  var row = -1
  var col = -1
  var isEmpty = false

  for r in 0..8:
    for c in 0..8:
      if grid[r][c] == 0:
        row = r
        col = c
        isEmpty = true
        break
    if isEmpty: break

  if not isEmpty: return true

  for num in 1..9:
    if isValid(grid, row, col, num):
      grid[row][col] = num
      if solve(grid): return true
      grid[row][col] = 0

  return false

proc main() =
  if paramCount() < 1:
    echo "Usage: ", getAppFilename(), " <matrix_file>"
    return

  let filename = paramStr(1)
  let content = readFile(filename)
  
  var grid: Grid
  var row = 0

  for line in content.splitLines():
    if line.len == 0: continue
    if line[0] < '0' or line[0] > '9': continue

    var col = 0
    for token in line.splitWhitespace():
      if col < 9:
        grid[row][col] = parseInt(token)
        col += 1
    
    if col > 0: row += 1
    if row >= 9: break

  if solve(grid):
    printGrid(grid)
    echo fmt"Solved in Iterations={iterations}"
  else:
    echo "No solution found."

main()
