import os, strutils, times, strformat

var puzzle: array[9, array[9, int]]
var count: int = 0

proc printPuzzle() =
  echo "\nPuzzle:"
  for j in 0..8:
    for i in 0..8:
      stdout.write($puzzle[j][i] & " ")
    echo ""

proc readMatrixFile(filename: string) =
  echo filename
  let content = readFile(filename)
  let lines = content.splitLines()
  var row = 0
  for line in lines:
    if line.startsWith("#") or line.strip() == "": continue
    let parts = line.strip().splitWhitespace()
    if parts.len == 9:
      for col in 0..8:
        puzzle[row][col] = parseInt(parts[col])
      inc(row)
      if row == 9: break

proc isPossible(y, x, val: int): bool =
  for i in 0..8:
    if puzzle[i][x] == val: return false
    if puzzle[y][i] == val: return false
  
  let x0 = (x div 3) * 3
  let y0 = (y div 3) * 3
  
  for i in 0..2:
    for j in 0..2:
      if puzzle[y0 + i][x0 + j] == val: return false
  
  return true

proc solve(): int =
  for j in 0..8:
    for i in 0..8:
      if puzzle[j][i] == 0:
        for val in 1..9:
          inc(count)
          if isPossible(j, i, val):
            puzzle[j][i] = val
            if solve() == 2: return 2
            puzzle[j][i] = 0
        return 0
  printPuzzle()
  echo "\nSolved in Iterations=", count, "\n"
  return 2

proc main() =
  let args = commandLineParams()
  let start = cpuTime()
  
  for arg in args:
    if arg.endsWith(".matrix"):
      readMatrixFile(arg)
      printPuzzle()
      count = 0
      discard solve()
      
  let finish = cpuTime()
  echo fmt"Seconds to process {finish - start:.3f}"

main()
