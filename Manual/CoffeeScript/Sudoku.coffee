fs = require 'fs'

class Sudoku
  constructor: (@board) ->
    @iterations = 0

  solve: ->
    empty = @findEmpty()
    return true unless empty

    [row, col] = empty

    for num in [1..9]
      if @isValid(row, col, num)
        @board[row][col] = num
        @iterations++
        
        if @solve()
          return true
        
        @board[row][col] = 0

    return false

  findEmpty: ->
    for r in [0..8]
      for c in [0..8]
        return [r, c] if @board[r][c] == 0
    return null

  isValid: (row, col, num) ->
    # Check row
    for c in [0..8]
      return false if @board[row][c] == num

    # Check column
    for r in [0..8]
      return false if @board[r][col] == num

    # Check 3x3 box
    startRow = row - (row % 3)
    startCol = col - (col % 3)
    for r in [0..2]
      for c in [0..2]
        return false if @board[r + startRow][c + startCol] == num

    return true

  printBoard: ->
    for row in @board
      console.log row.join(' ')

  getIterations: ->
    @iterations

# Main execution
if process.argv.length < 3
  console.log "Usage: coffee Sudoku.coffee <input_file>"
  process.exit(1)

inputFile = process.argv[2]
content = fs.readFileSync(inputFile, 'utf8')
lines = content.trim().split('\n')

# Parse input
board = []
for line in lines
  continue if line.trim() == ''
  row = []
  for char in line.trim()
    if char >= '0' and char <= '9'
      row.push parseInt(char)
    else
      row.push 0
  board.push row

solver = new Sudoku(board)
startTime = Date.now()
solver.solve()
endTime = Date.now()

console.log "Solved in Iterations= #{solver.getIterations()}"
# console.log "Time: #{(endTime - startTime) / 1000}s"
solver.printBoard()
