fs = require 'fs'

class Sudoku
  constructor: ->
    @board = []
    @iterations = 0
    for i in [0..8]
      @board.push([0, 0, 0, 0, 0, 0, 0, 0, 0])

  setCell: (row, col, val) ->
    @board[row][col] = val

  getCell: (row, col) ->
    @board[row][col]

  printBoard: ->
    console.log "\nPuzzle:"
    for row in @board
      console.log row.join(' ')

  isValid: (row, col, val) ->
    # Check row
    for c in [0..8]
      return false if @board[row][c] == val

    # Check column
    for r in [0..8]
      return false if @board[r][col] == val

    # Check 3x3 box
    boxRow = Math.floor(row / 3) * 3
    boxCol = Math.floor(col / 3) * 3
    for r in [0..2]
      for c in [0..2]
        return false if @board[boxRow + r][boxCol + c] == val

    return true

  solve: ->
    # Find first empty cell (row-major order)
    for row in [0..8]
      for col in [0..8]
        if @board[row][col] == 0
          # Try values 1-9
          for val in [1..9]
            @iterations++  # COUNT BEFORE validity check
            if @isValid(row, col, val)
              @board[row][col] = val
              if @solve()
                return true
              @board[row][col] = 0  # Backtrack
          return false  # No valid value found
    return true  # No empty cell = solved

  getIterations: ->
    @iterations

# Main execution
if process.argv.length < 3
  console.log "Usage: coffee Sudoku.coffee <input_file>"
  process.exit(1)

inputFile = process.argv[2]
content = fs.readFileSync(inputFile, 'utf8')
lines = content.trim().split('\n')

# Normalize path for output (convert absolute to relative)
displayPath = inputFile
if inputFile.startsWith('/app/Matrices/')
  displayPath = '../' + inputFile.substring(5)  # Skip "/app/" to get "Matrices/..."
console.log displayPath

# Create solver
solver = new Sudoku()

# Parse input and print as we go (like C does)
row = 0
for line in lines
  trimmed = line.trim()
  continue if trimmed == '' or trimmed.startsWith('#')

  nums = trimmed.split(/\s+/)
  if nums.length == 9 and row < 9
    for col in [0..8]
      solver.setCell(row, col, parseInt(nums[col]) or 0)
    # Print input line (space-separated like C)
    console.log solver.board[row].join(' ')
    row++

# Print "\nPuzzle:" header and input again (like C does before solve)
solver.printBoard()

# Solve
solver.solve()

# Print solved puzzle and iterations
solver.printBoard()
console.log "\nSolved in Iterations=#{solver.getIterations()}\n"
