fs = require 'fs'

puzzle = []
count = 0

printPuzzle = ->
  console.log "\nPuzzle:"
  for r in [0..8]
    line = ""
    for c in [0..8]
      line += "#{puzzle[r][c]} "
    console.log line

readMatrixFile = (filename) ->
  console.log filename
  content = fs.readFileSync(filename, 'utf8')
  lines = content.split('\n')
  puzzle = []
  row = 0
  for line in lines
    line = line.trim()
    if line.length > 0 and line[0] isnt '#'
      parts = line.split(/\s+/)
      if parts.length is 9
        puzzle[row] = (parseInt(p) for p in parts)
        row++
        break if row is 9

isPossible = (r, c, val) ->
  for i in [0..8]
    return false if puzzle[i][c] is val
    return false if puzzle[r][i] is val
  
  r0 = Math.floor(r / 3) * 3
  c0 = Math.floor(c / 3) * 3
  
  for i in [0..2]
    for j in [0..2]
      return false if puzzle[r0 + i][c0 + j] is val
  true

solve = ->
  for r in [0..8]
    for c in [0..8]
      if puzzle[r][c] is 0
        for val in [1..9]
          count++
          if isPossible(r, c, val)
            puzzle[r][c] = val
            return true if solve()
            puzzle[r][c] = 0
        return false
  printPuzzle()
  console.log "\nSolved in Iterations=#{count}\n"
  true

main = ->
  start = Date.now()
  args = process.argv.slice(2)
  for arg in args
    if arg.endsWith('.matrix')
      readMatrixFile(arg)
      printPuzzle()
      count = 0
      solve()
  
  end = Date.now()
  console.log "Seconds to process #{(end - start) / 1000}"

main()
