import "os" for Process
import "io" for File

class Sudoku {
    construct new() {
        _puzzle = List.filled(9, null)
        for (i in 0...9) {
            _puzzle[i] = List.filled(9, 0)
        }
        _count = 0
    }

    printPuzzle() {
        System.print("\nPuzzle:")
        for (row in 0...9) {
            var line = ""
            for (col in 0...9) {
                line = line + _puzzle[row][col].toString + " "
            }
            System.print(line)
        }
    }

    readMatrixFile(filename) {
        var content = File.read(filename)
        var lines = content.split("\n")
        
        // Normalize path for output
        if (filename.startsWith("/app/Matrices/")) {
            System.print("../" + filename[5...filename.count])
        } else {
            System.print(filename)
        }

        var lineCount = 0
        for (line in lines) {
            line = line.trim()
            if (line == "" || line.startsWith("#")) continue
            
            var parts = line.split(" ").where { |s| s != "" }.toList
            if (parts.count == 9) {
                if (lineCount < 9) {
                    var outLine = ""
                    for (i in 0...9) {
                        var val = Num.fromString(parts[i])
                        _puzzle[lineCount][i] = val
                        outLine = outLine + val.toString + " "
                    }
                    System.print(outLine)
                    lineCount = lineCount + 1
                }
            }
        }
        return true
    }

    isValid(row, col, val) {
        // Check row
        for (i in 0...9) {
            if (_puzzle[row][i] == val) return false
        }
        // Check column
        for (i in 0...9) {
            if (_puzzle[i][col] == val) return false
        }
        // Check 3x3 box
        var boxRow = (row / 3).floor * 3
        var boxCol = (col / 3).floor * 3
        for (i in 0...3) {
            for (j in 0...3) {
                if (_puzzle[boxRow + i][boxCol + j] == val) return false
            }
        }
        return true
    }

    solve() {
        // Find first empty cell (row-major order)
        var row = -1
        var col = -1
        
        var found = false
        for (r in 0...9) {
            for (c in 0...9) {
                if (_puzzle[r][c] == 0) {
                    row = r
                    col = c
                    found = true
                    break
                }
            }
            if (found) break
        }

        if (!found) {
            printPuzzle()
            System.print("\nSolved in Iterations=%(_count)\n")
            return true
        }

        for (val in 1..9) {
            _count = _count + 1
            if (isValid(row, col, val)) {
                _puzzle[row][col] = val
                if (solve()) return true
                _puzzle[row][col] = 0
            }
        }
        return false
    }
}

var start = System.clock
for (arg in Process.arguments) {
    var s = Sudoku.new()
    if (s.readMatrixFile(arg)) {
        s.printPuzzle()
        s.solve()
    }
}
var end = System.clock
System.print("Seconds to process %(end - start)")
