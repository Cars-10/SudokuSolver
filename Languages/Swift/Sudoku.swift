import Foundation

var puzzle = [[Int]](repeating: [Int](repeating: 0, count: 9), count: 9)
var iterations = 0

func printBoard() {
    print("\nPuzzle:")
    for i in 0..<9 {
        for j in 0..<9 {
            print("\(puzzle[i][j]) ", terminator: "")
        }
        print("")
    }
}

func isPossible(row: Int, col: Int, num: Int) -> Bool {
    for i in 0..<9 {
        if puzzle[row][i] == num || puzzle[i][col] == num {
            return false
        }
    }
    let startRow = (row / 3) * 3
    let startCol = (col / 3) * 3
    for i in 0..<3 {
        for j in 0..<3 {
            if puzzle[startRow + i][startCol + j] == num {
                return false
            }
        }
    }
    return true
}

func solve(row: Int, col: Int) -> Bool {
    if row == 9 {
        return true
    }
    var nextRow = row
    var nextCol = col + 1
    if nextCol == 9 {
        nextRow = row + 1
        nextCol = 0
    }

    if puzzle[row][col] != 0 {
        return solve(row: nextRow, col: nextCol)
    }

    for num in 1...9 {
        iterations += 1
        if isPossible(row: row, col: col, num: num) {
            puzzle[row][col] = num
            if solve(row: nextRow, col: nextCol) {
                return true
            }
            puzzle[row][col] = 0
        }
    }
    return false
}

func readBoard(filename: String) -> Bool {
    guard let content = try? String(contentsOfFile: filename, encoding: .utf8) else {
        fputs("Error reading file \(filename)\n", stderr)
        return false
    }

    // Print the path first (just the path, like C reference)
    print(filename)

    let lines = content.components(separatedBy: .newlines)
    var row = 0
    for line in lines {
        let trimmed = line.trimmingCharacters(in: .whitespaces)
        if trimmed.isEmpty || trimmed.hasPrefix("#") {
            continue
        }
        let parts = trimmed.components(separatedBy: .whitespaces).filter { !$0.isEmpty }
        var col = 0
        for part in parts {
            if col < 9, let val = Int(part) {
                puzzle[row][col] = val
                col += 1
            }
        }
        // Echo row after parsing (like C reference)
        if col == 9 {
            for c in 0..<9 {
                print("\(puzzle[row][c]) ", terminator: "")
            }
            print("")
        }
        row += 1
        if row == 9 { break }
    }
    return true
}

if CommandLine.arguments.count < 2 {
    print("Usage: swift Sudoku.swift <file1> <file2> ...")
    exit(1)
}

let startTime = Date()

for i in 1..<CommandLine.arguments.count {
    let filename = CommandLine.arguments[i]

    // Reset puzzle for each file
    puzzle = [[Int]](repeating: [Int](repeating: 0, count: 9), count: 9)

    if readBoard(filename: filename) {
        printBoard()
        iterations = 0
        if solve(row: 0, col: 0) {
            printBoard()
            print("\nSolved in Iterations=\(iterations)\n")
        } else {
            print("No solution found")
        }
    }
}

let elapsed = Date().timeIntervalSince(startTime)
print(String(format: "Seconds to process %.3f", elapsed))
