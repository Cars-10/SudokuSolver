import Foundation

// 9x9 Grid
var puzzle = [[Int]](repeating: [Int](repeating: 0, count: 9), count: 9)
var iterations = 0

func printPuzzle() {
    print("\nPuzzle:")
    for i in 0..<9 {
        for j in 0..<9 {
            print("\(puzzle[i][j]) ", terminator: "")
        }
        print("")
    }
}

func isValid(row: Int, col: Int, val: Int) -> Bool {
    // Check row
    for i in 0..<9 {
        if puzzle[row][i] == val { return false }
    }
    
    // Check col
    for i in 0..<9 {
        if puzzle[i][col] == val { return false }
    }
    
    // Check 3x3 box
    let startRow = (row / 3) * 3
    let startCol = (col / 3) * 3
    for i in 0..<3 {
        for j in 0..<3 {
            if puzzle[startRow + i][startCol + j] == val { return false }
        }
    }
    return true
}

// Exact match of C brute-force algorithm:
// 1. Find first empty cell (row-major order)
// 2. Try 1-9
// 3. Count EVERY attempt
func solve() -> Bool {
    var row = -1
    var col = -1
    
    // Find first empty cell
    outerLoop: for r in 0..<9 {
        for c in 0..<9 {
            if puzzle[r][c] == 0 {
                row = r
                col = c
                break outerLoop
            }
        }
    }
    
    // If no empty cell, solved
    if row == -1 {
        return true
    }
    
    for val in 1...9 {
        iterations += 1
        
        if isValid(row: row, col: col, val: val) {
            puzzle[row][col] = val
            if solve() {
                return true
            }
            puzzle[row][col] = 0 // Backtrack
        }
    }
    
    return false
}

func readMatrix(filename: String) -> Bool {
    guard let content = try? String(contentsOfFile: filename, encoding: .utf8) else {
        print("Error reading file \(filename)")
        return false
    }
    
    // Print filename exactly as passed (to match C output if possible, or close to it)
    print(filename)
    
    let lines = content.components(separatedBy: .newlines)
    var row = 0
    
    for line in lines {
        if row >= 9 { break }
        
        let trimmed = line.trimmingCharacters(in: .whitespaces)
        if trimmed.isEmpty || trimmed.hasPrefix("#") { continue }
        
        // Split by whitespace
        let parts = trimmed.components(separatedBy: .whitespaces).filter { !$0.isEmpty }
        
        if parts.count >= 9 {
            // Echo the parsed row
            for col in 0..<9 {
                if let val = Int(parts[col]) {
                    puzzle[row][col] = val
                    print("\(val) ", terminator: "")
                }
            }
            print("") // Newline after row echo
            row += 1
        }
    }
    
    return row == 9
}

// Main execution
let args = CommandLine.arguments
if args.count < 2 {
    print("Usage: Sudoku <matrix_file>")
    exit(1)
}

// Only process the first file for now (to match the pattern of other scripts handling one-by-one or looping themselves)
// But the benchmark runner passes multiple files. We'll handle arguments properly.

// Process all files passed
for i in 1..<args.count {
    let filename = args[i]
    
    // Reset state
    iterations = 0
    puzzle = [[Int]](repeating: [Int](repeating: 0, count: 9), count: 9)
    
    if readMatrix(filename: filename) {
        printPuzzle()
        
        if solve() {
            print("\nPuzzle:")
            for r in 0..<9 {
                for c in 0..<9 {
                    print("\(puzzle[r][c]) ", terminator: "")
                }
                print("")
            }
            print("\nSolved in Iterations=\(iterations)")
        } else {
            print("No solution found.")
        }
    }
}