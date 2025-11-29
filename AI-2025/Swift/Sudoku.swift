import Foundation

var puzzle = [[Int]](repeating: [Int](repeating: 0, count: 9), count: 9)
var count = 0
let DEBUG = 0

func printPuzzle() {
    print("\nPuzzle:")
    for j in 0..<9 {
        for i in 0..<9 {
            print("\(puzzle[j][i]) ", terminator: "")
        }
        print("")
    }
}

func readMatrixFile(_ filename: String) {
    print(filename)
    do {
        let content = try String(contentsOfFile: filename)
        let lines = content.components(separatedBy: .newlines)
        var row = 0
        for line in lines {
            if line.isEmpty || line.hasPrefix("#") { continue }
            let parts = line.split(separator: " ").map { Int($0)! }
            if parts.count == 9 {
                for col in 0..<9 {
                    puzzle[row][col] = parts[col]
                }
                row += 1
                if row == 9 { break }
            }
        }
    } catch {
        print("Error reading file: \(error)")
    }
}

func isPossible(_ y: Int, _ x: Int, _ val: Int) -> Bool {
    for i in 0..<9 {
        if puzzle[i][x] == val { return false }
        if puzzle[y][i] == val { return false }
    }
    
    let x0 = (x / 3) * 3
    let y0 = (y / 3) * 3
    
    for i in 0..<3 {
        for j in 0..<3 {
            if puzzle[y0 + i][x0 + j] == val { return false }
        }
    }
    return true
}

func solve() -> Int {
    for j in 0..<9 {
        for i in 0..<9 {
            if puzzle[j][i] == 0 {
                for val in 1...9 {
                    count += 1
                    if isPossible(j, i, val) {
                        puzzle[j][i] = val
                        if solve() == 2 { return 2 }
                        puzzle[j][i] = 0
                    }
                }
                return 0
            }
        }
    }
    printPuzzle()
    print("\nSolved in Iterations=\(count)\n")
    return 2
}

let start = DispatchTime.now()

for arg in CommandLine.arguments.dropFirst() {
    if arg.hasSuffix(".matrix") {
        readMatrixFile(arg)
        printPuzzle()
        count = 0
        _ = solve()
    }
}

let end = DispatchTime.now()
let nanoTime = end.uptimeNanoseconds - start.uptimeNanoseconds
let timeInterval = Double(nanoTime) / 1_000_000_000
print(String(format: "Seconds to process %.3f", timeInterval))
