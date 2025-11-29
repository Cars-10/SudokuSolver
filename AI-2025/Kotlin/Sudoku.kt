import java.io.File
import kotlin.system.measureNanoTime

var puzzle = Array(9) { IntArray(9) }
var count = 0
val DEBUG = 0

fun printPuzzle() {
    println("\nPuzzle:")
    for (j in 0 until 9) {
        for (i in 0 until 9) {
            print("${puzzle[j][i]} ")
        }
        println()
    }
}

fun readMatrixFile(filename: String) {
    println(filename)
    val lines = File(filename).readLines()
    var row = 0
    for (line in lines) {
        if (line.startsWith("#") || line.isBlank()) continue
        val parts = line.trim().split("\\s+".toRegex())
        if (parts.size == 9) {
            for (col in 0 until 9) {
                puzzle[row][col] = parts[col].toInt()
            }
            row++
            if (row == 9) break
        }
    }
}

fun isPossible(y: Int, x: Int, `val`: Int): Boolean {
    for (i in 0 until 9) {
        if (puzzle[i][x] == `val`) return false
        if (puzzle[y][i] == `val`) return false
    }

    val x0 = (x / 3) * 3
    val y0 = (y / 3) * 3

    for (i in 0 until 3) {
        for (j in 0 until 3) {
            if (puzzle[y0 + i][x0 + j] == `val`) return false
        }
    }
    return true
}

fun solve(): Int {
    for (j in 0 until 9) {
        for (i in 0 until 9) {
            if (puzzle[j][i] == 0) {
                for (`val` in 1..9) {
                    count++
                    if (isPossible(j, i, `val`)) {
                        puzzle[j][i] = `val`
                        if (solve() == 2) return 2
                        puzzle[j][i] = 0
                    }
                }
                return 0
            }
        }
    }
    printPuzzle()
    println("\nSolved in Iterations=$count\n")
    return 2
}

fun main(args: Array<String>) {
    val time = measureNanoTime {
        for (arg in args) {
            if (arg.endsWith(".matrix")) {
                readMatrixFile(arg)
                printPuzzle()
                count = 0
                solve()
            }
        }
    }
    println("Seconds to process %.3f".format(time / 1_000_000_000.0))
}
