import sys.io.File;
import sys.FileSystem;

class Sudoku {
    static var puzzle:Array<Array<Int>>;
    static var count:Int = 0;

    static function printPuzzle() {
        Sys.println("\nPuzzle:");
        for (r in 0...9) {
            for (c in 0...9) {
                Sys.print(puzzle[r][c] + " ");
            }
            Sys.println("");
        }
    }

    static function readMatrixFile(filename:String) {
        Sys.println(filename);
        puzzle = [for (i in 0...9) [for (j in 0...9) 0]];
        var content = File.getContent(filename);
        var lines = content.split("\n");
        var row = 0;
        for (line in lines) {
            var trimmed = StringTools.trim(line);
            if (trimmed.length == 0 || StringTools.startsWith(trimmed, "#")) continue;
            
            var parts = ~/\s+/g.split(trimmed);
            var validParts = [];
            for (p in parts) {
                if (p.length > 0) validParts.push(p);
            }
            
            if (validParts.length == 9) {
                for (col in 0...9) {
                    puzzle[row][col] = Std.parseInt(validParts[col]);
                }
                row++;
                if (row == 9) break;
            }
        }
    }

    static function isPossible(r:Int, c:Int, val:Int):Bool {
        for (i in 0...9) {
            if (puzzle[i][c] == val) return false;
            if (puzzle[r][i] == val) return false;
        }

        var r0 = Math.floor(r / 3) * 3;
        var c0 = Math.floor(c / 3) * 3;

        for (i in 0...3) {
            for (j in 0...3) {
                if (puzzle[r0 + i][c0 + j] == val) return false;
            }
        }
        return true;
    }

    static function solve():Bool {
        for (r in 0...9) {
            for (c in 0...9) {
                if (puzzle[r][c] == 0) {
                    for (val in 1...10) {
                        count++;
                        if (isPossible(r, c, val)) {
                            puzzle[r][c] = val;
                            if (solve()) return true;
                            puzzle[r][c] = 0;
                        }
                    }
                    return false;
                }
            }
        }
        printPuzzle();
        Sys.println("\nSolved in Iterations=" + count + "\n");
        return true;
    }

    static function main() {
        var start = Sys.time();
        var args = Sys.args();
        for (arg in args) {
            if (StringTools.endsWith(arg, ".matrix")) {
                readMatrixFile(arg);
                printPuzzle();
                count = 0;
                solve();
            }
        }
        var end = Sys.time();
        Sys.println("Seconds to process " + Math.round((end - start) * 1000) / 1000);
    }
}
