import sys.io.File;
import sys.FileSystem;
using StringTools;

class Sudoku {
    var puzzle:Array<Array<Int>>;
    var count:Int = 0;

    public function new() {
        puzzle = [for (i in 0...9) [for (j in 0...9) 0]];
    }

    function isValid(row:Int, col:Int, val:Int):Bool {
        // Check row
        for (i in 0...9) {
            if (puzzle[row][i] == val) return false;
        }
        // Check column
        for (i in 0...9) {
            if (puzzle[i][col] == val) return false;
        }
        // Check 3x3 box
        var boxRow = Std.int(row / 3) * 3;
        var boxCol = Std.int(col / 3) * 3;
        for (i in 0...3) {
            for (j in 0...3) {
                if (puzzle[boxRow + i][boxCol + j] == val) return false;
            }
        }
        return true;
    }

    function solve():Bool {
        // Find first empty cell (row-major order)
        var row = -1;
        var col = -1;
        for (r in 0...9) {
            for (c in 0...9) {
                if (puzzle[r][c] == 0) {
                    row = r;
                    col = c;
                    break;
                }
            }
            if (row != -1) break;
        }

        // If no empty cell found, puzzle is solved
        if (row == -1) {
            printPuzzle();
            Sys.println('\nSolved in Iterations=${count}\n');
            return true;
        }

        // Try values 1-9 in order
        for (val in 1...10) {
            count++;  // COUNT BEFORE validity check - this is the algorithm fingerprint

            if (isValid(row, col, val)) {
                puzzle[row][col] = val;  // Place value

                if (solve()) {
                    return true;  // Solved
                }

                puzzle[row][col] = 0;  // Backtrack
            }
        }

        return false;  // No solution found
    }

    function printPuzzle():Void {
        Sys.println("\nPuzzle:");
        for (r in 0...9) {
            var line = "";
            for (c in 0...9) {
                if (c > 0) line += " ";
                line += Std.string(puzzle[r][c]);
            }
            Sys.println(line);
        }
    }

    function readMatrix(filename:String):Bool {
        try {
            var content = File.getContent(filename);
            var lines = content.split("\n");

            // Normalize path for output (convert absolute to relative)
            if (filename.indexOf("/app/Matrices/") == 0) {
                var displayPath = filename.substr(5);  // Skip "/app/" to get "Matrices/..."
                Sys.println('../${displayPath}');
            } else {
                Sys.println(filename);
            }

            var row = 0;
            for (line in lines) {
                var trimmed = line.trim();
                // Skip comments and empty lines
                if (trimmed.length == 0 || trimmed.charAt(0) == "#") continue;

                // Parse 9 space-separated integers
                var parts = trimmed.split(" ");
                var col = 0;
                for (part in parts) {
                    if (part.length > 0 && col < 9) {
                        puzzle[row][col] = Std.parseInt(part);
                        col++;
                    }
                }

                // Print the row as we read it (like C does)
                var lineOutput = "";
                for (c in 0...9) {
                    if (c > 0) lineOutput += " ";
                    lineOutput += Std.string(puzzle[row][c]);
                }
                Sys.println(lineOutput);

                row++;
                if (row >= 9) break;
            }

            return true;
        } catch (e:Dynamic) {
            Sys.stderr().writeString('Error opening file \'${filename}\'\n');
            return false;
        }
    }

    static function main() {
        var args = Sys.args();

        // When running with haxe --run, args might include the working directory at the end
        // Filter for only .matrix files
        var matrixFiles:Array<String> = [];
        for (arg in args) {
            if (arg.indexOf(".matrix") != -1) {
                matrixFiles.push(arg);
            }
        }

        if (matrixFiles.length == 0) {
            Sys.println("Usage: haxe --run Sudoku <matrix_file.matrix>");
            Sys.exit(1);
        }

        for (matrixFile in matrixFiles) {
            var solver = new Sudoku();
            solver.count = 0;  // Reset counter for each matrix

            if (!solver.readMatrix(matrixFile)) {
                continue;
            }

            solver.printPuzzle();
            solver.solve();
        }
    }
}
