import sys.io.File;
import sys.FileSystem;

class Sudoku {
    var board:Array<Array<Int>>;
    var iterations:Int = 0;

    public function new(board:Array<Array<Int>>) {
        this.board = board;
    }

    public function solve():Bool {
        var empty = findEmpty();
        if (empty == null) return true;

        var row = empty[0];
        var col = empty[1];

        for (num in 1...10) {
            if (isValid(row, col, num)) {
                board[row][col] = num;
                iterations++;

                if (solve()) return true;

                board[row][col] = 0;
            }
        }
        return false;
    }

    function findEmpty():Array<Int> {
        for (r in 0...9) {
            for (c in 0...9) {
                if (board[r][c] == 0) return [r, c];
            }
        }
        return null;
    }

    function isValid(row:Int, col:Int, num:Int):Bool {
        // Check row
        for (c in 0...9) {
            if (board[row][c] == num) return false;
        }

        // Check column
        for (r in 0...9) {
            if (board[r][col] == num) return false;
        }

        // Check 3x3 box
        var startRow = row - (row % 3);
        var startCol = col - (col % 3);
        for (r in 0...3) {
            for (c in 0...3) {
                if (board[r + startRow][c + startCol] == num) return false;
            }
        }

        return true;
    }

    public function printBoard():Void {
        for (row in board) {
            trace(row.join(" "));
        }
    }
    
    public function printBoardClean():Void {
        for (row in board) {
            Sys.println(row.join(" "));
        }
    }

    public function getIterations():Int {
        return iterations;
    }

    static function main() {
        var args = Sys.args();
        if (args.length < 1) {
            Sys.println("Usage: haxe -main Sudoku -interp -- <input_file>");
            Sys.exit(1);
        }

        var inputFile = args[0];
        var content = File.getContent(inputFile);
        var lines = content.split("\n");

        var board:Array<Array<Int>> = [];
        for (line in lines) {
            var trimmed = StringTools.trim(line);
            if (trimmed.length == 0) continue;
            
            var row:Array<Int> = [];
            for (i in 0...trimmed.length) {
                var charCode = trimmed.charCodeAt(i);
                if (charCode >= 48 && charCode <= 57) { // '0' to '9'
                    row.push(charCode - 48);
                } else {
                    row.push(0);
                }
            }
            board.push(row);
        }

        var solver = new Sudoku(board);
        solver.solve();

        Sys.println("Solved in Iterations= " + solver.getIterations());
        solver.printBoardClean();
    }
}
