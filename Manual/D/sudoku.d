import std.stdio;
import std.file;
import std.string;
import std.conv;
import std.algorithm;
import std.array;

int[9][9] board;
long iterations = 0;

void readMatrix(string filename) {
    auto content = readText(filename);
    auto lines = content.splitLines();
    int row = 0;
    
    foreach (line; lines) {
        if (line.strip().length == 0 || line.strip()[0] == '#') {
            continue;
        }
        
        if (row >= 9) break;
        
        int col = 0;
        foreach (char c; line) {
            if (c >= '0' && c <= '9') {
                if (col < 9) {
                    board[row][col] = c - '0';
                    col++;
                }
            } else if (c == '.') {
                if (col < 9) {
                    board[row][col] = 0;
                    col++;
                }
            }
        }
        row++;
    }
}

void printBoard() {
    for (int i = 0; i < 9; i++) {
        for (int j = 0; j < 9; j++) {
            writef("%d ", board[i][j]);
        }
        writeln();
    }
}

bool isValid(int row, int col, int num) {
    // Row check
    for (int c = 0; c < 9; c++) {
        if (board[row][c] == num) return false;
    }

    // Col check
    for (int r = 0; r < 9; r++) {
        if (board[r][col] == num) return false;
    }

    // Box check
    int boxRow = (row / 3) * 3;
    int boxCol = (col / 3) * 3;

    for (int r = 0; r < 3; r++) {
        for (int c = 0; c < 3; c++) {
            if (board[boxRow + r][boxCol + c] == num) return false;
        }
    }

    return true;
}

bool findEmpty(out int row, out int col) {
    for (int r = 0; r < 9; r++) {
        for (int c = 0; c < 9; c++) {
            if (board[r][c] == 0) {
                row = r;
                col = c;
                return true;
            }
        }
    }
    row = -1;
    col = -1;
    return false;
}

bool solve() {
    int row, col;
    
    if (!findEmpty(row, col)) {
        return true;
    }

    for (int num = 1; num <= 9; num++) {
        iterations++;
        if (isValid(row, col, num)) {
            board[row][col] = num;
            
            if (solve()) {
                return true;
            }
            
            board[row][col] = 0;
        }
    }

    return false;
}

void main(string[] args) {
    if (args.length < 2) {
        writeln("Usage: ./sudoku <matrix_file>");
        return;
    }

    string filename = args[1];
    readMatrix(filename);

    writeln("Puzzle:");
    printBoard();

    if (solve()) {
        writeln("Puzzle:");
        printBoard();
        writefln("Solved in Iterations=%d", iterations);
    } else {
        writeln("No solution found.");
    }
}
