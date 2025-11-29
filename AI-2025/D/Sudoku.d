import std.stdio;
import std.file;
import std.string;
import std.conv;
import std.array;
import std.datetime.stopwatch;

int[9][9] puzzle;
long count = 0;

void printPuzzle() {
    writeln("\nPuzzle:");
    foreach (row; puzzle) {
        foreach (val; row) {
            write(val, " ");
        }
        writeln();
    }
}

void readMatrixFile(string filename) {
    writeln(filename);
    auto content = readText(filename);
    auto lines = splitLines(content);
    int row = 0;
    foreach (line; lines) {
        if (line.startsWith("#") || line.strip().length == 0) continue;
        auto parts = line.strip().split();
        if (parts.length == 9) {
            foreach (col, part; parts) {
                puzzle[row][col] = to!int(part);
            }
            row++;
            if (row == 9) break;
        }
    }
}

bool isPossible(int y, int x, int val) {
    for (int i = 0; i < 9; i++) {
        if (puzzle[i][x] == val) return false;
        if (puzzle[y][i] == val) return false;
    }

    int x0 = (x / 3) * 3;
    int y0 = (y / 3) * 3;

    for (int i = 0; i < 3; i++) {
        for (int j = 0; j < 3; j++) {
            if (puzzle[y0 + i][x0 + j] == val) return false;
        }
    }
    return true;
}

int solve() {
    for (int j = 0; j < 9; j++) {
        for (int i = 0; i < 9; i++) {
            if (puzzle[j][i] == 0) {
                for (int val = 1; val <= 9; val++) {
                    count++;
                    if (isPossible(j, i, val)) {
                        puzzle[j][i] = val;
                        if (solve() == 2) return 2;
                        puzzle[j][i] = 0;
                    }
                }
                return 0;
            }
        }
    }
    printPuzzle();
    writefln("\nSolved in Iterations=%d\n", count);
    return 2;
}

void main(string[] args) {
    auto sw = StopWatch(AutoStart.yes);
    
    foreach (arg; args[1..$]) {
        if (arg.endsWith(".matrix")) {
            readMatrixFile(arg);
            printPuzzle();
            count = 0;
            solve();
        }
    }
    
    sw.stop();
    writefln("Seconds to process %.3f", sw.peek().total!"msecs" / 1000.0);
}
