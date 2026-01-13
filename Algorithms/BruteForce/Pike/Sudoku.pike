#!/usr/bin/env pike

array(array(int)) puzzle = allocate(9);
int count = 0;

void print_puzzle() {
    write("\nPuzzle:\n");
    for (int i = 0; i < 9; i++) {
        for (int j = 0; j < 9; j++) {
            write(sprintf("%d ", puzzle[i][j]));
        }
        write("\n");
    }
}

int is_valid(int row, int col, int val) {
    // Check row
    for (int i = 0; i < 9; i++) {
        if (puzzle[row][i] == val) return 0;
    }
    // Check column
    for (int i = 0; i < 9; i++) {
        if (puzzle[i][col] == val) return 0;
    }
    // Check 3x3 box
    int box_row = (row / 3) * 3;
    int box_col = (col / 3) * 3;
    for (int i = 0; i < 3; i++) {
        for (int j = 0; j < 3; j++) {
            if (puzzle[box_row + i][box_col + j] == val) return 0;
        }
    }
    return 1;
}

int solve() {
    // Find empty cell (row-major)
    int row = -1;
    int col = -1;
    int found_empty = 0;

    for (int r = 0; r < 9; r++) {
        for (int c = 0; c < 9; c++) {
            if (puzzle[r][c] == 0) {
                row = r;
                col = c;
                found_empty = 1;
                break;
            }
        }
        if (found_empty) break;
    }

    if (!found_empty) {
        print_puzzle();
        write(sprintf("\nSolved in Iterations=%d\n\n", count));
        return 1;
    }

    for (int val = 1; val <= 9; val++) {
        count++; // COUNT BEFORE is_valid
        if (is_valid(row, col, val)) {
            puzzle[row][col] = val;
            if (solve()) return 1;
            puzzle[row][col] = 0;
        }
    }
    return 0;
}

int read_matrix_file(string filename) {
    string content = Stdio.read_file(filename);
    if (!content) {
        write("Error reading file: " + filename + "\n");
        return 0;
    }

    // Normalize path for output
    if (has_prefix(filename, "/app/Matrices/")) {
        write("../" + filename[5..] + "\n");
    } else {
        write(filename + "\n");
    }

    array(string) lines = content / "\n";
    int line_count = 0;

    foreach (lines, string line) {
        line = String.trim_all_whites(line);
        if (sizeof(line) == 0 || line[0] == '#') continue;

        array(string) parts = line / " " - ({ "" });
        if (sizeof(parts) == 9) {
            if (line_count < 9) {
                puzzle[line_count] = allocate(9);
                for (int i = 0; i < 9; i++) {
                    puzzle[line_count][i] = (int)parts[i];
                    write(sprintf("%d ", puzzle[line_count][i]));
                }
                write("\n");
                line_count++;
            }
        }
    }
    return 1;
}

int main(int argc, array(string) argv) {
    float start = (float)gethrtime();
    
    for (int i = 1; i < argc; i++) {
        string filename = argv[i];
        count = 0;
        puzzle = allocate(9); // Reset puzzle
        if (read_matrix_file(filename)) {
            print_puzzle();
            solve();
        }
    }
    
    float end = (float)gethrtime();
    write(sprintf("Seconds to process %.3f\n", (end - start) / 1000000000.0));
    return 0;
}