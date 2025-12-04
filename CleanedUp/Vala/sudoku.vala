using GLib;

public class Sudoku : Object {
    private int[,] board = new int[9, 9];
    private long iterations = 0;

    public void read_matrix(string filename) {
        try {
            var file = File.new_for_path(filename);
            var dis = new DataInputStream(file.read());
            string? line;
            int row = 0;

            while ((line = dis.read_line(null)) != null) {
                if (line.length == 0 || line[0] == '#') {
                    continue;
                }

                if (row >= 9) break;

                int col = 0;
                for (int i = 0; i < line.length; i++) {
                    unichar c = line.get_char(line.index_of_nth_char(i));
                    if (c >= '0' && c <= '9') {
                        if (col < 9) {
                            board[row, col] = (int)(c - '0');
                            col++;
                        }
                    } else if (c == '.') {
                        if (col < 9) {
                            board[row, col] = 0;
                            col++;
                        }
                    }
                }
                row++;
            }
        } catch (Error e) {
            stderr.printf("Error reading file: %s\n", e.message);
            Process.exit(1);
        }
    }

    public void print_board() {
        for (int i = 0; i < 9; i++) {
            for (int j = 0; j < 9; j++) {
                stdout.printf("%d ", board[i, j]);
            }
            stdout.printf("\n");
        }
    }

    private bool is_valid(int row, int col, int num) {
        // Row check
        for (int c = 0; c < 9; c++) {
            if (board[row, c] == num) return false;
        }

        // Col check
        for (int r = 0; r < 9; r++) {
            if (board[r, col] == num) return false;
        }

        // Box check
        int box_row = (row / 3) * 3;
        int box_col = (col / 3) * 3;

        for (int r = 0; r < 3; r++) {
            for (int c = 0; c < 3; c++) {
                if (board[box_row + r, box_col + c] == num) return false;
            }
        }

        return true;
    }

    private bool find_empty(out int row, out int col) {
        for (int r = 0; r < 9; r++) {
            for (int c = 0; c < 9; c++) {
                if (board[r, c] == 0) {
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

    public bool solve() {
        int row, col;

        if (!find_empty(out row, out col)) {
            return true;
        }

        for (int num = 1; num <= 9; num++) {
            iterations++;
            if (is_valid(row, col, num)) {
                board[row, col] = num;

                if (solve()) {
                    return true;
                }

                board[row, col] = 0;
            }
        }

        return false;
    }

    public long get_iterations() {
        return iterations;
    }

    public static int main(string[] args) {
        if (args.length < 2) {
            stdout.printf("Usage: %s <matrix_file>\n", args[0]);
            return 1;
        }

        var sudoku = new Sudoku();
        sudoku.read_matrix(args[1]);

        stdout.printf("Puzzle:\n");
        sudoku.print_board();

        if (sudoku.solve()) {
            stdout.printf("Puzzle:\n");
            sudoku.print_board();
            stdout.printf("Solved in Iterations=%ld\n", sudoku.get_iterations());
        } else {
            stdout.printf("No solution found.\n");
        }

        return 0;
    }
}
