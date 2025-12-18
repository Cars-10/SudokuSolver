// Sudoku Solver in Vala
// Brute-force backtracking algorithm matching C reference implementation

using GLib;

public class Sudoku : Object {
    private int[,] puzzle = new int[9, 9];
    private int count = 0;

    private bool is_valid(int row, int col, int val) {
        // Check row
        for (int i = 0; i < 9; i++) {
            if (puzzle[row, i] == val) return false;
        }

        // Check column
        for (int i = 0; i < 9; i++) {
            if (puzzle[i, col] == val) return false;
        }

        // Check 3x3 box
        int br = (row / 3) * 3;
        int bc = (col / 3) * 3;
        for (int i = 0; i < 3; i++) {
            for (int j = 0; j < 3; j++) {
                if (puzzle[br + i, bc + j] == val) return false;
            }
        }

        return true;
    }

    private void print_puzzle() {
        stdout.printf("\nPuzzle:\n");
        for (int row = 0; row < 9; row++) {
            for (int col = 0; col < 9; col++) {
                stdout.printf("%d ", puzzle[row, col]);
            }
            stdout.printf("\n");
        }
    }

    private bool solve() {
        // Find first empty cell (row-major order)
        int row = -1;
        int col = -1;

        for (int r = 0; r < 9; r++) {
            for (int c = 0; c < 9; c++) {
                if (puzzle[r, c] == 0) {
                    row = r;
                    col = c;
                    break;
                }
            }
            if (row >= 0) break;
        }

        // If no empty cell found, puzzle is solved
        if (row == -1) {
            return true;
        }

        // Try values 1-9 in order
        for (int val = 1; val <= 9; val++) {
            count++;  // COUNT BEFORE validity check - algorithm fingerprint
            if (is_valid(row, col, val)) {
                puzzle[row, col] = val;

                if (solve()) {
                    return true;
                }

                puzzle[row, col] = 0;  // Backtrack
            }
        }

        return false;
    }

    private bool read_matrix_file(string filename) {
        // Normalize path for output (convert absolute to relative)
        if (filename.has_prefix("/app/Matrices/")) {
            string display_path = filename.substring(5);  // Skip "/app/" to get "Matrices/..."
            stdout.printf("../%s\n", display_path);
        } else {
            stdout.printf("%s\n", filename);
        }

        try {
            var file = File.new_for_path(filename);
            var dis = new DataInputStream(file.read());
            string? line;
            int line_count = 0;

            while ((line = dis.read_line(null)) != null) {
                string trimmed = line.strip();
                if (trimmed.length == 0 || trimmed.has_prefix("#")) {
                    continue;
                }

                string[] parts = trimmed.split_set(" \t");
                int[] values = new int[9];
                int val_count = 0;

                foreach (string part in parts) {
                    if (part.length > 0 && val_count < 9) {
                        values[val_count] = int.parse(part);
                        val_count++;
                    }
                }

                if (val_count >= 9 && line_count < 9) {
                    for (int i = 0; i < 9; i++) {
                        puzzle[line_count, i] = values[i];
                        stdout.printf("%d ", values[i]);
                    }
                    stdout.printf("\n");
                    line_count++;
                }

                if (line_count >= 9) break;
            }

            return line_count == 9;
        } catch (Error e) {
            stderr.printf("Error reading file: %s\n", e.message);
            return false;
        }
    }

    public static int main(string[] args) {
        var start_time = GLib.get_monotonic_time();

        for (int i = 1; i < args.length; i++) {
            if (!args[i].has_suffix(".matrix")) {
                continue;
            }

            var sudoku = new Sudoku();

            if (!sudoku.read_matrix_file(args[i])) {
                stderr.printf("Error reading %s\n", args[i]);
                continue;
            }

            sudoku.print_puzzle();

            sudoku.count = 0;
            if (sudoku.solve()) {
                sudoku.print_puzzle();
                stdout.printf("\nSolved in Iterations=%d\n\n", sudoku.count);
            } else {
                stdout.printf("No solution found\n");
            }
        }

        var elapsed = (GLib.get_monotonic_time() - start_time) / 1000000.0;
        stdout.printf("Seconds to process %.3f\n", elapsed);

        return 0;
    }
}
