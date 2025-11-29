using GLib;

public class Sudoku : Object {
    private int[,] puzzle = new int[9,9];
    private long count = 0;

    public void print_puzzle() {
        stdout.printf("\nPuzzle:\n");
        for (int r = 0; r < 9; r++) {
            for (int c = 0; c < 9; c++) {
                stdout.printf("%d ", puzzle[r,c]);
            }
            stdout.printf("\n");
        }
    }

    public void read_matrix_file(string filename) {
        stdout.printf("%s\n", filename);
        try {
            var file = File.new_for_path(filename);
            var dis = new DataInputStream(file.read());
            string line;
            int row = 0;
            while ((line = dis.read_line(null)) != null) {
                string trimmed = line.strip();
                if (trimmed.length == 0 || trimmed.has_prefix("#")) continue;
                
                string[] parts = trimmed.split_set(" \t");
                int col = 0;
                foreach (string part in parts) {
                    if (part.length > 0) {
                        puzzle[row,col] = int.parse(part);
                        col++;
                    }
                }
                if (col == 9) {
                    row++;
                    if (row == 9) break;
                }
            }
        } catch (Error e) {
            stderr.printf("Error: %s\n", e.message);
        }
    }

    private bool is_possible(int r, int c, int val) {
        for (int i = 0; i < 9; i++) {
            if (puzzle[i,c] == val) return false;
            if (puzzle[r,i] == val) return false;
        }

        int r0 = (r / 3) * 3;
        int c0 = (c / 3) * 3;

        for (int i = 0; i < 3; i++) {
            for (int j = 0; j < 3; j++) {
                if (puzzle[r0 + i, c0 + j] == val) return false;
            }
        }
        return true;
    }

    public bool solve() {
        for (int r = 0; r < 9; r++) {
            for (int c = 0; c < 9; c++) {
                if (puzzle[r,c] == 0) {
                    for (int val = 1; val <= 9; val++) {
                        count++;
                        if (is_possible(r, c, val)) {
                            puzzle[r,c] = val;
                            if (solve()) return true;
                            puzzle[r,c] = 0;
                        }
                    }
                    return false;
                }
            }
        }
        print_puzzle();
        stdout.printf("\nSolved in Iterations=%lld\n\n", count);
        return true;
    }

    public static int main(string[] args) {
        double start = Timer.get_monotonic();
        var sudoku = new Sudoku();
        
        for (int i = 1; i < args.length; i++) {
            if (args[i].has_suffix(".matrix")) {
                sudoku.read_matrix_file(args[i]);
                sudoku.print_puzzle();
                sudoku.count = 0;
                sudoku.solve();
            }
        }
        
        double end = Timer.get_monotonic();
        stdout.printf("Seconds to process %.3f\n", end - start);
        return 0;
    }
}
