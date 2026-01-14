// DLX (Dancing Links) Sudoku Solver in Vala
// Implementation of Knuth's Algorithm X with Dancing Links

using GLib;

// DLX Node class - circular doubly-linked structure
public class DlxNode : Object {
    public DlxNode? up;
    public DlxNode? down;
    public DlxNode? left;
    public DlxNode? right;
    public DlxNode? column;
    public int size = 0;
    public int row_id = -1;
    public int col_id = -1;
    public string name = "";

    public DlxNode() {}
}

// Row metadata to map DLX rows back to Sudoku (row, col, num)
public class RowInfo : Object {
    public int row;
    public int col;
    public int num;

    public RowInfo(int r, int c, int n) {
        row = r;
        col = c;
        num = n;
    }
}

// Global state (initialized in main)
public int dlx_iterations = 0;
public int[,] puzzle;
public int[,] solution_grid;
public DlxNode root;
public DlxNode[] columns;
public List<RowInfo> row_info;
public DlxNode?[] row_starts;

// Calculate constraint column indices
int get_position_col(int r, int c) {
    return r * 9 + c;
}

int get_row_col(int r, int n) {
    return 81 + r * 9 + (n - 1);
}

int get_col_col(int c, int n) {
    return 162 + c * 9 + (n - 1);
}

int get_box_col(int r, int c, int n) {
    int box = (r / 3) * 3 + (c / 3);
    return 243 + box * 9 + (n - 1);
}

// Cover a column in the DLX matrix
void cover_column(DlxNode col) {
    // Remove column header from the header list
    col.right.left = col.left;
    col.left.right = col.right;

    // For each row in this column
    DlxNode? row_node = col.down;
    while (row_node != col) {
        // For each node in this row (excluding the column itself)
        DlxNode? right_node = row_node.right;
        while (right_node != row_node) {
            // Remove this node from its column
            right_node.down.up = right_node.up;
            right_node.up.down = right_node.down;
            right_node.column.size--;
            right_node = right_node.right;
        }
        row_node = row_node.down;
    }
}

// Uncover a column (exact reverse of cover)
void uncover_column(DlxNode col) {
    // For each row in this column (in reverse order)
    DlxNode? row_node = col.up;
    while (row_node != col) {
        // For each node in this row (in reverse order)
        DlxNode? left_node = row_node.left;
        while (left_node != row_node) {
            // Restore this node to its column
            left_node.column.size++;
            left_node.down.up = left_node;
            left_node.up.down = left_node;
            left_node = left_node.left;
        }
        row_node = row_node.up;
    }

    // Restore column header to the header list
    col.right.left = col;
    col.left.right = col;
}

// Choose column with minimum size (Knuth's S heuristic)
DlxNode? choose_column(DlxNode root) {
    DlxNode? best = null;
    int min_size = int.MAX;

    DlxNode? col_node = root.right;
    while (col_node != root) {
        if (col_node.size < min_size) {
            min_size = col_node.size;
            best = col_node;
        }
        col_node = col_node.right;
    }

    return best;
}

// DLX Search - Algorithm X with Dancing Links
bool dlx_search(DlxNode root, int k, int[] solution) {
    dlx_iterations++;  // Count every search call

    // If matrix is empty, we found a solution
    if (root.right == root) {
        return true;
    }

    // Choose column with minimum size
    DlxNode? col = choose_column(root);
    if (col == null || col.size == 0) {
        return false;
    }

    // Cover this column
    cover_column(col);

    // Try each row in this column
    DlxNode? row_node = col.down;
    while (row_node != col) {
        // Add row to partial solution
        solution[k] = row_node.row_id;

        // Cover all other columns in this row
        DlxNode? right_node = row_node.right;
        while (right_node != row_node) {
            cover_column(right_node.column);
            right_node = right_node.right;
        }

        // Recurse
        if (dlx_search(root, k + 1, solution)) {
            return true;  // Solution found
        }

        // Backtrack: uncover all columns in this row
        DlxNode? left_node = row_node.left;
        while (left_node != row_node) {
            uncover_column(left_node.column);
            left_node = left_node.left;
        }

        row_node = row_node.down;
    }

    // Uncover column
    uncover_column(col);

    return false;  // No solution found
}

// Initialize DLX matrix with column headers
void init_dlx_matrix() {
    root = new DlxNode();
    root.name = "root";

    // Create 324 column headers
    for (int i = 0; i < 324; i++) {
        columns[i] = new DlxNode();
        columns[i].name = "C%d".printf(i);
        columns[i].size = 0;
        columns[i].column = columns[i];  // Column header points to itself
        columns[i].up = columns[i];
        columns[i].down = columns[i];
    }

    // Link column headers horizontally
    DlxNode? prev = root;
    for (int i = 0; i < 324; i++) {
        prev.right = columns[i];
        columns[i].left = prev;
        prev = columns[i];
    }
    prev.right = root;
    root.left = prev;

    // Initialize row_starts array
    for (int i = 0; i < 729; i++) {
        row_starts[i] = null;
    }
}

// Add a row to the DLX matrix
void add_dlx_row(int row_id, int[] constraint_cols) {
    if (constraint_cols.length != 4) return;

    DlxNode? first_node = null;
    DlxNode? prev_node = null;

    foreach (int col_idx in constraint_cols) {
        DlxNode node = new DlxNode();
        node.row_id = row_id;
        node.column = columns[col_idx];

        // Link vertically in column
        node.up = columns[col_idx].up;
        node.down = columns[col_idx];
        columns[col_idx].up.down = node;
        columns[col_idx].up = node;
        columns[col_idx].size++;

        // Link horizontally in row
        if (first_node == null) {
            first_node = node;
            row_starts[row_id] = node;
        }

        if (prev_node != null) {
            prev_node.right = node;
            node.left = prev_node;
        }

        prev_node = node;
    }

    // Complete circular horizontal links
    if (first_node != null && prev_node != null) {
        prev_node.right = first_node;
        first_node.left = prev_node;
    }
}

// Build exact cover matrix for Sudoku
void build_sudoku_matrix() {
    int row_id = 0;

    for (int r = 0; r < 9; r++) {
        for (int c = 0; c < 9; c++) {
            if (puzzle[r, c] != 0) {
                // Given clue - add single row for this value
                int num = puzzle[r, c];
                int[] cols = {
                    get_position_col(r, c),
                    get_row_col(r, num),
                    get_col_col(c, num),
                    get_box_col(r, c, num)
                };

                add_dlx_row(row_id, cols);
                row_info.append(new RowInfo(r, c, num));
                row_id++;
            } else {
                // Empty cell - add row for each possible digit 1-9
                for (int num = 1; num <= 9; num++) {
                    int[] cols = {
                        get_position_col(r, c),
                        get_row_col(r, num),
                        get_col_col(c, num),
                        get_box_col(r, c, num)
                    };

                    add_dlx_row(row_id, cols);
                    row_info.append(new RowInfo(r, c, num));
                    row_id++;
                }
            }
        }
    }
}

// Cover given clues (pre-selected rows)
void cover_clues() {
    for (int r = 0; r < 9; r++) {
        for (int c = 0; c < 9; c++) {
            if (puzzle[r, c] != 0) {
                int num = puzzle[r, c];

                // Find the row for this clue
                for (int row_id = 0; row_id < 729; row_id++) {
                    DlxNode? node = row_starts[row_id];
                    if (node != null) {
                        RowInfo info = row_info.nth_data((uint)row_id);
                        if (info.row == r && info.col == c && info.num == num) {
                            // Cover all columns in this row
                            DlxNode? curr = node;
                            do {
                                cover_column(curr.column);
                                curr = curr.right;
                            } while (curr != node);
                            break;
                        }
                    }
                }
            }
        }
    }
}

// Extract solution from DLX result
void extract_solution(int[] solution, int solution_len) {
    // Initialize solution grid with original puzzle
    for (int r = 0; r < 9; r++) {
        for (int c = 0; c < 9; c++) {
            solution_grid[r, c] = puzzle[r, c];
        }
    }

    // Fill in solution
    for (int i = 0; i < solution_len; i++) {
        int row_id = solution[i];
        if (row_id >= 0 && row_id < row_info.length()) {
            RowInfo info = row_info.nth_data((uint)row_id);
            solution_grid[info.row, info.col] = info.num;
        }
    }
}

// Print puzzle
void print_puzzle(int[,] grid) {
    stdout.printf("\nPuzzle:\n");
    for (int row = 0; row < 9; row++) {
        for (int col = 0; col < 9; col++) {
            stdout.printf("%d ", grid[row, col]);
        }
        stdout.printf("\n");
    }
}

// Read matrix file
bool read_matrix_file(string filename) {
    // Normalize path for output
    if (filename.has_prefix("/app/Matrices/")) {
        string display_path = filename.substring(5);
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

void main(string[] args) {
    // Initialize global arrays
    puzzle = new int[9, 9];
    solution_grid = new int[9, 9];
    columns = new DlxNode[324];
    row_info = new List<RowInfo>();
    row_starts = new DlxNode?[729];

    var start_time = GLib.get_monotonic_time();

    for (int i = 1; i < args.length; i++) {
        if (!args[i].has_suffix(".matrix")) {
            continue;
        }

        if (!read_matrix_file(args[i])) {
            stderr.printf("Error reading %s\n", args[i]);
            continue;
        }

        print_puzzle(puzzle);

        // Initialize DLX structures
        init_dlx_matrix();
        build_sudoku_matrix();
        cover_clues();

        // Solve using DLX
        dlx_iterations = 0;
        int[] solution = new int[81];
        if (dlx_search(root, 0, solution)) {
            extract_solution(solution, 81);
            print_puzzle(solution_grid);
            stdout.printf("\nSolved in Iterations=%d\n\n", dlx_iterations);
        } else {
            stdout.printf("No solution found\n");
        }

        // Clear for next puzzle
        row_info = new List<RowInfo>();
    }

    var elapsed = (GLib.get_monotonic_time() - start_time) / 1000000.0;
    stdout.printf("Seconds to process %.3f\n", elapsed);
}
