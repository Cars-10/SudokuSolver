#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <sys/time.h>
#include "dlx.h"

// Sudoku puzzle grid [row][col]
int puzzle[9][9];
int solution_grid[9][9];
int DEBUG = 0;  // 0 off, 1 on

// DLX matrix structures
DlxColumn *root = nullptr;
DlxColumn *columns[324];  // 324 constraint columns
DlxNode *nodes = nullptr;
int node_count = 0;
int max_nodes = 729 * 4;  // Maximum possible nodes (729 rows * 4 constraints each)

// Row metadata to map DLX rows back to Sudoku (row, col, num)
struct RowInfo {
    int row;
    int col;
    int num;
};

RowInfo row_info[729];  // 729 possible rows (9x9x9)
DlxNode *row_starts[729];  // Pointer to first node in each row

// Calculate constraint column indices
// For a Sudoku cell (r,c) with value n (1-9):
// - Position constraint: (r,c) must be filled -> column index: r*9 + c
// - Row constraint: row r must have number n -> column index: 81 + r*9 + (n-1)
// - Column constraint: column c must have number n -> column index: 162 + c*9 + (n-1)
// - Box constraint: box b must have number n -> column index: 243 + b*9 + (n-1)
//   where b = (r/3)*3 + (c/3)

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

// Initialize DLX matrix structure
void init_dlx_matrix() {
    // Allocate root column
    root = new DlxColumn();
    std::strcpy(root->name, "root");
    root->node.left = reinterpret_cast<DlxNode*>(root);
    root->node.right = reinterpret_cast<DlxNode*>(root);
    root->node.up = reinterpret_cast<DlxNode*>(root);
    root->node.down = reinterpret_cast<DlxNode*>(root);
    root->node.column = root;
    root->node.row_id = -1;  // Root has no row
    root->size = 0;

    // Allocate 324 column headers
    for (int i = 0; i < 324; i++) {
        columns[i] = new DlxColumn();
        std::snprintf(columns[i]->name, 16, "C%d", i);
        columns[i]->size = 0;

        // Initialize as circular list
        columns[i]->node.up = reinterpret_cast<DlxNode*>(columns[i]);
        columns[i]->node.down = reinterpret_cast<DlxNode*>(columns[i]);
        columns[i]->node.column = columns[i];
        columns[i]->node.row_id = -1;  // Column headers have no row

        // Link into header list
        columns[i]->node.left = root->node.left;
        columns[i]->node.right = reinterpret_cast<DlxNode*>(root);
        root->node.left->right = reinterpret_cast<DlxNode*>(columns[i]);
        root->node.left = reinterpret_cast<DlxNode*>(columns[i]);
    }

    // Allocate node pool
    nodes = new DlxNode[max_nodes];
    node_count = 0;
}

// Add a node to the DLX matrix
DlxNode* add_node(DlxColumn* col, int row_id) {
    if (node_count >= max_nodes) {
        std::fprintf(stderr, "ERROR: Exceeded maximum node count\n");
        std::exit(1);
    }

    DlxNode *node = &nodes[node_count++];
    node->column = col;
    node->row_id = row_id;

    // Insert at end of column's circular list
    node->down = reinterpret_cast<DlxNode*>(col);
    node->up = col->node.up;
    col->node.up->down = node;
    col->node.up = node;
    col->size++;

    return node;
}

// Build a DLX row for Sudoku cell (r,c) with value n
void build_dlx_row(int r, int c, int n, int row_id) {
    // Store row metadata
    row_info[row_id].row = r;
    row_info[row_id].col = c;
    row_info[row_id].num = n;

    // Create nodes for the 4 constraints
    DlxNode *n1 = add_node(columns[get_position_col(r, c)], row_id);
    DlxNode *n2 = add_node(columns[get_row_col(r, n)], row_id);
    DlxNode *n3 = add_node(columns[get_col_col(c, n)], row_id);
    DlxNode *n4 = add_node(columns[get_box_col(r, c, n)], row_id);

    // Link nodes horizontally in circular list
    n1->right = n2;
    n2->right = n3;
    n3->right = n4;
    n4->right = n1;

    n1->left = n4;
    n2->left = n1;
    n3->left = n2;
    n4->left = n3;

    // Store first node for this row
    row_starts[row_id] = n1;
}

// Build the complete DLX matrix from the puzzle
void build_dlx_matrix_from_puzzle() {
    int row_id = 0;

    for (int r = 0; r < 9; r++) {
        for (int c = 0; c < 9; c++) {
            if (puzzle[r][c] != 0) {
                // Cell has a clue - create only one row for that value
                build_dlx_row(r, c, puzzle[r][c], row_id++);
            } else {
                // Cell is empty - create rows for all possible values
                for (int n = 1; n <= 9; n++) {
                    build_dlx_row(r, c, n, row_id++);
                }
            }
        }
    }
}

// Extract solution from DLX and populate solution_grid
void extract_solution(int *solution, int solution_len) {
    // Initialize solution grid - start with the original puzzle (includes clues)
    std::memcpy(solution_grid, puzzle, sizeof(solution_grid));

    // Each solution entry is a row_id
    for (int i = 0; i < solution_len; i++) {
        int row_id = solution[i];
        if (row_id >= 0 && row_id < 729) {
            solution_grid[row_info[row_id].row][row_info[row_id].col] = row_info[row_id].num;
        }
    }
}

// Print puzzle
void printPuzzle(int grid[9][9]) {
    std::printf("\nPuzzle:\n");
    for (int r = 0; r < 9; r++) {
        for (int c = 0; c < 9; c++) {
            std::printf("%i ", grid[r][c]);
        }
        std::printf("\n");
    }
}

// Read matrix file
int readMatrixFile(char *filename) {
    FILE *file = nullptr;
    char *line_buf = nullptr;
    size_t line_buf_size = 0;
    int line_count = 0;
    ssize_t line_size;

    file = std::fopen(filename, "r");
    if (!file) {
        std::fprintf(stderr, "Error opening file '%s'\n", filename);
        return 1;
    }

    // Normalize path for output (convert absolute to relative)
    const char *display_path = filename;
    if (std::strncmp(filename, "/app/Matrices/", 14) == 0) {
        display_path = filename + 5;  // Skip "/app/" to get "Matrices/..."
        std::printf("../%s\n", display_path);
    } else {
        std::printf("%s\n", filename);
    }

    line_size = getline(&line_buf, &line_buf_size, file);
    while (line_size >= 0) {
        // Skip comments and empty lines
        if (line_buf[0] != '#' && line_buf[0] != '\n' && line_buf[0] != '\r') {
            if (DEBUG)
                std::printf("line[%06d]: chars=%06zd, contents: %s",
                       line_count, line_size, line_buf);

            int parsed_count =
                std::sscanf(line_buf, "%i %i %i %i %i %i %i %i %i",
                       &puzzle[line_count][0], &puzzle[line_count][1],
                       &puzzle[line_count][2], &puzzle[line_count][3],
                       &puzzle[line_count][4], &puzzle[line_count][5],
                       &puzzle[line_count][6], &puzzle[line_count][7],
                       &puzzle[line_count][8]);

            if (parsed_count == 9) {
                if (line_count < 9) {
                    for (int i = 0; i < 9; i++)
                        std::printf("%i ", puzzle[line_count][i]);
                    std::printf("\n");
                    line_count++;
                }
            } else {
                std::printf("Error: line does not contain 9 integers\n");
                std::printf("line[%06d]: chars=%06zd, contents: %s",
                       line_count, line_size, line_buf);
                return 1;
            }
        }
        line_size = getline(&line_buf, &line_buf_size, file);
    }
    std::fclose(file);
    free(line_buf);
    return 0;
}

// Free DLX matrix memory
void free_dlx_matrix() {
    if (root) delete root;
    for (int i = 0; i < 324; i++) {
        if (columns[i]) delete columns[i];
    }
    if (nodes) delete[] nodes;
}

// Cover given clues (pre-selected rows)
void cover_clues() {
    for (int r = 0; r < 9; r++) {
        for (int c = 0; c < 9; c++) {
            if (puzzle[r][c] != 0) {
                int n = puzzle[r][c];

                // Find the row for this clue
                for (int row_id = 0; row_id < 729; row_id++) {
                    if (row_starts[row_id] &&
                        row_info[row_id].row == r &&
                        row_info[row_id].col == c &&
                        row_info[row_id].num == n) {

                        // Cover all columns in this row
                        DlxNode *node = row_starts[row_id];
                        DlxNode *curr = node;
                        do {
                            dlx_cover_column(curr->column);
                            curr = curr->right;
                        } while (curr != node);
                        break;
                    }
                }
            }
        }
    }
}

int main(int argc, char **argv) {
    struct timeval stop, start;
    gettimeofday(&start, nullptr);

    // Process each .matrix file from command line
    for (int i = 1; i < argc; i++) {
        char *point = std::strrchr(argv[i], '.');
        if (point != nullptr && std::strcmp(point, ".matrix") == 0) {
            if (readMatrixFile(argv[i]) != 0) {
                std::fprintf(stderr, "Error reading %s\n", argv[i]);
                continue;
            }

            printPuzzle(puzzle);

            // Initialize DLX matrix
            init_dlx_matrix();

            // Build matrix from puzzle
            build_dlx_matrix_from_puzzle();

            // Cover pre-filled clues
            cover_clues();

            // Solve using DLX
            dlx_iterations = 0;
            int solution[81];
            int result = dlx_search(root, 0, solution);

            if (result) {
                extract_solution(solution, 81);
                printPuzzle(solution_grid);
                std::printf("\nSolved in Iterations=%i\n\n", dlx_iterations);
            } else {
                std::printf("\nNo solution found\n");
            }

            // Cleanup
            free_dlx_matrix();
        }
    }

    gettimeofday(&stop, nullptr);
    std::printf("Seconds to process %.3f\n",
           (stop.tv_sec - start.tv_sec) +
               (stop.tv_usec - start.tv_usec) / 1000000.0);

    return 0;
}
