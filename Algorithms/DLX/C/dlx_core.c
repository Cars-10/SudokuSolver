#include "dlx.h"

// Global iteration counter (analogous to brute-force count)
int dlx_iterations = 0;

// Cover a column in the DLX matrix
// Remove column from header list and remove all rows in the column's list from other column lists
void dlx_cover_column(DlxColumn* c) {
    DlxNode *col_node = (DlxNode*)c;

    // Remove column header from the header list
    col_node->right->left = col_node->left;
    col_node->left->right = col_node->right;

    // For each row in this column
    DlxNode *row_node = col_node->down;
    while (row_node != col_node) {
        // For each node in this row (excluding the column itself)
        DlxNode *right_node = row_node->right;
        while (right_node != row_node) {
            // Remove this node from its column
            right_node->down->up = right_node->up;
            right_node->up->down = right_node->down;
            right_node->column->size--;
            right_node = right_node->right;
        }
        row_node = row_node->down;
    }
}

// Uncover a column (exact reverse of cover)
void dlx_uncover_column(DlxColumn* c) {
    DlxNode *col_node = (DlxNode*)c;

    // For each row in this column (in reverse order)
    DlxNode *row_node = col_node->up;
    while (row_node != col_node) {
        // For each node in this row (in reverse order)
        DlxNode *left_node = row_node->left;
        while (left_node != row_node) {
            // Restore this node to its column
            left_node->column->size++;
            left_node->down->up = left_node;
            left_node->up->down = left_node;
            left_node = left_node->left;
        }
        row_node = row_node->up;
    }

    // Restore column header to the header list
    col_node->right->left = col_node;
    col_node->left->right = col_node;
}

// Choose column with minimum size (Knuth's S heuristic)
DlxColumn* choose_column(DlxColumn* root) {
    DlxNode *root_node = (DlxNode*)root;
    DlxColumn *best = NULL;
    int min_size = __INT_MAX__;

    DlxNode *col_node = root_node->right;
    while (col_node != root_node) {
        DlxColumn *col = (DlxColumn*)col_node;
        if (col->size < min_size) {
            min_size = col->size;
            best = col;
        }
        col_node = col_node->right;
    }

    return best;
}

// DLX Search - Algorithm X with Dancing Links
// Returns 1 if solution found, 0 otherwise
// solution[] stores the row indices of the solution
int dlx_search(DlxColumn* root, int k, int* solution) {
    dlx_iterations++;  // Count every search call (analogous to brute-force iterations)

    DlxNode *root_node = (DlxNode*)root;

    // If matrix is empty, we found a solution
    if (root_node->right == root_node) {
        return 1;
    }

    // Choose column with minimum size
    DlxColumn *col = choose_column(root);

    // If column has no rows, no solution possible
    if (col->size == 0) {
        return 0;
    }

    // Cover this column
    dlx_cover_column(col);

    // Try each row in this column
    DlxNode *row_node = ((DlxNode*)col)->down;
    while (row_node != (DlxNode*)col) {
        // Add row to partial solution
        solution[k] = row_node->row_id;  // Store row ID

        // Cover all other columns in this row
        DlxNode *right_node = row_node->right;
        while (right_node != row_node) {
            dlx_cover_column(right_node->column);
            right_node = right_node->right;
        }

        // Recurse
        if (dlx_search(root, k + 1, solution)) {
            return 1;  // Solution found
        }

        // Backtrack: uncover all columns in this row
        DlxNode *left_node = row_node->left;
        while (left_node != row_node) {
            dlx_uncover_column(left_node->column);
            left_node = left_node->left;
        }

        row_node = row_node->down;
    }

    // Uncover column
    dlx_uncover_column(col);

    return 0;  // No solution found
}
