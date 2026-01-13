#import "dlx.h"
#import <limits.h>

// ============================================================================
// GLOBAL ITERATION COUNTER
// ============================================================================
int dlxIterations = 0;

// ============================================================================
// OBJECTIVE-C CLASS IMPLEMENTATIONS
// ============================================================================

@implementation DlxNode

- (instancetype)init {
    self = [super init];
    if (self) {
        up = self;
        down = self;
        left = self;
        right = self;
        column = nil;
        rowId = -1;
    }
    return self;
}

@end

@implementation DlxColumn

- (instancetype)init {
    self = [super init];
    if (self) {
        size = 0;
        memset(name, 0, sizeof(name));
    }
    return self;
}

@end

// ============================================================================
// CORE DLX ALGORITHM (C-style functions)
// ============================================================================

// Cover a column in the DLX matrix
// Remove column from header list and remove all rows in the column's list from other column lists
void coverColumn(DlxColumn* c) {
    DlxNode *colNode = (DlxNode*)c;

    // Remove column header from the header list
    colNode->right->left = colNode->left;
    colNode->left->right = colNode->right;

    // For each row in this column
    DlxNode *rowNode = colNode->down;
    while (rowNode != colNode) {
        // For each node in this row (excluding the column itself)
        DlxNode *rightNode = rowNode->right;
        while (rightNode != rowNode) {
            // Remove this node from its column
            rightNode->down->up = rightNode->up;
            rightNode->up->down = rightNode->down;
            rightNode->column->size--;
            rightNode = rightNode->right;
        }
        rowNode = rowNode->down;
    }
}

// Uncover a column (exact reverse of cover)
void uncoverColumn(DlxColumn* c) {
    DlxNode *colNode = (DlxNode*)c;

    // For each row in this column (in reverse order)
    DlxNode *rowNode = colNode->up;
    while (rowNode != colNode) {
        // For each node in this row (in reverse order)
        DlxNode *leftNode = rowNode->left;
        while (leftNode != rowNode) {
            // Restore this node to its column
            leftNode->column->size++;
            leftNode->down->up = leftNode;
            leftNode->up->down = leftNode;
            leftNode = leftNode->left;
        }
        rowNode = rowNode->up;
    }

    // Restore column header to the header list
    colNode->right->left = colNode;
    colNode->left->right = colNode;
}

// Choose column with minimum size (Knuth's S heuristic)
DlxColumn* chooseColumn(DlxColumn* root) {
    DlxNode *rootNode = (DlxNode*)root;
    DlxColumn *best = NULL;
    int minSize = INT_MAX;

    DlxNode *colNode = rootNode->right;
    while (colNode != rootNode) {
        DlxColumn *col = (DlxColumn*)colNode;
        if (col->size < minSize) {
            minSize = col->size;
            best = col;
        }
        colNode = colNode->right;
    }

    return best;
}

// DLX Search - Algorithm X with Dancing Links
// Returns 1 if solution found, 0 otherwise
// solution[] stores the row indices of the solution
int dlxSearch(DlxColumn* root, int k, int* solution) {
    dlxIterations++;  // Count every search call (analogous to brute-force iterations)

    DlxNode *rootNode = (DlxNode*)root;

    // If matrix is empty, we found a solution
    if (rootNode->right == rootNode) {
        return 1;
    }

    // Choose column with minimum size
    DlxColumn *col = chooseColumn(root);

    // If column has no rows, no solution possible
    if (col->size == 0) {
        return 0;
    }

    // Cover this column
    coverColumn(col);

    // Try each row in this column
    DlxNode *rowNode = ((DlxNode*)col)->down;
    while (rowNode != (DlxNode*)col) {
        // Add row to partial solution
        solution[k] = rowNode->rowId;  // Store row ID

        // Cover all other columns in this row
        DlxNode *rightNode = rowNode->right;
        while (rightNode != rowNode) {
            coverColumn(rightNode->column);
            rightNode = rightNode->right;
        }

        // Recurse
        if (dlxSearch(root, k + 1, solution)) {
            return 1;  // Solution found
        }

        // Backtrack: uncover all columns in this row
        DlxNode *leftNode = rowNode->left;
        while (leftNode != rowNode) {
            uncoverColumn(leftNode->column);
            leftNode = leftNode->left;
        }

        rowNode = rowNode->down;
    }

    // Uncover column
    uncoverColumn(col);

    return 0;  // No solution found
}
