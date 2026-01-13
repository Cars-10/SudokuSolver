#ifndef DLX_H
#define DLX_H

#include <cstdio>
#include <cstdlib>
#include <cstring>

// Forward declaration
class DlxNode;
class DlxColumn;

class DlxNode {
public:
    DlxNode *up;
    DlxNode *down;
    DlxNode *left;
    DlxNode *right;
    DlxColumn *column;  // Pointer to the column header
    int row_id;         // ID of the row this node belongs to

    DlxNode() : up(nullptr), down(nullptr), left(nullptr), right(nullptr),
                column(nullptr), row_id(-1) {}
};

class DlxColumn {
public:
    DlxNode node;       // Inherits from DlxNode (must be first)
    int size;           // Number of nodes in this column
    char name[16];      // For debugging/identification

    DlxColumn() : size(0) {
        name[0] = '\0';
    }
};

// Global iteration counter
extern int dlx_iterations;

// Core functions
void dlx_cover_column(DlxColumn* c);
void dlx_uncover_column(DlxColumn* c);
DlxColumn* choose_column(DlxColumn* root);
int dlx_search(DlxColumn* root, int k, int* solution);

#endif // DLX_H
