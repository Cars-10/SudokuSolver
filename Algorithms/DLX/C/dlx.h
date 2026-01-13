#ifndef DLX_H
#define DLX_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Forward declaration
typedef struct DlxNode DlxNode;
typedef struct DlxColumn DlxColumn;

struct DlxNode {
    DlxNode *up;
    DlxNode *down;
    DlxNode *left;
    DlxNode *right;
    DlxColumn *column;  // Pointer to the column header
    int row_id;         // ID of the row this node belongs to
};

struct DlxColumn {
    DlxNode node;       // Inherits from DlxNode (must be first)
    int size;           // Number of nodes in this column
    char name[16];      // For debugging/identification
};

// Global iteration counter
extern int dlx_iterations;

// Core functions
void dlx_cover_column(DlxColumn* c);
void dlx_uncover_column(DlxColumn* c);
int dlx_search(DlxColumn* root, int k, int* solution);

#endif // DLX_H
