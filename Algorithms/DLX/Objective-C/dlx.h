#ifndef DLX_H
#define DLX_H

#import <Foundation/Foundation.h>

// Forward declaration
@class DlxColumn;

// ============================================================================
// DLX NODE - Base node in the dancing links structure
// ============================================================================
@interface DlxNode : NSObject {
@public
    DlxNode *up;
    DlxNode *down;
    DlxNode *left;
    DlxNode *right;
    DlxColumn *column;  // Pointer to the column header
    int rowId;          // ID of the row this node belongs to
}
@end

// ============================================================================
// DLX COLUMN - Column header (inherits from DlxNode)
// ============================================================================
@interface DlxColumn : DlxNode {
@public
    int size;           // Number of nodes in this column
    char name[16];      // For debugging/identification
}
@end

// ============================================================================
// GLOBAL ITERATION COUNTER
// ============================================================================
extern int dlxIterations;

// ============================================================================
// CORE DLX FUNCTIONS (C-style for algorithm)
// ============================================================================
void coverColumn(DlxColumn* c);
void uncoverColumn(DlxColumn* c);
DlxColumn* chooseColumn(DlxColumn* root);
int dlxSearch(DlxColumn* root, int k, int* solution);

#endif // DLX_H
