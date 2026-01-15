#import <Foundation/Foundation.h>
#import <stdio.h>
#import <stdlib.h>
#import <string.h>
#import <sys/time.h>
#import "dlx.h"

// ============================================================================
// SUDOKU PUZZLE STRUCTURE
// ============================================================================
int puzzle[9][9];
int solutionGrid[9][9];
int DEBUG = 0;  // 0 off, 1 on

// DLX matrix structures
DlxColumn *root = NULL;
DlxColumn *columns[324];  // 324 constraint columns
DlxNode **nodePool = NULL;  // Array of node pointers
int nodeCount = 0;
int maxNodes = 729 * 4;  // Maximum possible nodes (729 rows * 4 constraints each)

// Row metadata to map DLX rows back to Sudoku (row, col, num)
typedef struct {
    int row;
    int col;
    int num;
} RowInfo;

RowInfo rowInfo[729];  // 729 possible rows (9x9x9)
DlxNode *rowStarts[729];  // Pointer to first node in each row

// ============================================================================
// CONSTRAINT COLUMN CALCULATION
// ============================================================================

// Calculate constraint column indices
// For a Sudoku cell (r,c) with value n (1-9):
// - Position constraint: (r,c) must be filled -> column index: r*9 + c
// - Row constraint: row r must have number n -> column index: 81 + r*9 + (n-1)
// - Column constraint: column c must have number n -> column index: 162 + c*9 + (n-1)
// - Box constraint: box b must have number n -> column index: 243 + b*9 + (n-1)
//   where b = (r/3)*3 + (c/3)

int getPositionCol(int r, int c) {
    return r * 9 + c;
}

int getRowCol(int r, int n) {
    return 81 + r * 9 + (n - 1);
}

int getColCol(int c, int n) {
    return 162 + c * 9 + (n - 1);
}

int getBoxCol(int r, int c, int n) {
    int box = (r / 3) * 3 + (c / 3);
    return 243 + box * 9 + (n - 1);
}

// ============================================================================
// DLX MATRIX INITIALIZATION
// ============================================================================

void initDlxMatrix() {
    // Allocate root column
    root = [[DlxColumn alloc] init];
    strcpy(root->name, "root");
    root->up = (DlxNode*)root;
    root->down = (DlxNode*)root;
    root->left = (DlxNode*)root;
    root->right = (DlxNode*)root;
    root->column = root;
    root->rowId = -1;
    root->size = 0;

    // Allocate 324 column headers
    for (int i = 0; i < 324; i++) {
        columns[i] = [[DlxColumn alloc] init];
        snprintf(columns[i]->name, 16, "C%d", i);
        columns[i]->size = 0;

        // Initialize as circular list
        columns[i]->up = (DlxNode*)columns[i];
        columns[i]->down = (DlxNode*)columns[i];
        columns[i]->column = columns[i];
        columns[i]->rowId = -1;

        // Link into header list
        columns[i]->left = root->left;
        columns[i]->right = (DlxNode*)root;
        root->left->right = (DlxNode*)columns[i];
        root->left = (DlxNode*)columns[i];
    }

    // Allocate node pool (array of pointers to DlxNode objects)
    nodePool = (DlxNode**)malloc(maxNodes * sizeof(DlxNode*));
    memset(nodePool, 0, maxNodes * sizeof(DlxNode*));
    nodeCount = 0;
}

// ============================================================================
// DLX MATRIX BUILDING
// ============================================================================

// Add a node to the DLX matrix
DlxNode* addNode(DlxColumn* col, int rowId) {
    if (nodeCount >= maxNodes) {
        fprintf(stderr, "ERROR: Exceeded maximum node count\n");
        exit(1);
    }

    DlxNode *node = [[DlxNode alloc] init];
    nodePool[nodeCount++] = node;
    node->column = col;
    node->rowId = rowId;

    // Insert at end of column's circular list
    node->down = (DlxNode*)col;
    node->up = col->up;
    col->up->down = node;
    col->up = node;
    col->size++;

    return node;
}

// Build a DLX row for Sudoku cell (r,c) with value n
void buildDlxRow(int r, int c, int n, int rowId) {
    // Store row metadata
    rowInfo[rowId].row = r;
    rowInfo[rowId].col = c;
    rowInfo[rowId].num = n;

    // Create nodes for the 4 constraints
    DlxNode *n1 = addNode(columns[getPositionCol(r, c)], rowId);
    DlxNode *n2 = addNode(columns[getRowCol(r, n)], rowId);
    DlxNode *n3 = addNode(columns[getColCol(c, n)], rowId);
    DlxNode *n4 = addNode(columns[getBoxCol(r, c, n)], rowId);

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
    rowStarts[rowId] = n1;
}

// Build the complete DLX matrix from the puzzle
void buildDlxMatrixFromPuzzle() {
    int rowId = 0;

    for (int r = 0; r < 9; r++) {
        for (int c = 0; c < 9; c++) {
            if (puzzle[r][c] != 0) {
                // Cell has a clue - create only one row for that value
                buildDlxRow(r, c, puzzle[r][c], rowId++);
            } else {
                // Cell is empty - create rows for all possible values
                for (int n = 1; n <= 9; n++) {
                    buildDlxRow(r, c, n, rowId++);
                }
            }
        }
    }
}

// Cover given clues (pre-selected rows)
void coverClues() {
    for (int r = 0; r < 9; r++) {
        for (int c = 0; c < 9; c++) {
            if (puzzle[r][c] != 0) {
                int n = puzzle[r][c];

                // Find the row for this clue
                for (int rowId = 0; rowId < 729; rowId++) {
                    if (rowStarts[rowId] &&
                        rowInfo[rowId].row == r &&
                        rowInfo[rowId].col == c &&
                        rowInfo[rowId].num == n) {

                        // Cover all columns in this row
                        DlxNode *node = rowStarts[rowId];
                        DlxNode *curr = node;
                        do {
                            coverColumn(curr->column);
                            curr = curr->right;
                        } while (curr != node);
                        break;
                    }
                }
            }
        }
    }
}

// ============================================================================
// SOLUTION EXTRACTION
// ============================================================================

// Extract solution from DLX and populate solutionGrid
void extractSolution(int *solution, int solutionLen) {
    // Initialize solution grid - start with the original puzzle (includes clues)
    memcpy(solutionGrid, puzzle, sizeof(solutionGrid));

    // Each solution entry is a rowId
    for (int i = 0; i < solutionLen; i++) {
        int rowId = solution[i];
        if (rowId >= 0 && rowId < 729) {
            solutionGrid[rowInfo[rowId].row][rowInfo[rowId].col] = rowInfo[rowId].num;
        }
    }
}

// ============================================================================
// PUZZLE I/O
// ============================================================================

// Print puzzle
void printPuzzle(int grid[9][9]) {
    printf("\nPuzzle:\n");
    for (int r = 0; r < 9; r++) {
        for (int c = 0; c < 9; c++) {
            printf("%i ", grid[r][c]);
        }
        printf("\n");
    }
}

// Read matrix file
int readMatrixFile(char *filename) {
    FILE *file = NULL;
    char *lineBuf = NULL;
    size_t lineBufSize = 0;
    int lineCount = 0;
    ssize_t lineSize;

    file = fopen(filename, "r");
    if (!file) {
        fprintf(stderr, "Error opening file '%s'\n", filename);
        return 1;
    }

    // Normalize path for output (convert absolute to relative)
    const char *displayPath = filename;
    if (strncmp(filename, "/app/Matrices/", 14) == 0) {
        displayPath = filename + 5;  // Skip "/app/" to get "Matrices/..."
        printf("../%s\n", displayPath);
    } else {
        printf("%s\n", filename);
    }

    lineSize = getline(&lineBuf, &lineBufSize, file);
    while (lineSize >= 0) {
        // Skip comments and empty lines
        if (lineBuf[0] != '#' && lineBuf[0] != '\n' && lineBuf[0] != '\r') {
            if (DEBUG)
                printf("line[%06d]: chars=%06zd, contents: %s",
                       lineCount, lineSize, lineBuf);

            int parsedCount =
                sscanf(lineBuf, "%i %i %i %i %i %i %i %i %i",
                       &puzzle[lineCount][0], &puzzle[lineCount][1],
                       &puzzle[lineCount][2], &puzzle[lineCount][3],
                       &puzzle[lineCount][4], &puzzle[lineCount][5],
                       &puzzle[lineCount][6], &puzzle[lineCount][7],
                       &puzzle[lineCount][8]);

            if (parsedCount == 9) {
                if (lineCount < 9) {
                    for (int i = 0; i < 9; i++)
                        printf("%i ", puzzle[lineCount][i]);
                    printf("\n");
                    lineCount++;
                }
            } else {
                printf("Error: line does not contain 9 integers\n");
                printf("line[%06d]: chars=%06zd, contents: %s",
                       lineCount, lineSize, lineBuf);
                return 1;
            }
        }
        lineSize = getline(&lineBuf, &lineBufSize, file);
    }
    fclose(file);
    free(lineBuf);
    return 0;
}

// Free DLX matrix memory
void freeDlxMatrix() {
    if (nodePool) free(nodePool);
}

// ============================================================================
// MAIN ENTRYPOINT
// ============================================================================

int main(int argc, char **argv) {
    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
    struct timeval stop, start;
    gettimeofday(&start, NULL);

    // Process each .matrix file from command line
    for (int i = 1; i < argc; i++) {
        char *point = strrchr(argv[i], '.');
        if (point != NULL && strcmp(point, ".matrix") == 0) {
            if (readMatrixFile(argv[i]) != 0) {
                fprintf(stderr, "Error reading %s\n", argv[i]);
                continue;
            }

            printPuzzle(puzzle);

            // Initialize DLX matrix
            initDlxMatrix();

            // Build matrix from puzzle
            buildDlxMatrixFromPuzzle();

            // Cover pre-filled clues
            coverClues();

            // Solve using DLX
            dlxIterations = 0;
            int solution[81];
            int result = dlxSearch(root, 0, solution);

            if (result) {
                extractSolution(solution, 81);
                printPuzzle(solutionGrid);
                printf("\nSolved in Iterations=%i\n\n", dlxIterations);
            } else {
                printf("\nNo solution found\n");
            }

            // Cleanup
            freeDlxMatrix();
        }
    }

    gettimeofday(&stop, NULL);
    printf("Seconds to process %.3f\n",
           (stop.tv_sec - start.tv_sec) +
               (stop.tv_usec - start.tv_usec) / 1000000.0);
    [pool drain];

    return 0;
}
