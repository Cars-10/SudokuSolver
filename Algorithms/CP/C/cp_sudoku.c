#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include "cp.h"

// ============================================================================
// PUZZLE I/O STUBS
// ============================================================================

int readMatrixFile(const char *filename, int puzzle[9][9]) {
    // TODO: Implement in Plan 2
    // Read puzzle from matrix file
    return 0;
}

void printPuzzle(int puzzle[9][9]) {
    // TODO: Implement in Plan 2
    // Print puzzle in standard format
}

// ============================================================================
// MAIN ENTRYPOINT
// ============================================================================

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <matrix_file>\n", argv[0]);
        return 1;
    }

    printf("CP Solver stub - implementation in Plan 2\n");
    return 0;
}
