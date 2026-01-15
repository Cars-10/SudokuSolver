#import <Foundation/Foundation.h>
#import <stdio.h>
#import <stdlib.h>
#import <string.h>
#import <sys/time.h>
#import "cp.h"

// ============================================================================
// PUZZLE I/O
// ============================================================================

int readMatrixFile(const char *filename, int puzzle[9][9]) {
    FILE *file = fopen(filename, "r");
    if (!file) {
        fprintf(stderr, "Error opening file '%s'\n", filename);
        return 0;
    }

    // Normalize path for output (convert absolute to relative)
    const char *displayPath = filename;
    if (strncmp(filename, "/app/Matrices/", 14) == 0) {
        displayPath = filename + 5;  // Skip "/app/" to get "Matrices/..."
        printf("../%s\n", displayPath);
    } else {
        printf("%s\n", filename);
    }

    char *lineBuf = NULL;
    size_t lineBufSize = 0;
    ssize_t lineSize;
    int lineCount = 0;

    lineSize = getline(&lineBuf, &lineBufSize, file);
    while (lineSize >= 0 && lineCount < 9) {
        // Skip comments and empty lines
        if (lineBuf[0] != '#' && lineBuf[0] != '\n' && lineBuf[0] != '\r') {
            int parsedCount = sscanf(lineBuf, "%i %i %i %i %i %i %i %i %i",
                                      &puzzle[lineCount][0], &puzzle[lineCount][1],
                                      &puzzle[lineCount][2], &puzzle[lineCount][3],
                                      &puzzle[lineCount][4], &puzzle[lineCount][5],
                                      &puzzle[lineCount][6], &puzzle[lineCount][7],
                                      &puzzle[lineCount][8]);

            if (parsedCount == 9) {
                for (int i = 0; i < 9; i++) {
                    printf("%i ", puzzle[lineCount][i]);
                }
                printf("\n");
                lineCount++;
            } else {
                fprintf(stderr, "Error: line does not contain 9 integers\n");
                fclose(file);
                free(lineBuf);
                return 0;
            }
        }
        lineSize = getline(&lineBuf, &lineBufSize, file);
    }

    fclose(file);
    free(lineBuf);
    return (lineCount == 9) ? 1 : 0;
}

void printPuzzle(int puzzle[9][9]) {
    printf("\nPuzzle:\n");
    for (int r = 0; r < 9; r++) {
        for (int c = 0; c < 9; c++) {
            printf("%i ", puzzle[r][c]);
        }
        printf("\n");
    }
}

// ============================================================================
// MAIN ENTRYPOINT
// ============================================================================

int main(int argc, char *argv[]) {
    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
    struct timeval stop, start;
    gettimeofday(&start, NULL);

    if (argc != 2) {
        fprintf(stderr, "Usage: %s <matrix_file>\n", argv[0]);
        [pool drain];
        return 1;
    }

        // Read puzzle from file
        int puzzle[9][9];
        if (!readMatrixFile(argv[1], puzzle)) {
            fprintf(stderr, "Failed to read matrix file\n");
            return 1;
        }

        // Print initial puzzle
        printPuzzle(puzzle);

        // Initialize CP grid
        CPGrid *grid = [[CPGrid alloc] init];
        initGrid(grid, puzzle);

        // Apply initial propagation
        if (!propagate(grid)) {
            printf("\nNo solution found (contradiction during initial propagation)\n");
            gettimeofday(&stop, NULL);
            printf("Seconds to process %.3f\n",
                   (stop.tv_sec - start.tv_sec) +
                   (stop.tv_usec - start.tv_usec) / 1000000.0);
            return 0;
        }

        // Run search
        int solution[81];
        int solved = cpSearch(grid, solution);

        if (solved) {
            // Convert solution array back to 2D for printing
            int solutionGrid[9][9];
            for (int r = 0; r < 9; r++) {
                for (int c = 0; c < 9; c++) {
                    solutionGrid[r][c] = solution[r * 9 + c];
                }
            }

            printPuzzle(solutionGrid);
            printf("\nSolved in Iterations=%lld\n\n", cpIterations);
        } else {
            printf("\nNo solution found\n");
        }

    gettimeofday(&stop, NULL);
    printf("Seconds to process %.3f\n",
           (stop.tv_sec - start.tv_sec) +
           (stop.tv_usec - start.tv_usec) / 1000000.0);
    [pool drain];

    return 0;
}
