#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include "cp.h"

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
    const char *display_path = filename;
    if (strncmp(filename, "/app/Matrices/", 14) == 0) {
        display_path = filename + 5;  // Skip "/app/" to get "Matrices/..."
        printf("../%s\n", display_path);
    } else {
        printf("%s\n", filename);
    }

    char *line_buf = NULL;
    size_t line_buf_size = 0;
    ssize_t line_size;
    int line_count = 0;

    line_size = getline(&line_buf, &line_buf_size, file);
    while (line_size >= 0 && line_count < 9) {
        // Skip comments and empty lines
        if (line_buf[0] != '#' && line_buf[0] != '\n' && line_buf[0] != '\r') {
            int parsed_count = sscanf(line_buf, "%i %i %i %i %i %i %i %i %i",
                                      &puzzle[line_count][0], &puzzle[line_count][1],
                                      &puzzle[line_count][2], &puzzle[line_count][3],
                                      &puzzle[line_count][4], &puzzle[line_count][5],
                                      &puzzle[line_count][6], &puzzle[line_count][7],
                                      &puzzle[line_count][8]);

            if (parsed_count == 9) {
                for (int i = 0; i < 9; i++) {
                    printf("%i ", puzzle[line_count][i]);
                }
                printf("\n");
                line_count++;
            } else {
                fprintf(stderr, "Error: line does not contain 9 integers\n");
                fclose(file);
                free(line_buf);
                return 0;
            }
        }
        line_size = getline(&line_buf, &line_buf_size, file);
    }

    fclose(file);
    free(line_buf);
    return (line_count == 9) ? 1 : 0;
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
    struct timeval stop, start;
    gettimeofday(&start, NULL);

    if (argc != 2) {
        fprintf(stderr, "Usage: %s <matrix_file>\n", argv[0]);
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
    CPGrid grid;
    init_grid(&grid, puzzle);

    // Apply initial propagation
    if (!propagate(&grid)) {
        printf("\nNo solution found (contradiction during initial propagation)\n");
        gettimeofday(&stop, NULL);
        printf("Seconds to process %.3f\n",
               (stop.tv_sec - start.tv_sec) +
               (stop.tv_usec - start.tv_usec) / 1000000.0);
        return 0;
    }

    // Run search
    int solution[81];
    int solved = cp_search(&grid, solution);

    if (solved) {
        // Convert solution array back to 2D for printing
        int solution_grid[9][9];
        for (int r = 0; r < 9; r++) {
            for (int c = 0; c < 9; c++) {
                solution_grid[r][c] = solution[r * 9 + c];
            }
        }

        printPuzzle(solution_grid);
        printf("\nSolved in Iterations=%lld\n\n", cp_iterations);
    } else {
        printf("\nNo solution found\n");
    }

    gettimeofday(&stop, NULL);
    printf("Seconds to process %.3f\n",
           (stop.tv_sec - start.tv_sec) +
           (stop.tv_usec - start.tv_usec) / 1000000.0);

    return 0;
}
