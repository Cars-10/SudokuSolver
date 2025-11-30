dnl M4 Sudoku Solver Generator
dnl This macro file generates a C program to solve Sudoku
dnl It demonstrates using M4 for code generation (metaprogramming)

define(`SIZE', `9')
define(`UNASSIGNED', `0')

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Global board and counter
int grid[SIZE][SIZE];
long long iterations = 0;

// Function prototypes
void print_grid();
int solve();
int is_safe(int row, int col, int num);
int find_empty_location(int *row, int *col);

int main(int argc, char *argv[]) {
    if (argc < 2) {
        printf("Usage: %s <matrix_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "r");
    if (!file) {
        printf("Error opening file\n");
        return 1;
    }

    char line[256];
    int row = 0;
    while (fgets(line, sizeof(line), file) && row < SIZE) {
        if (line[0] == '#') continue; // Skip comments
        
        // Parse line
        char *token = strtok(line, " \t\n");
        int col = 0;
        while (token && col < SIZE) {
            grid[row][col] = atoi(token);
            token = strtok(NULL, " \t\n");
            col++;
        }
        if (col == SIZE) row++;
    }
    fclose(file);

    printf("Puzzle:\n");
    print_grid();

    if (solve()) {
        printf("Puzzle:\n");
        print_grid();
        printf("Solved in Iterations=%lld\n", iterations);
    } else {
        printf("No solution exists\n");
    }

    return 0;
}

void print_grid() {
    for (int row = 0; row < SIZE; row++) {
        for (int col = 0; col < SIZE; col++) {
            printf("%d ", grid[row][col]);
        }
        printf("\n");
    }
    printf("\n");
}

int is_safe(int row, int col, int num) {
    // Check row
    for (int x = 0; x < SIZE; x++)
        if (grid[row][x] == num) return 0;

    // Check col
    for (int x = 0; x < SIZE; x++)
        if (grid[x][col] == num) return 0;

    // Check box
    int startRow = row - row % 3;
    int startCol = col - col % 3;
    for (int i = 0; i < 3; i++)
        for (int j = 0; j < 3; j++)
            if (grid[i + startRow][j + startCol] == num) return 0;

    return 1;
}

int solve() {
    int row, col;

    if (!find_empty_location(&row, &col))
        return 1; // Success

    for (int num = 1; num <= SIZE; num++) {
        iterations++; // Count every attempt
        
        if (is_safe(row, col, num)) {
            grid[row][col] = num;

            if (solve())
                return 1;

            grid[row][col] = UNASSIGNED;
        }
    }
    return 0;
}

int find_empty_location(int *row, int *col) {
    for (*row = 0; *row < SIZE; (*row)++)
        for (*col = 0; *col < SIZE; (*col)++)
            if (grid[*row][*col] == UNASSIGNED)
                return 1;
    return 0;
}
