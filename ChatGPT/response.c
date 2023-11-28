#include <stdio.h>
#include <stdlib.h>
#include <locale.h>

#define N 9

// Function prototype for checking if it's safe to place a number
int isSafe(int board[N][N], int row, int col, int num);

// Function to read a 9x9 Sudoku board from a file and return it as a 2D array
void readBoardFromFile(char *filename, int board[N][N]) {
    FILE *file = fopen(filename, "r");
    if (file == NULL) {
        perror("Error opening file");
        exit(EXIT_FAILURE);
    }

    char line[100];
    int row = 0;
    while (fgets(line, sizeof(line), file) && row < N) {
        if (line[0] == '#' || line[0] == '\n') continue; // Ignore comments and empty lines

        int col = 0;
        for (char *p = line; *p && col < N; p++) {
            if (*p >= '0' && *p <= '9') {
                board[row][col++] = *p - '0';
            }
        }

        row++;
    }

    fclose(file);
}

// Function to calculate the complexity of a 9x9 Sudoku matrix
int calculateComplexity(int board[N][N]) {
    int emptyCells = 0;
    for (int i = 0; i < N; i++) {
        for (int j = 0; j < N; j++) {
            if (board[i][j] == 0) {
                emptyCells++;
            }
        }
    }
    return emptyCells;
}

// Function to format and print the number of iterations
void printIterationsFormatted(int iterations) {
    setlocale(LC_NUMERIC, "");
    printf("Iterations: %'d\n", iterations);
}

// Function to print the Sudoku board
void printBoard(int board[N][N], int displayComplexity) {
    if (displayComplexity) {
        int complexity = calculateComplexity(board);
        printf("Unsolved Board (Complexity: %d)\n", complexity);
    } else {
        printf("Solved Board\n");
    }

    for (int i = 0; i < N; i++) {
        if (i % 3 == 0 && i != 0) {
            printf("---------------------\n");
        }

        for (int j = 0; j < N; j++) {
            if (j % 3 == 0 && j != 0) {
                printf("| ");
            }

            printf("%d ", board[i][j]);
        }
        printf("\n");
    }
    printf("\n");
}

// Function that solves the Sudoku board using backtracking
int solveSudoku(int board[N][N], int *iterations) {
    int row, col, empty = 1;
    for (row = 0; row < N; row++) {
        for (col = 0; col < N; col++) {
            if (board[row][col] == 0) {
                empty = 0;
                break;
            }
        }
        if (!empty) break;
    }

    if (empty) return 1; // If no empty cell, puzzle solved

    for (int num = 1; num <= N; num++) {
        if (isSafe(board, row, col, num)) {
            board[row][col] = num;
            (*iterations)++;

            if (solveSudoku(board, iterations)) return 1;

            board[row][col] = 0; // Backtrack
        }
    }

    return 0;
}

// Function to check if it's safe to place a number at the given row, col
int isSafe(int board[N][N], int row, int col, int num) {
    // Check the row
    for (int x = 0; x < N; x++) {
        if (board[row][x] == num) return 0;
    }

    // Check the column
    for (int x = 0; x < N; x++) {
        if (board[x][col] == num) return 0;
    }

    // Check the 3x3 box
    int startRow = row - row % 3, startCol = col - col % 3;
    for (int i = 0; i < 3; i++) {
        for (int j = 0; j < 3; j++) {
            if (board[i + startRow][j + startCol] == num) return 0;
        }
    }

    return 1;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        printf("Usage: %s <sudoku_puzzle_file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    int board[N][N];
    readBoardFromFile(argv[1], board);
    printBoard(board, 1);

    int iterations = 0;
    if (solveSudoku(board, &iterations)) {
        printBoard(board, 0);
        printIterationsFormatted(iterations);
    } else {
        printf("No solution exists\n");
    }

    return EXIT_SUCCESS;
}
