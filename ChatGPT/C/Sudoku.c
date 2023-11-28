#include <stdio.h>
#include <stdlib.h>
#include <locale.h>

#define SIZE 9
#define UNASSIGNED 0

// Function prototypes
int isSafe(int *board, int row, int col, int num);

// Function to read the matrix from a file and return it as a 1D array
int* readMatrixFromFile(const char *filename) {
    static int board[SIZE*SIZE];
    FILE *file = fopen(filename, "r");
    if (file == NULL) {
        printf("Error opening file\n");
        exit(EXIT_FAILURE);
    }
    char line[100];
    int row = 0;
    while (fgets(line, sizeof(line), file) && row < SIZE) {
        if (line[0] == '#' || line[0] == '\n') continue; // Ignore comments and empty lines
        for (int i = 0, col = 0; i < SIZE*2 && col < SIZE; i += 2, ++col) {
            char ch = line[i];
            board[row*SIZE + col] = (ch >= '1' && ch <= '9') ? ch - '0' : UNASSIGNED;
        }
        ++row;
    }
    fclose(file);
    return board;
}

// Function to calculate the complexity of the matrix
int calculateComplexity(int *board) {
    int emptyCells = 0;
    for (int i = 0; i < SIZE*SIZE; ++i) {
        if (board[i] == UNASSIGNED) ++emptyCells;
    }
    return emptyCells;
}

// Function to format and print the number of iterations
void printIterationsFormatted(int iterations) {
    setlocale(LC_NUMERIC, "en_US.UTF-8");
    printf("Iterations: %'d\n", iterations);
}

// Function to print the board in a 9x9 grid
void printBoard(int *board, int displayComplexity) {
    if (displayComplexity) {
        int complexity = calculateComplexity(board);
        printf("Unsolved Board (Complexity: %d):\n", complexity);
    } else {
        printf("Solved Board:\n");
    }
    for (int row = 0; row < SIZE; ++row) {
        if (row % 3 == 0 && row != 0) {
            printf("------+-------+------\n");
        }
        for (int col = 0; col < SIZE; ++col) {
            if (col % 3 == 0 && col != 0) printf("| ");
            printf("%d ", board[row*SIZE + col] ? board[row*SIZE + col] : ' ');
        }
        printf("\n");
    }
    printf("\n");
}

// Function to solve the Sudoku board using a backtracking algorithm
int solveSudoku(int *board, int *iterations) {
    int row, col, found = 0;
    for (row = 0; row < SIZE; ++row) {
        for (col = 0; col < SIZE; ++col) {
            if (board[row*SIZE + col] == UNASSIGNED) {
                found = 1;
                break;
            }
        }
        if (found) break;
    }
    if (!found) return 1; // Solution found

    for (int num = 1; num <= SIZE; ++num) {
        if (isSafe(board, row, col, num)) {
            board[row*SIZE + col] = num;
            ++(*iterations);
            if (solveSudoku(board, iterations)) return 1;
            board[row*SIZE + col] = UNASSIGNED; // Backtrack
        }
    }
    return 0; // Trigger backtracking
}

// Function to check if it's safe to place a number in the given cell
int isSafe(int *board, int row, int col, int num) {
    for (int x = 0; x < SIZE; ++x) {
        if (board[row*SIZE + x] == num || board[x*SIZE + col] == num ||
            board[(row/3)*3*SIZE + (col/3)*3 + x/3*SIZE + x%3] == num) {
            return 0;
        }
    }
    return 1;
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        printf("Usage: %s filename\n", argv[0]);
        return EXIT_FAILURE;
    }
    int *board = readMatrixFromFile(argv[1]);
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
