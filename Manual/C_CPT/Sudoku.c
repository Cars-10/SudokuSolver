#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>

#define SIZE 9
#define SUBGRID_SIZE 3

int solve_sudoku(int board[SIZE][SIZE]);
int is_valid_move(int board[SIZE][SIZE], int row, int col, int num);
void print_board(int board[SIZE][SIZE]);
void read_sudoku_from_file(char *file_path, int board[SIZE][SIZE]);

int solverCount = 0;

int main(int argc, char *argv[]) {
    struct timeval stop, start;
    gettimeofday(&start, NULL);

    if (argc < 2) {
        fprintf(stderr, "Usage: %s input_file1.txt input_file2.txt ...\n", argv[0]);
        return 1;
    }

    int board[SIZE][SIZE];

    for (int i = 1; i < argc; i++) {
        printf("\n%s:\n", argv[i]);
        read_sudoku_from_file(argv[i], board);
        print_board(board);

        if (solve_sudoku(board)) {
            printf("\nPuzzle:\n");
            print_board(board);
            printf("\nSolved in Iterations=%d\n", solverCount);
        } else {
            printf("No solution found for %s\n\n", argv[i]);
        }
        gettimeofday(&stop, NULL);
        printf("\nSeconds to process %.3f\n", (stop.tv_sec - start.tv_sec) + (stop.tv_usec - start.tv_usec)/1000000.0);
    }

    return 0;
}

int solve_sudoku(int board[SIZE][SIZE]) {
    solverCount++;

    for (int row = 0; row < SIZE; row++) {
        for (int col = 0; col < SIZE; col++) {
            if (board[row][col] == 0) {
                for (int num = 1; num <= SIZE; num++) {
                    if (is_valid_move(board, row, col, num)) {
                        board[row][col] = num;
                        if (solve_sudoku(board)) {
                            return 1;
                        }
                        board[row][col] = 0;  // Backtrack
                    }
                }
                return 0;
            }
        }
    }
    return 1;
}

int is_valid_move(int board[SIZE][SIZE], int row, int col, int num) {
    // Check row, column, and subgrid for conflicts.
    for (int i = 0; i < SIZE; i++) {
        if (board[row][i] == num || board[i][col] == num || board[row - row % SUBGRID_SIZE + i / SUBGRID_SIZE][col - col % SUBGRID_SIZE + i % SUBGRID_SIZE] == num) {
            return 0;
        }
    }
    return 1;
}

void print_board(int board[SIZE][SIZE]) {
    for (int i = 0; i < SIZE; i++) {
        for (int j = 0; j < SIZE; j++) {
            printf("%d ", board[i][j]);
        }
        printf("\n");
    }
}

void read_sudoku_from_file(char *file_path, int board[SIZE][SIZE]) {
    FILE *file = fopen(file_path, "r");
    if (file == NULL) {
        perror("Error opening file");
        exit(1);
    }

    for (int i = 0; i < SIZE; i++) {
        char line[256];  // Adjust the buffer size as needed.
        if (fgets(line, sizeof(line), file) == NULL) {
            perror("Error reading from file");
            exit(1);
        }

        // Ignore lines starting with #
        if (line[0] == '#') {
            i--;  // Decrement the counter to read another line for this row.
            continue;
        }

        for (int j = 0; j < SIZE; j++) {
            if (line[j * 2] != ' ') {
                board[i][j] = line[j * 2] - '0';
            } else {
                board[i][j] = 0;
            }
        }
    }

    fclose(file);
}
