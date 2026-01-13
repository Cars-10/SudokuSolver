#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

// Sudoku puzzle grid [row][col]
int puzzle[9][9];
int count = 0;  // Iteration counter
int DEBUG = 0;  // 0 off, 1 on

void printPuzzle() {
  printf("\nPuzzle:\n");
  for (int j = 0; j < 9; j++) {
    for (int i = 0; i < 9; i++) {
      printf("%i ", puzzle[j][i]);
    }
    printf("\n");
  }
}

int readMatrixFile(char *filename) {
  FILE *file = NULL;
  char *line_buf = NULL;
  size_t line_buf_size = 0;
  int line_count = 0;
  ssize_t line_size;

  file = fopen(filename, "r");
  if (!file) {
    fprintf(stderr, "Error opening file '%s'\n", filename);
    return 1;
  }

  // Normalize path for output (convert absolute to relative)
  const char *display_path = filename;
  if (strncmp(filename, "/app/Matrices/", 14) == 0) {
    display_path = filename + 5;  // Skip "/app/" to get "Matrices/..."
    printf("../%s\n", display_path);
  } else {
    printf("%s\n", filename);
  }

  line_size = getline(&line_buf, &line_buf_size, file);
  while (line_size >= 0) {
    // Skip comments and empty lines
    if (line_buf[0] != '#' && line_buf[0] != '\n' && line_buf[0] != '\r') {
      if (DEBUG)
        printf("line[%06d]: chars=%06zd, contents: %s",
               line_count, line_size, line_buf);

      int parsed_count =
          sscanf(line_buf, "%i %i %i %i %i %i %i %i %i",
                 &puzzle[line_count][0], &puzzle[line_count][1],
                 &puzzle[line_count][2], &puzzle[line_count][3],
                 &puzzle[line_count][4], &puzzle[line_count][5],
                 &puzzle[line_count][6], &puzzle[line_count][7],
                 &puzzle[line_count][8]);

      if (parsed_count == 9) {
        if (line_count < 9) {
          for (int i = 0; i < 9; i++)
            printf("%i ", puzzle[line_count][i]);
          printf("\n");
          line_count++;
        }
      } else {
        printf("Error: line does not contain 9 integers\n");
        printf("line[%06d]: chars=%06zd, contents: %s",
               line_count, line_size, line_buf);
        return 1;
      }
    }
    line_size = getline(&line_buf, &line_buf_size, file);
  }
  fclose(file);
  free(line_buf);
  return 0;
}

// Check if placing val at (row, col) is valid
int isValid(int row, int col, int val) {
  // Check row
  for (int i = 0; i < 9; i++) {
    if (puzzle[row][i] == val)
      return 0;
  }

  // Check column
  for (int i = 0; i < 9; i++) {
    if (puzzle[i][col] == val)
      return 0;
  }

  // Check 3x3 box
  int box_row = (row / 3) * 3;
  int box_col = (col / 3) * 3;
  for (int i = 0; i < 3; i++) {
    for (int j = 0; j < 3; j++) {
      if (puzzle[box_row + i][box_col + j] == val)
        return 0;
    }
  }

  return 1;
}

// BRUTE-FORCE SOLVER
// Searches row-major order (top-to-bottom, left-to-right)
// Tries candidates 1-9 in ascending order
// Counts EVERY placement attempt (the algorithm fingerprint)
int solve() {
  // Find first empty cell (row-major order)
  int row = -1, col = -1;
  for (int r = 0; r < 9; r++) {
    for (int c = 0; c < 9; c++) {
      if (puzzle[r][c] == 0) {
        row = r;
        col = c;
        goto found_empty;
      }
    }
  }

found_empty:
  // If no empty cell found, puzzle is solved
  if (row == -1) {
    printPuzzle();
    printf("\nSolved in Iterations=%i\n\n", count);
    return 1;  // Success
  }

  // Try values 1-9 in order
  for (int val = 1; val <= 9; val++) {
    count++;  // COUNT EVERY ATTEMPT - this is the algorithm fingerprint

    if (isValid(row, col, val)) {
      puzzle[row][col] = val;  // Place value

      if (solve() == 1) {
        return 1;  // Solved
      }

      puzzle[row][col] = 0;  // Backtrack
    }
  }

  return 0;  // No solution found
}

int main(int argc, char **argv) {
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

      printPuzzle();
      count = 0;
      solve();
    }
  }

  gettimeofday(&stop, NULL);
  printf("Seconds to process %.3f\n",
         (stop.tv_sec - start.tv_sec) +
             (stop.tv_usec - start.tv_usec) / 1000000.0);

  return 0;
}
