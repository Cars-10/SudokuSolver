#include <iostream>
#include <sstream>
#include <fstream>
#include <vector>
#include <chrono>
#include <cstring>

using namespace std;
using namespace std::chrono;

const int SIZE = 9;
int iteration_count;

bool isSafe(int grid[SIZE][SIZE], int row, int col, int num) {
    for (int x = 0; x <= 8; x++)
        if (grid[row][x] == num)
            return false;

    for (int x = 0; x <= 8; x++)
        if (grid[x][col] == num)
            return false;

    int startRow = row - row % 3, startCol = col - col % 3;
    for (int i = 0; i < 3; i++)
        for (int j = 0; j < 3; j++)
            if (grid[i + startRow][j + startCol] == num)
                return false;

    return true;
}

bool solveSudoku(int grid[SIZE][SIZE], int row, int col) {
    iteration_count++;  // Increment the iteration count
    if (row == SIZE - 1 && col == SIZE)
        return true;
    if (col == SIZE) {
        row++;
        col = 0;
    }
    if (grid[row][col] > 0)
        return solveSudoku(grid, row, col + 1);
    for (int num = 1; num <= SIZE; num++) {
        if (isSafe(grid, row, col, num)) {
            grid[row][col] = num;
            if (solveSudoku(grid, row, col + 1))
                return true;
        }
        grid[row][col] = 0; // reset on backtrack
    }
    return false;
}

void printGrid(int grid[SIZE][SIZE]) {
    for (int row = 0; row < SIZE; row++) {
        for (int col = 0; col < SIZE; col++) {
            cout << grid[row][col] << " ";
        }
        cout << endl;
    }
}

bool loadSudokuGrid(const char* path, int grid[SIZE][SIZE]) {
    ifstream file(path);
    if (!file.is_open()) {
        cerr << "Could not open file " << path << endl;
        return false;
    }
    string line;
    int row = 0;
    while (getline(file, line)) {
        if (line.empty() || line[0] == '#') continue; // Skip comments and empty lines
        istringstream iss(line);
        for (int col = 0; col < SIZE; ++col) {
            iss >> grid[row][col];
        }
        ++row;
    }
    file.close();
    return row == SIZE;
}

int main(int argc, char* argv[]) {
    if (argc < 2) {
        cout << "Usage: " << argv[0] << " <file1> [<file2> ...]" << endl;
        return 1;
    }

    auto start = high_resolution_clock::now();
    for (int i = 1; i < argc; i++) {
        int grid[SIZE][SIZE] = {0};
        if (!loadSudokuGrid(argv[i], grid)) {
            cerr << "Error reading Sudoku grid from file: " << argv[i] << endl;
            continue;
        }

        iteration_count = 0;
        if (solveSudoku(grid, 0, 0)) {
            cout << "Sudoku Grid Solved with " << iteration_count << " iterations:" << endl;
            printGrid(grid);
        } else {
            cout << "No solution exists for the provided Sudoku grid in file: " << argv[i] << endl;
        }
        cout << endl;
    }
    auto stop = high_resolution_clock::now();
    auto duration = duration_cast<microseconds>(stop - start);
    cout << "Execution time: " << fixed << setprecision(2) << duration.count() / 1000000.0 << " seconds" << endl;

    return 0;
}
