import sys
import time
import itertools
import numpy as np

def read_puzzle(file_path):
    """
    Reads a Sudoku puzzle from a file. Ignores lines starting with '#'.
    Expects a 9x9 space-separated file, where each cell is either a number (1-9) or a placeholder (e.g., '.') for an empty cell.
    Returns the puzzle as a 2D list.
    """
    with open(file_path, 'r') as file:
        puzzle = [list(map(lambda x: int(x) if x.isdigit() else 0, line.split())) for line in file if not line.startswith('#')]
    return np.array(puzzle)

def is_valid(puzzle, row, col, num):
    """
    Checks if a number can be placed in a given position without breaking Sudoku rules.
    Optimized by using numpy's any() function to check if the number exists in the row, column, or box.
    """
    box_start_row, box_start_col = row - row % 3, col - col % 3
    if np.any(puzzle[row,:] == num) or np.any(puzzle[:,col] == num) or np.any(puzzle[box_start_row:box_start_row+3, box_start_col:box_start_col+3] == num):
        return False
    return True

def find_next_cell(puzzle):
    min_options = 10
    next_cell = None
    options = {}
    for i, j in itertools.product(range(9), repeat=2):
        if puzzle[i][j] == 0:
            options[(i, j)] = [num for num in range(1, 10) if is_valid(puzzle, i, j, num)]
            if len(options[(i, j)]) < min_options:
                min_options = len(options[(i, j)])
                next_cell = (i, j)
    return next_cell, options

def solve(puzzle, iteration_counter):
    """
    Solves the Sudoku puzzle using backtracking.
    """
    iteration_counter[0] += 1
    next_cell, options = find_next_cell(puzzle)
    if next_cell is None:
        return True
    i, j = next_cell
    for num in options[(i, j)]:
        puzzle[i][j] = num
        if solve(puzzle, iteration_counter):
            return True
        puzzle[i][j] = 0
    return False

def print_puzzle(puzzle):
    """
    Prints the Sudoku puzzle in a readable format.
    """
    for row in puzzle:
        print(" ".join(str(num) for num in row))

def main():
    iteration_counter = [0]  # Using a list to pass by reference
    file_paths = sys.argv[1:]  # Get file paths from command line arguments

    for file_path in file_paths:
        puzzle = read_puzzle(file_path)
        print_puzzle(puzzle)
        start_time = time.time()
        if solve(puzzle, iteration_counter):
            print(f"Solved puzzle from {file_path}:")
            print_puzzle(puzzle)
        else:
            print(f"Could not solve puzzle from {file_path}")
        end_time = time.time()
        print(f"Time taken: {int((end_time - start_time) * 1000)} milliseconds")
        print(f"Iterations: {iteration_counter[0]}")
        iteration_counter[0] = 0  # Reset counter for the next puzzle

if __name__ == "__main__":
    main()
