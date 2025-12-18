#!/usr/bin/env python3
"""
Sudoku Solver - Python Implementation
Brute-force backtracking algorithm matching C reference exactly.

Algorithm:
- Row-major search for empty cells (top-to-bottom, left-to-right)
- Try values 1-9 in ascending order
- Count EVERY placement attempt (algorithm fingerprint)
"""
import sys
import time

# Global puzzle grid [row][col]
puzzle = [[0] * 9 for _ in range(9)]
count = 0  # Iteration counter


def print_puzzle():
    """Print puzzle in exact C reference format."""
    print("\nPuzzle:")
    for row in range(9):
        print(" ".join(str(puzzle[row][col]) for col in range(9)) + " ")


def read_matrix_file(filename):
    """Read matrix file matching C reference output format."""
    global puzzle

    # Normalize path for output (match C format)
    display_path = filename
    if filename.startswith("/app/Matrices/"):
        display_path = "../" + filename[5:]  # Skip "/app/" to get "Matrices/..."
    print(display_path)

    line_count = 0
    with open(filename, 'r') as f:
        for line in f:
            # Skip comments and empty lines
            line = line.strip()
            if not line or line.startswith('#'):
                continue

            # Parse 9 integers from line
            values = list(map(int, line.split()))
            if len(values) == 9 and line_count < 9:
                puzzle[line_count] = values
                print(" ".join(str(v) for v in values) + " ")
                line_count += 1


def is_valid(row, col, val):
    """Check if placing val at (row, col) is valid."""
    # Check row
    for i in range(9):
        if puzzle[row][i] == val:
            return False

    # Check column
    for i in range(9):
        if puzzle[i][col] == val:
            return False

    # Check 3x3 box
    box_row = (row // 3) * 3
    box_col = (col // 3) * 3
    for i in range(3):
        for j in range(3):
            if puzzle[box_row + i][box_col + j] == val:
                return False

    return True


def solve():
    """
    BRUTE-FORCE SOLVER
    Searches row-major order (top-to-bottom, left-to-right)
    Tries candidates 1-9 in ascending order
    Counts EVERY placement attempt (the algorithm fingerprint)
    """
    global count

    # Find first empty cell (row-major order)
    row, col = -1, -1
    for r in range(9):
        for c in range(9):
            if puzzle[r][c] == 0:
                row, col = r, c
                break
        if row != -1:
            break

    # If no empty cell found, puzzle is solved
    if row == -1:
        print_puzzle()
        print(f"\nSolved in Iterations={count}\n")
        return True

    # Try values 1-9 in order
    for val in range(1, 10):
        count += 1  # COUNT EVERY ATTEMPT - this is the algorithm fingerprint

        if is_valid(row, col, val):
            puzzle[row][col] = val  # Place value

            if solve():
                return True

            puzzle[row][col] = 0  # Backtrack

    return False


def main():
    """Main program - process each .matrix file from command line."""
    global puzzle, count

    start = time.time()

    # Process each .matrix file from command line
    for arg in sys.argv[1:]:
        if arg.endswith(".matrix"):
            # Reset puzzle
            puzzle = [[0] * 9 for _ in range(9)]

            read_matrix_file(arg)
            print_puzzle()
            count = 0
            solve()

    elapsed = time.time() - start
    print(f"Seconds to process {elapsed:.3f}")


if __name__ == "__main__":
    main()
