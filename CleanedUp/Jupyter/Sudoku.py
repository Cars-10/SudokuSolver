import sys
import numpy as np
import time

sys.setrecursionlimit(5000)

puzzle = []
iterations = 0

def isPossible(y, x, val):
    global puzzle
    for i in range(0, 9):
        if puzzle[y][i] == val:
            return False
    for i in range(0, 9):
        if puzzle[i][x] == val:
            return False
    x0 = (x // 3) * 3
    y0 = (y // 3) * 3
    for i in range(0, 3):
        for j in range(0, 3):
            if puzzle[y0 + i][x0 + j] == val:
                return False
    return True

def solve():
    global puzzle, iterations
    for j in range(0, 9):
        for i in range(0, 9):
            if puzzle[j][i] == 0:
                for val in range(1, 10):
                    iterations += 1
                    if isPossible(j, i, val):
                        puzzle[j][i] = val
                        if solve():
                            return True
                        puzzle[j][i] = 0
                return False
    return True

def read_board(filename):
    global puzzle
    puzzle = []
    try:
        with open(filename, 'r') as f:
            for line in f:
                if line.strip() and not line.startswith('#'):
                    row = [int(c) for c in line.split()]
                    puzzle.append(row)
        puzzle = np.array(puzzle)
    except Exception as e:
        print(f"Error reading file {filename}: {e}")
        sys.exit(1)

def print_board():
    global puzzle
    print("Puzzle:")
    for row in puzzle:
        print(" ".join(map(str, row)))

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python3 Sudoku.py <file1> <file2> ...")
        sys.exit(1)

    start_total = time.time()

    for filename in sys.argv[1:]:
        print(f"\nProcessing {filename}")
        read_board(filename)
        print_board()
        
        iterations = 0
        if solve():
            print_board()
            print(f"\nSolved in Iterations={iterations}")
        else:
            print("No solution found")
            
    end_total = time.time()
    print(f"\nSeconds to process {end_total - start_total:.3f}")
