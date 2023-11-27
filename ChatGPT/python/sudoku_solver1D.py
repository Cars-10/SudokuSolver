import sys
import time
import itertools

def read_puzzle(file_path):
    with open(file_path, 'r') as file:
        puzzle = [int(x) if x.isdigit() else 0 for line in file if not line.startswith('#') for x in line.split()]
    return puzzle

def is_valid(puzzle, index, num):
    row, col = divmod(index, 9)
    box_start_index = (row // 3) * 27 + (col // 3) * 3
    if num in puzzle[row*9:row*9+9] or num in puzzle[col::9] or num in puzzle[box_start_index:box_start_index+21:9] or num in puzzle[box_start_index+3:box_start_index+24:9] or num in puzzle[box_start_index+6:box_start_index+27:9]:
        return False
    return True

def find_next_cell(puzzle):
    min_options = 10
    next_cell = None
    options = {}
    for index in range(81):
        if puzzle[index] == 0:
            options[index] = [num for num in range(1, 10) if is_valid(puzzle, index, num)]
            if len(options[index]) < min_options:
                min_options = len(options[index])
                next_cell = index
    return next_cell, options

def solve(puzzle, iteration_counter):
    iteration_counter[0] += 1
    next_cell, options = find_next_cell(puzzle)
    if next_cell is None:
        return True
    for num in options[next_cell]:
        puzzle[next_cell] = num
        if solve(puzzle, iteration_counter):
            return True
        puzzle[next_cell] = 0
    return False

def print_puzzle(puzzle):
    for i in range(0, 81, 9):
        print(" ".join(str(num) for num in puzzle[i:i+9]))

def main():
    iteration_counter = [0]
    file_paths = sys.argv[1:]

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
        iteration_counter[0] = 0

if __name__ == "__main__":
    main()
