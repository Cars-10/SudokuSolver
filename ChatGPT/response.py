import sys

# Step 1: Function to read a 9x9 matrix from a file and return it as a 1D array
def read_sudoku(file_path):
    board = []
    with open(file_path, 'r') as file:
        for line in file:
            if not line.startswith('#') and line.strip():
                board.extend([int(num) for num in line.strip().split() if num.isdigit()])
    return board

# Step 2: Function to calculate the complexity of a 9x9 matrix
def calculate_complexity(board):
    empty_cells = board.count(0)
    return empty_cells

# Step 3: Function to print the board in a 9x9 grid
def print_board(board, display_complexity=False):
    complexity = calculate_complexity(board) if display_complexity else None
    for i in range(9):
        for j in range(9):
            print(board[i * 9 + j], end=' ')
            if j == 2 or j == 5:
                print('|', end=' ')
        print()
        if i == 2 or i == 5:
            print('-'*21)
    if display_complexity:
        print('Complexity:', complexity)

# Step 4: Function to solve the Sudoku board using a backtracking algorithm
def solve_sudoku(board):
    def is_valid(num, row, col):
        # Check row
        row_start = (row // 3) * 3
        col_start = (col // 3) * 3
        if any(board[row * 9 + i] == num for i in range(9)): return False
        # Check column
        if any(board[i * 9 + col] == num for i in range(9)): return False
        # Check 3x3 square
        for r in range(row_start, row_start + 3):
            for c in range(col_start, col_start + 3):
                if board[r * 9 + c] == num: return False
        return True

    def solve():
        for i in range(81):
            row, col = divmod(i, 9)
            if board[i] == 0:
                for num in range(1, 10):
                    if is_valid(num, row, col):
                        board[i] = num
                        if solve():
                            return True
                        else:
                            board[i] = 0
                return False
        return True

    solve()
    return board

# Step 5: Main function to tie it all together
def main():
    if len(sys.argv) != 2:
        print('Usage: python sudoku_solver.py <sudoku_file>')
        return

    file_path = sys.argv[1]
    board = read_sudoku(file_path)
    if len(board) != 81:
        print('Invalid sudoku board! The board must be a 9x9 grid.')
        return

    print('Sudoku Board:')
    print_board(board)
    print('\nSolving...')

    iterations = 0
    def solve_and_count_iterations():
        nonlocal iterations

        def is_valid(num, row, col):
            row_start = (row // 3) * 3
            col_start = (col // 3) * 3
            if any(board[row * 9 + i] == num for i in range(9)): return False
            if any(board[i * 9 + col] == num for i in range(9)): return False
            for r in range(row_start, row_start + 3):
                for c in range(col_start, col_start + 3):
                    if board[r * 9 + c] == num: return False
            return True

        def solve():
            nonlocal iterations
            for i in range(81):
                row, col = divmod(i, 9)
                if board[i] == 0:
                    for num in range(1, 10):
                        if is_valid(num, row, col):
                            board[i] = num
                            iterations += 1
                            if solve():
                                return True
                            else:
                                board[i] = 0
                    return False
            return True

        return solve()

    solved = solve_and_count_iterations()
    if solved:
        print('Solved Sudoku Board:')
        print_board(board)
        print('Iterations:', iterations)
    else:
        print('No solution exists!')

if __name__ == '__main__':
    main()

