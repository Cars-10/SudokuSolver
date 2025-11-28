import sys
import time
start = time.time()

class SudokuSolver:
    def __init__(self):
        self.board = None
        self.size = 9
        self.subgrid_size = 3
        self.solve_count = 0

    def solve_sudoku(self, board):
        self.board = board
        if self._solve():
            return self.board
        return None

    def _solve(self):
        self.solve_count += 1
        for row in range(self.size):
            for col in range(self.size):
                if self.board[row][col] == 0:
                    for num in range(1, self.size + 1):
                        if self._is_valid_move(row, col, num):
                            self.board[row][col] = num
                            if self._solve():
                                return True
                            self.board[row][col] = 0  # Backtrack
                    return False
        return True

    def _is_valid_move(self, row, col, num):
        # Check row, column, and subgrid for conflicts.
        return (
            self._is_valid_row(row, num) and
            self._is_valid_col(col, num) and
            self._is_valid_subgrid(row, col, num)
        )

    def _is_valid_row(self, row, num):
        return num not in self.board[row]

    def _is_valid_col(self, col, num):
        return num not in [self.board[row][col] for row in range(self.size)]

    def _is_valid_subgrid(self, row, col, num):
        start_row, start_col = row - row % self.subgrid_size, col - col % self.subgrid_size
        for i in range(self.subgrid_size):
            for j in range(self.subgrid_size):
                if self.board[i + start_row][j + start_col] == num:
                    return False
        return True

    def print_board(self, board):
        for row in board:
            print(" ".join(map(str, row)))

def read_sudoku_from_file(file_path):
    with open(file_path, 'r') as file:
        sudoku_board = []
        for line in file:
            # Ignore lines starting with #
            if line.startswith("#"):
                continue

            row = [int(num) for num in line.split()]
            sudoku_board.append(row)
        return sudoku_board

if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("Usage: python sudoku_solver.py input_file1.txt input_file2.txt ...")
        sys.exit(1)

    solver = SudokuSolver()

    for file_path in sys.argv[1:]:
        print(f"\n{file_path}:\n")
        sudoku_board = read_sudoku_from_file(file_path)
        solver.print_board(sudoku_board)

        solution = solver.solve_sudoku(sudoku_board)

        if solution:
            print("\nPuzzles:")
            solver.print_board(solution)
            print(f"\nSolved in Iterations={solver.solve_count}\n")
            print(f"\nSeconds to process {(time.time()-start):.3f}")

        else:
            print(f"No solution found for {file_path}\n")
