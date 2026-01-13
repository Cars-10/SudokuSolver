#!/usr/bin/env python3
"""
Dancing Links (DLX) Sudoku Solver - Python Implementation
Mechanical translation from C reference to preserve algorithm correctness.

Algorithm: Knuth's Algorithm X with Dancing Links data structure
- Exact cover problem: 324 constraints (row-col, row-num, col-num, box-num)
- Each Sudoku placement satisfies exactly 4 constraints
- Doubly-linked circular lists for efficient cover/uncover operations
"""
import sys
import time

# Global iteration counter (analogous to brute-force count)
dlx_iterations = 0


class DlxNode:
    """Node in the DLX matrix - part of circular doubly-linked lists."""
    def __init__(self):
        self.left = None      # Left neighbor in row
        self.right = None     # Right neighbor in row
        self.up = None        # Up neighbor in column
        self.down = None      # Down neighbor in column
        self.column = None    # Pointer to column header
        self.row_id = -1      # ID of the row this node belongs to


class DlxColumn:
    """Column header in DLX matrix - contains a node plus metadata."""
    def __init__(self, name=""):
        self.node = DlxNode()       # Embedded node (inheritance via composition)
        self.size = 0               # Number of nodes in this column
        self.name = name            # For debugging/identification
        self.node.column = self     # Column nodes point to themselves


class RowInfo:
    """Metadata to map DLX rows back to Sudoku (row, col, num)."""
    def __init__(self, row=0, col=0, num=0):
        self.row = row
        self.col = col
        self.num = num


# DLX matrix structures
root = None
columns = []           # 324 constraint columns
nodes = []             # Node pool
row_info = []          # 729 possible rows (9x9x9)
row_starts = []        # Pointer to first node in each row

# Sudoku puzzle grid [row][col]
puzzle = [[0] * 9 for _ in range(9)]
solution_grid = [[0] * 9 for _ in range(9)]


def get_position_col(r, c):
    """Position constraint: cell (r,c) must be filled."""
    return r * 9 + c


def get_row_col(r, n):
    """Row constraint: row r must have number n."""
    return 81 + r * 9 + (n - 1)


def get_col_col(c, n):
    """Column constraint: column c must have number n."""
    return 162 + c * 9 + (n - 1)


def get_box_col(r, c, n):
    """Box constraint: box b must have number n."""
    box = (r // 3) * 3 + (c // 3)
    return 243 + box * 9 + (n - 1)


def dlx_cover_column(col):
    """
    Cover a column in the DLX matrix.
    Remove column from header list and remove all rows in the column's list
    from other column lists.
    """
    col_node = col.node

    # Remove column header from the header list
    col_node.right.left = col_node.left
    col_node.left.right = col_node.right

    # For each row in this column
    row_node = col_node.down
    while row_node != col_node:
        # For each node in this row (excluding the column itself)
        right_node = row_node.right
        while right_node != row_node:
            # Remove this node from its column
            right_node.down.up = right_node.up
            right_node.up.down = right_node.down
            right_node.column.size -= 1
            right_node = right_node.right
        row_node = row_node.down


def dlx_uncover_column(col):
    """Uncover a column (exact reverse of cover)."""
    col_node = col.node

    # For each row in this column (in reverse order)
    row_node = col_node.up
    while row_node != col_node:
        # For each node in this row (in reverse order)
        left_node = row_node.left
        while left_node != row_node:
            # Restore this node to its column
            left_node.column.size += 1
            left_node.down.up = left_node
            left_node.up.down = left_node
            left_node = left_node.left
        row_node = row_node.up

    # Restore column header to the header list
    col_node.right.left = col_node
    col_node.left.right = col_node


def choose_column(root_col):
    """Choose column with minimum size (Knuth's S heuristic)."""
    root_node = root_col.node
    best = None
    min_size = float('inf')

    col_node = root_node.right
    while col_node != root_node:
        col = col_node.column
        if col.size < min_size:
            min_size = col.size
            best = col
        col_node = col_node.right

    return best


def dlx_search(root_col, k, solution):
    """
    DLX Search - Algorithm X with Dancing Links.
    Returns True if solution found, False otherwise.
    solution[] stores the row indices of the solution.
    """
    global dlx_iterations
    dlx_iterations += 1  # Count every search call (analogous to brute-force iterations)

    root_node = root_col.node

    # If matrix is empty, we found a solution
    if root_node.right == root_node:
        return True

    # Choose column with minimum size
    col = choose_column(root_col)

    # If column has no rows, no solution possible
    if col.size == 0:
        return False

    # Cover this column
    dlx_cover_column(col)

    # Try each row in this column
    row_node = col.node.down
    while row_node != col.node:
        # Add row to partial solution
        solution[k] = row_node.row_id

        # Cover all other columns in this row
        right_node = row_node.right
        while right_node != row_node:
            dlx_cover_column(right_node.column)
            right_node = right_node.right

        # Recurse
        if dlx_search(root_col, k + 1, solution):
            return True  # Solution found

        # Backtrack: uncover all columns in this row
        left_node = row_node.left
        while left_node != row_node:
            dlx_uncover_column(left_node.column)
            left_node = left_node.left

        row_node = row_node.down

    # Uncover column
    dlx_uncover_column(col)

    return False  # No solution found


def init_dlx_matrix():
    """Initialize DLX matrix structure."""
    global root, columns, nodes, row_info, row_starts

    # Allocate root column
    root = DlxColumn("root")
    root.node.left = root.node
    root.node.right = root.node
    root.node.up = root.node
    root.node.down = root.node
    root.node.row_id = -1

    # Allocate 324 column headers
    columns = []
    for i in range(324):
        col = DlxColumn(f"C{i}")

        # Initialize as circular list
        col.node.up = col.node
        col.node.down = col.node
        col.node.row_id = -1

        # Link into header list
        col.node.left = root.node.left
        col.node.right = root.node
        root.node.left.right = col.node
        root.node.left = col.node

        columns.append(col)

    # Initialize node pool, row info, and row starts
    nodes = []
    row_info = [RowInfo() for _ in range(729)]
    row_starts = [None for _ in range(729)]


def add_node(col, row_id):
    """Add a node to the DLX matrix."""
    node = DlxNode()
    node.column = col
    node.row_id = row_id

    # Insert at end of column's circular list
    node.down = col.node
    node.up = col.node.up
    col.node.up.down = node
    col.node.up = node
    col.size += 1

    nodes.append(node)
    return node


def build_dlx_row(r, c, n, row_id):
    """Build a DLX row for Sudoku cell (r,c) with value n."""
    # Store row metadata
    row_info[row_id].row = r
    row_info[row_id].col = c
    row_info[row_id].num = n

    # Create nodes for the 4 constraints
    n1 = add_node(columns[get_position_col(r, c)], row_id)
    n2 = add_node(columns[get_row_col(r, n)], row_id)
    n3 = add_node(columns[get_col_col(c, n)], row_id)
    n4 = add_node(columns[get_box_col(r, c, n)], row_id)

    # Link nodes horizontally in circular list
    n1.right = n2
    n2.right = n3
    n3.right = n4
    n4.right = n1

    n1.left = n4
    n2.left = n1
    n3.left = n2
    n4.left = n3

    # Store first node for this row
    row_starts[row_id] = n1


def build_dlx_matrix_from_puzzle():
    """Build the complete DLX matrix from the puzzle."""
    row_id = 0

    for r in range(9):
        for c in range(9):
            if puzzle[r][c] != 0:
                # Cell has a clue - create only one row for that value
                build_dlx_row(r, c, puzzle[r][c], row_id)
                row_id += 1
            else:
                # Cell is empty - create rows for all possible values
                for n in range(1, 10):
                    build_dlx_row(r, c, n, row_id)
                    row_id += 1


def cover_clues():
    """Cover given clues (pre-selected rows)."""
    for r in range(9):
        for c in range(9):
            if puzzle[r][c] != 0:
                n = puzzle[r][c]

                # Find the row for this clue
                for row_id in range(729):
                    if (row_starts[row_id] and
                        row_info[row_id].row == r and
                        row_info[row_id].col == c and
                        row_info[row_id].num == n):

                        # Cover all columns in this row
                        node = row_starts[row_id]
                        curr = node
                        while True:
                            dlx_cover_column(curr.column)
                            curr = curr.right
                            if curr == node:
                                break
                        break


def extract_solution(solution, solution_len):
    """Extract solution from DLX and populate solution_grid."""
    global solution_grid

    # Initialize solution grid - start with the original puzzle (includes clues)
    solution_grid = [row[:] for row in puzzle]

    # Each solution entry is a row_id
    for i in range(solution_len):
        row_id = solution[i]
        if 0 <= row_id < 729:
            solution_grid[row_info[row_id].row][row_info[row_id].col] = row_info[row_id].num


def print_puzzle(grid):
    """Print puzzle."""
    print("\nPuzzle:")
    for r in range(9):
        print(" ".join(str(grid[r][c]) for c in range(9)) + " ")


def read_matrix_file(filename):
    """Read matrix file."""
    global puzzle

    # Normalize path for output (convert absolute to relative)
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

    if line_count != 9:
        raise ValueError(f"Expected 9 lines, got {line_count}")


def main():
    """Main program - process each .matrix file from command line."""
    global dlx_iterations, puzzle

    start = time.time()

    # Process each .matrix file from command line
    for arg in sys.argv[1:]:
        if arg.endswith(".matrix"):
            # Reset puzzle
            puzzle = [[0] * 9 for _ in range(9)]

            try:
                read_matrix_file(arg)
                print_puzzle(puzzle)

                # Initialize DLX matrix
                init_dlx_matrix()

                # Build matrix from puzzle
                build_dlx_matrix_from_puzzle()

                # Cover pre-filled clues
                cover_clues()

                # Solve using DLX
                dlx_iterations = 0
                solution = [-1] * 81
                result = dlx_search(root, 0, solution)

                if result:
                    extract_solution(solution, 81)
                    print_puzzle(solution_grid)
                    print(f"\nSolved in Iterations={dlx_iterations}\n")
                else:
                    print("\nNo solution found\n")

            except Exception as e:
                print(f"Error processing {arg}: {e}", file=sys.stderr)
                continue

    elapsed = time.time() - start
    print(f"Seconds to process {elapsed:.3f}")


if __name__ == "__main__":
    main()
