#!/usr/bin/env python3
"""
Constraint Propagation (CP) Sudoku Solver - Python Implementation
Mechanical translation from C reference to preserve algorithm correctness.

Algorithm: Constraint propagation with MRV (Minimum Remaining Values) heuristic
- Uses bitsets to track candidate values (bits 1-9)
- Propagates constraints: singleton elimination, hidden singles
- Search with MRV cell selection for efficiency
"""
import sys
import time

# Global iteration counter
cp_iterations = 0


class CPGrid:
    """Grid structure with assigned values and candidate tracking."""
    def __init__(self):
        self.values = [[0] * 9 for _ in range(9)]              # Assigned values (0 = empty)
        self.candidates = [[0] * 9 for _ in range(9)]          # Possible values per cell (bitset)


def has_candidate(candidate_set, digit):
    """Check if digit is a candidate in the bitset."""
    return (candidate_set & (1 << digit)) != 0


def add_candidate(candidate_set, digit):
    """Add digit to the candidate bitset."""
    return candidate_set | (1 << digit)


def remove_candidate(candidate_set, digit):
    """Remove digit from the candidate bitset."""
    return candidate_set & ~(1 << digit)


def count_candidates(candidate_set):
    """Count number of candidates in a bitset."""
    return bin(candidate_set).count('1')


def get_first_candidate(candidate_set):
    """Get first candidate digit from bitset (1-9)."""
    for digit in range(1, 10):
        if has_candidate(candidate_set, digit):
            return digit
    return 0


def get_peers(row, col):
    """Get all 20 peers for a cell (row, col, box) - returns list of (r,c) tuples."""
    peers = []

    # Same row (9 cells minus self = 8)
    for c in range(9):
        if c != col:
            peers.append((row, c))

    # Same column (9 cells minus self = 8)
    for r in range(9):
        if r != row:
            peers.append((r, col))

    # Same 3x3 box (9 cells minus self minus already counted = 4)
    box_row = (row // 3) * 3
    box_col = (col // 3) * 3
    for r in range(box_row, box_row + 3):
        for c in range(box_col, box_col + 3):
            if r != row and c != col:
                peers.append((r, c))

    return peers


def init_grid(grid, puzzle):
    """Initialize grid from puzzle."""
    for row in range(9):
        for col in range(9):
            if puzzle[row][col] == 0:
                # Empty cell: set all candidates 1-9 (bits 1-9 set)
                grid.values[row][col] = 0
                grid.candidates[row][col] = 0x3FE  # Binary: 0011 1111 1110 (bits 1-9)
            else:
                # Given clue: set single value
                digit = puzzle[row][col]
                grid.values[row][col] = digit
                grid.candidates[row][col] = (1 << digit)


def eliminate(grid, row, col, digit):
    """
    Eliminate digit from candidates at (row, col).
    Returns False if contradiction detected, True otherwise.
    """
    # Check if digit is already eliminated
    if not has_candidate(grid.candidates[row][col], digit):
        return True  # Already eliminated, no change

    # Remove digit from candidates
    grid.candidates[row][col] = remove_candidate(grid.candidates[row][col], digit)

    # Check for contradiction (no candidates left)
    remaining = count_candidates(grid.candidates[row][col])
    if remaining == 0:
        return False  # Contradiction

    # If only one candidate left, assign it (singleton elimination)
    if remaining == 1 and grid.values[row][col] == 0:
        last_digit = get_first_candidate(grid.candidates[row][col])
        if not assign(grid, row, col, last_digit):
            return False  # Assignment caused contradiction

    return True


def assign(grid, row, col, digit):
    """
    Assign digit to cell at (row, col).
    Returns False if contradiction detected, True otherwise.
    """
    global cp_iterations
    # Increment iteration counter (this is our benchmark metric)
    cp_iterations += 1

    # Set value
    grid.values[row][col] = digit
    grid.candidates[row][col] = (1 << digit)

    # Eliminate digit from all peers
    peers = get_peers(row, col)
    for peer_row, peer_col in peers:
        if not eliminate(grid, peer_row, peer_col, digit):
            return False  # Contradiction in peer elimination

    return True


def propagate(grid):
    """
    Apply constraint propagation until quiescence.
    Returns False if contradiction detected, True otherwise.
    """
    changed = True

    while changed:
        changed = False

        # Strategy 1: Singleton elimination
        # If a cell has only one candidate, assign it
        for row in range(9):
            for col in range(9):
                if grid.values[row][col] == 0:
                    num_candidates = count_candidates(grid.candidates[row][col])
                    if num_candidates == 0:
                        return False  # Contradiction
                    if num_candidates == 1:
                        digit = get_first_candidate(grid.candidates[row][col])
                        if not assign(grid, row, col, digit):
                            return False  # Assignment caused contradiction
                        changed = True

        # Strategy 2: Hidden singles
        # For each unit (row, col, box), if a digit appears in only one cell, assign it

        # Check rows
        for row in range(9):
            for digit in range(1, 10):
                count = 0
                last_col = -1
                # Check if digit is already assigned in this row
                already_assigned = False
                for col in range(9):
                    if grid.values[row][col] == digit:
                        already_assigned = True
                        break
                    if has_candidate(grid.candidates[row][col], digit):
                        count += 1
                        last_col = col

                if already_assigned:
                    continue

                if count == 1:
                    if not assign(grid, row, last_col, digit):
                        return False
                    changed = True
                elif count == 0:
                    return False  # Digit cannot be placed anywhere in row

        # Check columns
        for col in range(9):
            for digit in range(1, 10):
                count = 0
                last_row = -1
                # Check if digit is already assigned in this column
                already_assigned = False
                for row in range(9):
                    if grid.values[row][col] == digit:
                        already_assigned = True
                        break
                    if has_candidate(grid.candidates[row][col], digit):
                        count += 1
                        last_row = row

                if already_assigned:
                    continue

                if count == 1:
                    if not assign(grid, last_row, col, digit):
                        return False
                    changed = True
                elif count == 0:
                    return False  # Digit cannot be placed anywhere in column

        # Check boxes
        for box in range(9):
            box_row = (box // 3) * 3
            box_col = (box % 3) * 3

            for digit in range(1, 10):
                count = 0
                last_r = -1
                last_c = -1

                # Check if digit is already assigned in this box
                already_assigned = False
                for r in range(box_row, box_row + 3):
                    for c in range(box_col, box_col + 3):
                        if grid.values[r][c] == digit:
                            already_assigned = True
                            break
                        if has_candidate(grid.candidates[r][c], digit):
                            count += 1
                            last_r = r
                            last_c = c
                    if already_assigned:
                        break

                if already_assigned:
                    continue

                if count == 1:
                    if not assign(grid, last_r, last_c, digit):
                        return False
                    changed = True
                elif count == 0:
                    return False  # Digit cannot be placed anywhere in box

    return True  # Success - reached fixpoint


def find_mrv_cell(grid):
    """
    Find empty cell with Minimum Remaining Values (fewest candidates).
    Returns (row, col) tuple if found, None if no empty cells.
    """
    min_candidates = 10  # More than 9, so any cell will be smaller
    found = False
    mrv_row = -1
    mrv_col = -1

    for r in range(9):
        for c in range(9):
            if grid.values[r][c] == 0:
                num_candidates = count_candidates(grid.candidates[r][c])
                if num_candidates < min_candidates:
                    min_candidates = num_candidates
                    mrv_row = r
                    mrv_col = c
                    found = True

    if found:
        return (mrv_row, mrv_col)
    return None


def cp_search(grid, solution):
    """
    Search with constraint propagation.
    Returns True if solution found, False otherwise.
    """
    # Base case: check if grid is complete
    mrv_cell = find_mrv_cell(grid)
    if mrv_cell is None:
        # No empty cells - grid is complete, extract solution
        for r in range(9):
            for c in range(9):
                solution[r * 9 + c] = grid.values[r][c]
        return True

    mrv_row, mrv_col = mrv_cell

    # Recursive case: try each candidate for the MRV cell
    candidates = grid.candidates[mrv_row][mrv_col]

    for digit in range(1, 10):
        if has_candidate(candidates, digit):
            # Save grid state for backtracking
            grid_copy = CPGrid()
            grid_copy.values = [row[:] for row in grid.values]
            grid_copy.candidates = [row[:] for row in grid.candidates]

            # Try assigning this digit
            if assign(grid, mrv_row, mrv_col, digit):
                # Assignment succeeded, propagate constraints
                if propagate(grid):
                    # Propagation succeeded, recurse
                    if cp_search(grid, solution):
                        return True  # Found solution

            # Failed - restore grid state and try next candidate
            grid.values = grid_copy.values
            grid.candidates = grid_copy.candidates

    # All candidates exhausted - dead end
    return False


def print_puzzle(puzzle):
    """Print puzzle."""
    print("\nPuzzle:")
    for r in range(9):
        print(" ".join(str(puzzle[r][c]) for c in range(9)) + " ")


def read_matrix_file(filename):
    """Read matrix file."""
    puzzle = [[0] * 9 for _ in range(9)]

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

    return puzzle


def main():
    """Main program - process each .matrix file from command line."""
    global cp_iterations

    start = time.time()

    if len(sys.argv) != 2:
        print(f"Usage: {sys.argv[0]} <matrix_file>", file=sys.stderr)
        return 1

    # Read puzzle from file
    try:
        puzzle = read_matrix_file(sys.argv[1])
        print_puzzle(puzzle)

        # Initialize CP grid
        grid = CPGrid()
        init_grid(grid, puzzle)

        # Apply initial propagation
        cp_iterations = 0
        if not propagate(grid):
            print("\nNo solution found (contradiction during initial propagation)\n")
            elapsed = time.time() - start
            print(f"Seconds to process {elapsed:.3f}")
            return 0

        # Run search
        solution = [0] * 81
        solved = cp_search(grid, solution)

        if solved:
            # Convert solution array back to 2D for printing
            solution_grid = [[0] * 9 for _ in range(9)]
            for r in range(9):
                for c in range(9):
                    solution_grid[r][c] = solution[r * 9 + c]

            print_puzzle(solution_grid)
            print(f"\nSolved in Iterations={cp_iterations}\n")
        else:
            print("\nNo solution found\n")

    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        return 1

    elapsed = time.time() - start
    print(f"Seconds to process {elapsed:.3f}")

    return 0


if __name__ == "__main__":
    sys.exit(main())
