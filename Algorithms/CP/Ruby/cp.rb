#!/usr/bin/env ruby
# CP (Constraint Propagation) Sudoku Solver - Ruby Implementation
# Uses constraint propagation with MRV (Minimum Remaining Values) heuristic
#
# Algorithm:
# - Track candidates for each cell using bitsets
# - Propagate constraints (singleton elimination + hidden singles)
# - Use MRV heuristic for cell selection during search
# - Count iterations via assign() calls

# Global iteration counter
$cp_iterations = 0

# CPGrid class - holds values and candidates
class CPGrid
  attr_accessor :values, :candidates

  def initialize
    @values = Array.new(9) { Array.new(9, 0) }
    @candidates = Array.new(9) { Array.new(9, 0) }
  end

  def deep_copy
    copy = CPGrid.new
    9.times do |r|
      9.times do |c|
        copy.values[r][c] = @values[r][c]
        copy.candidates[r][c] = @candidates[r][c]
      end
    end
    copy
  end
end

# Bitset helper methods
def has_candidate?(set, digit)
  (set & (1 << digit)) != 0
end

def add_candidate(set, digit)
  set | (1 << digit)
end

def remove_candidate(set, digit)
  set & ~(1 << digit)
end

def count_candidates(set)
  # Count set bits using Ruby's built-in method
  set.to_s(2).count('1')
end

def get_first_candidate(set)
  (1..9).each do |digit|
    return digit if has_candidate?(set, digit)
  end
  0
end

# Get all 20 peers for a cell (row, col, box)
def get_peers(row, col)
  peers = []

  # Same row (8 cells)
  9.times do |c|
    peers << [row, c] if c != col
  end

  # Same column (8 cells)
  9.times do |r|
    peers << [r, col] if r != row
  end

  # Same 3x3 box (4 remaining cells)
  box_row = (row / 3) * 3
  box_col = (col / 3) * 3
  (box_row...box_row + 3).each do |r|
    (box_col...box_col + 3).each do |c|
      peers << [r, c] if r != row && c != col
    end
  end

  peers
end

# Initialize grid with puzzle
def init_grid(grid, puzzle)
  9.times do |row|
    9.times do |col|
      if puzzle[row][col] == 0
        # Empty cell: set all candidates 1-9
        grid.values[row][col] = 0
        grid.candidates[row][col] = 0x3FE  # Binary: 0011 1111 1110 (bits 1-9)
      else
        # Given clue: set single value
        digit = puzzle[row][col]
        grid.values[row][col] = digit
        grid.candidates[row][col] = (1 << digit)
      end
    end
  end
end

# Eliminate a digit from a cell's candidates
def eliminate(grid, row, col, digit)
  # Check if digit is already eliminated
  return true unless has_candidate?(grid.candidates[row][col], digit)

  # Remove digit from candidates
  grid.candidates[row][col] = remove_candidate(grid.candidates[row][col], digit)

  # Check for contradiction (no candidates left)
  remaining = count_candidates(grid.candidates[row][col])
  return false if remaining == 0

  # If only one candidate left, assign it (singleton elimination)
  if remaining == 1 && grid.values[row][col] == 0
    last_digit = get_first_candidate(grid.candidates[row][col])
    return false unless assign(grid, row, col, last_digit)
  end

  true
end

# Assign a value to a cell
def assign(grid, row, col, digit)
  # Increment iteration counter
  $cp_iterations += 1

  # Set value
  grid.values[row][col] = digit
  grid.candidates[row][col] = (1 << digit)

  # Eliminate digit from all peers
  peers = get_peers(row, col)
  peers.each do |peer_row, peer_col|
    return false unless eliminate(grid, peer_row, peer_col, digit)
  end

  true
end

# Propagate constraints until quiescence
def propagate(grid)
  changed = true

  while changed
    changed = false

    # Strategy 1: Singleton elimination
    # If a cell has only one candidate, assign it
    9.times do |row|
      9.times do |col|
        next if grid.values[row][col] != 0

        num_candidates = count_candidates(grid.candidates[row][col])
        return false if num_candidates == 0

        if num_candidates == 1
          digit = get_first_candidate(grid.candidates[row][col])
          return false unless assign(grid, row, col, digit)
          changed = true
        end
      end
    end

    # Strategy 2: Hidden singles - rows
    9.times do |row|
      (1..9).each do |digit|
        count = 0
        last_col = -1

        9.times do |col|
          if grid.values[row][col] == digit
            count = 0  # Already assigned
            break
          end
          if has_candidate?(grid.candidates[row][col], digit)
            count += 1
            last_col = col
          end
        end

        if count == 1
          return false unless assign(grid, row, last_col, digit)
          changed = true
        elsif count == 0
          # Check if digit is already assigned in this row
          found = (0...9).any? { |col| grid.values[row][col] == digit }
          return false unless found
        end
      end
    end

    # Strategy 2: Hidden singles - columns
    9.times do |col|
      (1..9).each do |digit|
        count = 0
        last_row = -1

        9.times do |row|
          if grid.values[row][col] == digit
            count = 0  # Already assigned
            break
          end
          if has_candidate?(grid.candidates[row][col], digit)
            count += 1
            last_row = row
          end
        end

        if count == 1
          return false unless assign(grid, last_row, col, digit)
          changed = true
        elsif count == 0
          # Check if digit is already assigned in this column
          found = (0...9).any? { |row| grid.values[row][col] == digit }
          return false unless found
        end
      end
    end

    # Strategy 2: Hidden singles - boxes
    9.times do |box|
      box_row = (box / 3) * 3
      box_col = (box % 3) * 3

      (1..9).each do |digit|
        count = 0
        last_r = -1
        last_c = -1
        already_assigned = false

        (box_row...box_row + 3).each do |r|
          (box_col...box_col + 3).each do |c|
            if grid.values[r][c] == digit
              already_assigned = true
              break
            end
            if has_candidate?(grid.candidates[r][c], digit)
              count += 1
              last_r = r
              last_c = c
            end
          end
          break if already_assigned
        end

        next if already_assigned

        if count == 1
          return false unless assign(grid, last_r, last_c, digit)
          changed = true
        elsif count == 0
          # Check if digit is already assigned in this box
          found = false
          (box_row...box_row + 3).each do |r|
            (box_col...box_col + 3).each do |c|
              if grid.values[r][c] == digit
                found = true
                break
              end
            end
            break if found
          end
          return false unless found
        end
      end
    end
  end

  true  # Success - reached fixpoint
end

# Find cell with Minimum Remaining Values
def find_mrv_cell(grid)
  min_candidates = 10
  mrv_row = -1
  mrv_col = -1

  9.times do |r|
    9.times do |c|
      next if grid.values[r][c] != 0

      num_candidates = count_candidates(grid.candidates[r][c])
      if num_candidates < min_candidates
        min_candidates = num_candidates
        mrv_row = r
        mrv_col = c
      end
    end
  end

  return nil if mrv_row == -1  # No empty cells

  [mrv_row, mrv_col]
end

# CP Search with backtracking
def cp_search(grid, solution)
  # Base case: check if grid is complete
  mrv_cell = find_mrv_cell(grid)

  unless mrv_cell
    # No empty cells - grid is complete
    9.times do |r|
      9.times do |c|
        solution[r * 9 + c] = grid.values[r][c]
      end
    end
    return true
  end

  mrv_row, mrv_col = mrv_cell
  candidates = grid.candidates[mrv_row][mrv_col]

  # Try each candidate for the MRV cell
  (1..9).each do |digit|
    next unless has_candidate?(candidates, digit)

    # Save grid state for backtracking
    grid_copy = grid.deep_copy

    # Try assigning this digit
    if assign(grid, mrv_row, mrv_col, digit)
      # Assignment succeeded, propagate constraints
      if propagate(grid)
        # Propagation succeeded, recurse
        return true if cp_search(grid, solution)
      end
    end

    # Failed - restore grid state and try next candidate
    grid.values = grid_copy.values
    grid.candidates = grid_copy.candidates
  end

  # All candidates exhausted - dead end
  false
end

# Print puzzle
def print_puzzle(grid)
  puts "\nPuzzle:"
  grid.each do |row|
    puts row.map(&:to_s).join(" ") + " "
  end
end

# Read matrix file
def read_matrix_file(filename)
  # Normalize path for output
  display_path = filename
  if filename.start_with?("/app/Matrices/")
    display_path = "../" + filename[5..-1]
  end
  puts display_path

  puzzle = Array.new(9) { Array.new(9, 0) }
  line_count = 0

  File.foreach(filename) do |line|
    line = line.strip
    next if line.empty? || line.start_with?('#')

    values = line.split.map(&:to_i)
    if values.length == 9 && line_count < 9
      puzzle[line_count] = values
      puts values.map(&:to_s).join(" ") + " "
      line_count += 1
    end
  end

  puzzle
end

# Main program
if ARGV.empty?
  puts "Usage: ruby cp.rb <matrix_file>"
  exit 1
end

start_time = Time.now

ARGV.each do |filename|
  next unless filename.end_with?(".matrix")

  # Read puzzle from file
  puzzle = read_matrix_file(filename)
  print_puzzle(puzzle)

  # Initialize CP grid
  grid = CPGrid.new
  init_grid(grid, puzzle)

  # Apply initial propagation
  unless propagate(grid)
    puts "\nNo solution found (contradiction during initial propagation)"
    elapsed = Time.now - start_time
    puts "Seconds to process %.3f" % elapsed
    next
  end

  # Run search
  $cp_iterations = 0
  solution = Array.new(81)
  solved = cp_search(grid, solution)

  if solved
    # Convert solution array back to 2D for printing
    solution_grid = Array.new(9) { Array.new(9, 0) }
    9.times do |r|
      9.times do |c|
        solution_grid[r][c] = solution[r * 9 + c]
      end
    end

    print_puzzle(solution_grid)
    puts "\nSolved in Iterations=#{$cp_iterations}\n"
  else
    puts "\nNo solution found"
  end
end

elapsed = Time.now - start_time
puts "Seconds to process %.3f" % elapsed
