#!/usr/bin/env ruby
# DLX (Dancing Links) Sudoku Solver - Ruby Implementation
# Algorithm X with Dancing Links for Exact Cover problem
#
# This implements Knuth's Algorithm X using circular doubly-linked lists
# for efficient column covering/uncovering operations.

# DLX Node - circular doubly-linked list node
class DlxNode
  attr_accessor :up, :down, :left, :right, :column, :row_id

  def initialize
    @up = self
    @down = self
    @left = self
    @right = self
    @column = nil
    @row_id = -1
  end
end

# DLX Column - special node that also tracks column size
class DlxColumn < DlxNode
  attr_accessor :size, :name

  def initialize(name = "")
    super()
    @size = 0
    @name = name
    @column = self  # Column points to itself
  end
end

# Global iteration counter (analogous to brute-force count)
$dlx_iterations = 0

# Sudoku puzzle and solution
$puzzle = Array.new(9) { Array.new(9, 0) }
$solution_grid = Array.new(9) { Array.new(9, 0) }

# DLX matrix structures
$root = nil
$columns = []
$row_info = []  # Array of {row:, col:, num:} hashes

# Cover a column in the DLX matrix
# Remove column from header list and remove all rows in the column's list
def cover_column(c)
  # Remove column header from the header list
  c.right.left = c.left
  c.left.right = c.right

  # For each row in this column
  row_node = c.down
  while row_node != c
    # For each node in this row (excluding the column itself)
    right_node = row_node.right
    while right_node != row_node
      # Remove this node from its column
      right_node.down.up = right_node.up
      right_node.up.down = right_node.down
      right_node.column.size -= 1
      right_node = right_node.right
    end
    row_node = row_node.down
  end
end

# Uncover a column (exact reverse of cover)
def uncover_column(c)
  # For each row in this column (in reverse order)
  row_node = c.up
  while row_node != c
    # For each node in this row (in reverse order)
    left_node = row_node.left
    while left_node != row_node
      # Restore this node to its column
      left_node.column.size += 1
      left_node.down.up = left_node
      left_node.up.down = left_node
      left_node = left_node.left
    end
    row_node = row_node.up
  end

  # Restore column header to the header list
  c.right.left = c
  c.left.right = c
end

# Choose column with minimum size (Knuth's S heuristic)
def choose_column(root)
  best = nil
  min_size = Float::INFINITY

  col_node = root.right
  while col_node != root
    if col_node.size < min_size
      min_size = col_node.size
      best = col_node
    end
    col_node = col_node.right
  end

  best
end

# DLX Search - Algorithm X with Dancing Links
# Returns true if solution found, false otherwise
def dlx_search(root, k, solution)
  $dlx_iterations += 1  # Count every search call

  # If matrix is empty, we found a solution
  return true if root.right == root

  # Choose column with minimum size
  col = choose_column(root)

  # If column has no rows, no solution possible
  return false if col.size == 0

  # Cover this column
  cover_column(col)

  # Try each row in this column
  row_node = col.down
  while row_node != col
    # Add row to partial solution
    solution[k] = row_node.row_id

    # Cover all other columns in this row
    right_node = row_node.right
    while right_node != row_node
      cover_column(right_node.column)
      right_node = right_node.right
    end

    # Recurse
    if dlx_search(root, k + 1, solution)
      return true  # Solution found
    end

    # Backtrack: uncover all columns in this row
    left_node = row_node.left
    while left_node != row_node
      uncover_column(left_node.column)
      left_node = left_node.left
    end

    row_node = row_node.down
  end

  # Uncover column
  uncover_column(col)

  false  # No solution found
end

# Calculate constraint column indices
# For a Sudoku cell (r,c) with value n (1-9):
# - Position constraint: (r,c) must be filled -> column index: r*9 + c
# - Row constraint: row r must have number n -> column index: 81 + r*9 + (n-1)
# - Column constraint: column c must have number n -> column index: 162 + c*9 + (n-1)
# - Box constraint: box b must have number n -> column index: 243 + b*9 + (n-1)

def get_position_col(r, c)
  r * 9 + c
end

def get_row_col(r, n)
  81 + r * 9 + (n - 1)
end

def get_col_col(c, n)
  162 + c * 9 + (n - 1)
end

def get_box_col(r, c, n)
  box = (r / 3) * 3 + (c / 3)
  243 + box * 9 + (n - 1)
end

# Add a node to the DLX matrix
def add_node(col, row_id)
  node = DlxNode.new
  node.column = col
  node.row_id = row_id

  # Insert at end of column's circular list
  node.down = col
  node.up = col.up
  col.up.down = node
  col.up = node
  col.size += 1

  node
end

# Build a DLX row for Sudoku cell (r,c) with value n
def build_dlx_row(r, c, n, row_id)
  # Store row metadata
  $row_info[row_id] = {row: r, col: c, num: n}

  # Create nodes for the 4 constraints
  n1 = add_node($columns[get_position_col(r, c)], row_id)
  n2 = add_node($columns[get_row_col(r, n)], row_id)
  n3 = add_node($columns[get_col_col(c, n)], row_id)
  n4 = add_node($columns[get_box_col(r, c, n)], row_id)

  # Link nodes horizontally in circular list
  n1.right = n2
  n2.right = n3
  n3.right = n4
  n4.right = n1

  n1.left = n4
  n2.left = n1
  n3.left = n2
  n4.left = n3

  n1
end

# Initialize DLX matrix structure
def init_dlx_matrix
  # Allocate root column
  $root = DlxColumn.new("root")

  # Allocate 324 column headers
  $columns = []
  324.times do |i|
    col = DlxColumn.new("C#{i}")

    # Link into header list
    col.left = $root.left
    col.right = $root
    $root.left.right = col
    $root.left = col

    $columns << col
  end

  $row_info = []
end

# Build the complete DLX matrix from the puzzle
def build_dlx_matrix_from_puzzle
  row_id = 0
  row_starts = []

  9.times do |r|
    9.times do |c|
      if $puzzle[r][c] != 0
        # Cell has a clue - create only one row for that value
        row_starts[row_id] = build_dlx_row(r, c, $puzzle[r][c], row_id)
        row_id += 1
      else
        # Cell is empty - create rows for all possible values
        (1..9).each do |n|
          row_starts[row_id] = build_dlx_row(r, c, n, row_id)
          row_id += 1
        end
      end
    end
  end

  row_starts
end

# Cover given clues (pre-selected rows)
def cover_clues(row_starts)
  9.times do |r|
    9.times do |c|
      if $puzzle[r][c] != 0
        n = $puzzle[r][c]

        # Find the row for this clue
        row_starts.each_with_index do |start_node, row_id|
          next unless start_node
          info = $row_info[row_id]
          if info[:row] == r && info[:col] == c && info[:num] == n
            # Cover all columns in this row
            node = start_node
            curr = node
            loop do
              cover_column(curr.column)
              curr = curr.right
              break if curr == node
            end
            break
          end
        end
      end
    end
  end
end

# Extract solution from DLX and populate solution_grid
def extract_solution(solution)
  # Initialize solution grid with original puzzle
  $solution_grid = $puzzle.map(&:dup)

  # Each solution entry is a row_id
  solution.each do |row_id|
    next if row_id.nil? || row_id < 0
    info = $row_info[row_id]
    $solution_grid[info[:row]][info[:col]] = info[:num] if info
  end
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

  line_count = 0
  File.foreach(filename) do |line|
    line = line.strip
    next if line.empty? || line.start_with?('#')

    values = line.split.map(&:to_i)
    if values.length == 9 && line_count < 9
      $puzzle[line_count] = values
      puts values.map(&:to_s).join(" ") + " "
      line_count += 1
    end
  end
end

# Main program
if ARGV.empty?
  puts "Usage: ruby dlx.rb <matrix_file>"
  exit 1
end

start_time = Time.now

ARGV.each do |filename|
  next unless filename.end_with?(".matrix")

  # Reset puzzle
  $puzzle = Array.new(9) { Array.new(9, 0) }

  read_matrix_file(filename)
  print_puzzle($puzzle)

  # Initialize DLX matrix
  init_dlx_matrix

  # Build matrix from puzzle
  row_starts = build_dlx_matrix_from_puzzle

  # Cover pre-filled clues
  cover_clues(row_starts)

  # Solve using DLX
  $dlx_iterations = 0
  solution = Array.new(81)
  result = dlx_search($root, 0, solution)

  if result
    extract_solution(solution)
    print_puzzle($solution_grid)
    puts "\nSolved in Iterations=#{$dlx_iterations}\n"
  else
    puts "\nNo solution found"
  end
end

elapsed = Time.now - start_time
puts "Seconds to process %.3f" % elapsed
