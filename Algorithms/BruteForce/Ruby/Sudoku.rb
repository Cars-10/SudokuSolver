#!/usr/bin/env ruby
# Sudoku Solver - Ruby Implementation
# Brute-force backtracking algorithm matching C reference exactly.
#
# Algorithm:
# - Row-major search for empty cells (top-to-bottom, left-to-right)
# - Try values 1-9 in ascending order
# - Count EVERY placement attempt (algorithm fingerprint)

# Global puzzle grid [row][col]
$puzzle = Array.new(9) { Array.new(9, 0) }
$count = 0  # Iteration counter

def print_puzzle
  puts "\nPuzzle:"
  9.times do |row|
    puts $puzzle[row].map(&:to_s).join(" ") + " "
  end
end

def read_matrix_file(filename)
  # Normalize path for output (match C format)
  display_path = filename
  if filename.start_with?("/app/Matrices/")
    display_path = "../" + filename[5..-1]  # Skip "/app/" to get "Matrices/..."
  end
  puts display_path

  line_count = 0
  File.foreach(filename) do |line|
    line = line.strip
    # Skip comments and empty lines
    next if line.empty? || line.start_with?('#')

    # Parse 9 integers from line
    values = line.split.map(&:to_i)
    if values.length == 9 && line_count < 9
      $puzzle[line_count] = values
      puts values.map(&:to_s).join(" ") + " "
      line_count += 1
    end
  end
end

def is_valid(row, col, val)
  # Check row
  9.times do |i|
    return false if $puzzle[row][i] == val
  end

  # Check column
  9.times do |i|
    return false if $puzzle[i][col] == val
  end

  # Check 3x3 box
  box_row = (row / 3) * 3
  box_col = (col / 3) * 3
  3.times do |i|
    3.times do |j|
      return false if $puzzle[box_row + i][box_col + j] == val
    end
  end

  true
end

def solve
  # Find first empty cell (row-major order)
  row = -1
  col = -1
  catch(:found) do
    9.times do |r|
      9.times do |c|
        if $puzzle[r][c] == 0
          row = r
          col = c
          throw :found
        end
      end
    end
  end

  # If no empty cell found, puzzle is solved
  if row == -1
    print_puzzle
    puts "\nSolved in Iterations=#{$count}\n"
    return true
  end

  # Try values 1-9 in order
  (1..9).each do |val|
    $count += 1  # COUNT EVERY ATTEMPT - this is the algorithm fingerprint

    if is_valid(row, col, val)
      $puzzle[row][col] = val  # Place value

      return true if solve

      $puzzle[row][col] = 0  # Backtrack
    end
  end

  false
end

# Main program - process each .matrix file from command line
start_time = Time.now

ARGV.each do |arg|
  next unless arg.end_with?(".matrix")

  # Reset puzzle
  $puzzle = Array.new(9) { Array.new(9, 0) }

  read_matrix_file(arg)
  print_puzzle
  $count = 0
  solve
end

elapsed = Time.now - start_time
puts "Seconds to process %.3f" % elapsed
