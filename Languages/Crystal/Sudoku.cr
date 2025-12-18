module Sudoku
  # Sudoku puzzle grid [row][col]
  @@puzzle = Array(Array(Int32)).new(9) { Array(Int32).new(9, 0) }
  @@count = 0  # Iteration counter

  def self.print_puzzle
    puts "\nPuzzle:"
    9.times do |j|
      9.times do |i|
        print "#{@@puzzle[j][i]} "
      end
      puts ""
    end
  end

  def self.read_matrix_file(filename : String) : Int32
    file = File.open(filename, "r") rescue nil
    if file.nil?
      STDERR.puts "Error opening file '#{filename}'"
      return 1
    end

    # Normalize path for output (convert absolute to relative)
    if filename.size >= 14 && filename[0, 14] == "/app/Matrices/"
      puts "../#{filename[5..]}"
    else
      puts filename
    end

    line_count = 0
    file.each_line do |line|
      line_str = line.strip

      # Skip comments and empty lines
      next if line_str.empty? || line_str[0] == '#'

      break if line_count >= 9

      # Parse 9 integers from line
      parts = line_str.split
      if parts.size >= 9
        9.times do |i|
          @@puzzle[line_count][i] = parts[i].to_i32
          print "#{@@puzzle[line_count][i]} "
        end
        puts ""
        line_count += 1
      end
    end

    file.close
    0
  end

  # Check if placing val at (row, col) is valid
  def self.is_valid(row : Int32, col : Int32, val : Int32) : Bool
    # Check row
    9.times do |i|
      return false if @@puzzle[row][i] == val
    end

    # Check column
    9.times do |i|
      return false if @@puzzle[i][col] == val
    end

    # Check 3x3 box
    box_row = (row // 3) * 3
    box_col = (col // 3) * 3
    3.times do |i|
      3.times do |j|
        return false if @@puzzle[box_row + i][box_col + j] == val
      end
    end

    true
  end

  # BRUTE-FORCE SOLVER
  # Searches row-major order (top-to-bottom, left-to-right)
  # Tries candidates 1-9 in ascending order
  # Counts EVERY placement attempt (the algorithm fingerprint)
  def self.solve : Bool
    # Find first empty cell (row-major order)
    row = -1
    col = -1
    9.times do |r|
      9.times do |c|
        if @@puzzle[r][c] == 0
          row = r
          col = c
          break
        end
      end
      break if row != -1
    end

    # If no empty cell found, puzzle is solved
    if row == -1
      print_puzzle
      puts "\nSolved in Iterations=#{@@count}\n"
      return true  # Success
    end

    # Try values 1-9 in order
    (1..9).each do |val|
      @@count += 1  # COUNT EVERY ATTEMPT - this is the algorithm fingerprint

      if is_valid(row, col, val)
        @@puzzle[row][col] = val  # Place value

        return true if solve  # Solved

        @@puzzle[row][col] = 0  # Backtrack
      end
    end

    false  # No solution found
  end

  def self.main
    start_time = Time.monotonic

    # Process each .matrix file from command line
    ARGV.each do |filename|
      if filename.size >= 7 && filename[-7..] == ".matrix"
        if read_matrix_file(filename) != 0
          STDERR.puts "Error reading #{filename}"
          next
        end

        print_puzzle
        @@count = 0
        solve
      end
    end

    elapsed = Time.monotonic - start_time
    puts "Seconds to process %.3f" % elapsed.total_seconds
  end
end

Sudoku.main
