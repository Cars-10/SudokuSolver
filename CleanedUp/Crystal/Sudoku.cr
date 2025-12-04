class Sudoku
  @@puzzle = Array(Array(Int32)).new(9) { Array(Int32).new(9, 0) }
  @@iterations = 0

  def self.main
    if ARGV.empty?
      puts "Usage: ./Sudoku <file1> <file2> ..."
    else
      ARGV.each do |filename|
        puts "\nProcessing #{filename}"
        if read_board(filename)
          print_board
          @@iterations = 0
          if solve(0, 0)
            print_board
            puts "\nSolved in Iterations=#{@@iterations}"
          else
            puts "No solution found"
          end
        end
      end
    end
  end

  def self.read_board(filename)
    begin
      lines = File.read_lines(filename)
      row = 0
      lines.each do |line|
        trimmed = line.strip
        if !trimmed.empty? && !trimmed.starts_with?("#")
          parts = trimmed.split(/\s+/)
          col = 0
          parts.each do |part|
            if col < 9
              @@puzzle[row][col] = part.to_i
              col += 1
            end
          end
          row += 1
          return true if row == 9
        end
      end
      return true
    rescue ex
      puts "Error reading file #{filename}: #{ex.message}"
      return false
    end
  end

  def self.print_board
    puts "Puzzle:"
    @@puzzle.each do |row|
      row.each do |val|
        print "#{val} "
      end
      puts
    end
  end

  def self.is_possible(row, col, num)
    (0..8).each do |i|
      return false if @@puzzle[row][i] == num || @@puzzle[i][col] == num
    end

    start_row = (row // 3) * 3
    start_col = (col // 3) * 3
    (0..2).each do |i|
      (0..2).each do |j|
        return false if @@puzzle[start_row + i][start_col + j] == num
      end
    end
    true
  end

  def self.solve(row, col)
    return true if row == 9

    next_row = row
    next_col = col + 1
    if next_col == 9
      next_row = row + 1
      next_col = 0
    end

    if @@puzzle[row][col] != 0
      return solve(next_row, next_col)
    end

    (1..9).each do |num|
      @@iterations += 1
      if is_possible(row, col, num)
        @@puzzle[row][col] = num
        return true if solve(next_row, next_col)
        @@puzzle[row][col] = 0
      end
    end
    false
  end
end

Sudoku.main
