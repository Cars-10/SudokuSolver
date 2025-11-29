class Sudoku
  @puzzle : Array(Array(Int32))
  @count : Int32

  def initialize
    @puzzle = Array.new(9) { Array.new(9, 0) }
    @count = 0
  end

  def print_puzzle
    puts "\nPuzzle:"
    @puzzle.each do |row|
      row.each do |val|
        print "#{val} "
      end
      puts
    end
  end

  def read_matrix_file(filename : String)
    puts filename
    lines = File.read_lines(filename)
    row = 0
    lines.each do |line|
      next if line.starts_with?("#") || line.strip.empty?
      parts = line.strip.split(/\s+/)
      if parts.size == 9
        9.times do |col|
          @puzzle[row][col] = parts[col].to_i
        end
        row += 1
        break if row == 9
      end
    end
  end

  def is_possible(y : Int32, x : Int32, val : Int32) : Bool
    9.times do |i|
      return false if @puzzle[i][x] == val
      return false if @puzzle[y][i] == val
    end

    x0 = (x // 3) * 3
    y0 = (y // 3) * 3

    3.times do |i|
      3.times do |j|
        return false if @puzzle[y0 + i][x0 + j] == val
      end
    end
    true
  end

  def solve : Int32
    9.times do |j|
      9.times do |i|
        if @puzzle[j][i] == 0
          (1..9).each do |val|
            @count += 1
            if is_possible(j, i, val)
              @puzzle[j][i] = val
              return 2 if solve == 2
              @puzzle[j][i] = 0
            end
          end
          return 0
        end
      end
    end
    print_puzzle
    puts "\nSolved in Iterations=#{@count}\n"
    return 2
  end

  def run(args)
    start_time = Time.monotonic
    args.each do |arg|
      if arg.ends_with?(".matrix")
        read_matrix_file(arg)
        print_puzzle
        @count = 0
        solve
      end
    end
    end_time = Time.monotonic
    elapsed = (end_time - start_time).total_seconds
    printf "Seconds to process %.3f\n", elapsed
  end
end

Sudoku.new.run(ARGV)
