# Constraint Propagation Sudoku Solver
# Uses bitsets for candidate tracking and MRV heuristic for cell selection

module CP
  alias CandidateSet = UInt16

  class CPGrid
    property values : Array(Array(Int32))
    property candidates : Array(Array(CandidateSet))

    def initialize
      @values = Array(Array(Int32)).new(9) { Array(Int32).new(9, 0) }
      @candidates = Array(Array(CandidateSet)).new(9) { Array(CandidateSet).new(9, 0_u16) }
    end

    def deep_copy : CPGrid
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

  # Global iteration counter
  @@cp_iterations = 0_i64

  # Helper: check if candidate digit is in bitset
  def self.has_cand(cs : CandidateSet, digit : Int32) : Bool
    (cs & (1_u16 << digit)) != 0
  end

  # Helper: count candidates in bitset
  def self.count_cand(cs : CandidateSet) : Int32
    cs.popcount.to_i32
  end

  # Get first candidate digit from bitset (1-9)
  def self.get_first_candidate(cs : CandidateSet) : Int32
    (1..9).each do |digit|
      return digit if has_cand(cs, digit)
    end
    0
  end

  # Get all 20 peers for a cell (row, col, box)
  def self.get_peers(row : Int32, col : Int32) : Array(Tuple(Int32, Int32))
    peers = Array(Tuple(Int32, Int32)).new

    # Same row
    9.times do |c|
      peers << {row, c} if c != col
    end

    # Same column
    9.times do |r|
      peers << {r, col} if r != row
    end

    # Same 3x3 box
    box_row = (row // 3) * 3
    box_col = (col // 3) * 3
    (box_row...box_row + 3).each do |r|
      (box_col...box_col + 3).each do |c|
        peers << {r, c} if r != row && c != col
      end
    end

    peers
  end

  # Initialize grid from puzzle
  def self.init_grid(puzzle : Array(Array(Int32))) : CPGrid
    grid = CPGrid.new

    # Set all cells to either clue value or empty with all candidates
    # Note: We do NOT eliminate clue digits from peers here (matching C implementation)
    9.times do |row|
      9.times do |col|
        digit = puzzle[row][col]
        if digit == 0
          grid.values[row][col] = 0
          grid.candidates[row][col] = 0x3FE_u16 # Binary: 0011 1111 1110 (bits 1-9)
        else
          # Given clue: set single value
          grid.values[row][col] = digit
          grid.candidates[row][col] = (1_u16 << digit)
        end
      end
    end

    grid
  end

  # Eliminate digit from candidates
  def self.eliminate(grid : CPGrid, row : Int32, col : Int32, digit : Int32) : Bool
    # Check if digit is already eliminated
    return true if !has_cand(grid.candidates[row][col], digit)

    # Remove digit from candidates
    grid.candidates[row][col] &= ~(1_u16 << digit)

    # Check for contradiction (no candidates left)
    remaining = count_cand(grid.candidates[row][col])
    if remaining == 0
      return false
    end

    # If only one candidate left, assign it (singleton elimination)
    if remaining == 1 && grid.values[row][col] == 0
      last_digit = get_first_candidate(grid.candidates[row][col])
      return false if !assign(grid, row, col, last_digit)
    end

    true
  end

  # Assign digit to cell and propagate constraints
  def self.assign(grid : CPGrid, row : Int32, col : Int32, digit : Int32) : Bool
    # Increment iteration counter (benchmark metric)
    @@cp_iterations += 1

    # Set value
    grid.values[row][col] = digit
    grid.candidates[row][col] = (1_u16 << digit)

    # Eliminate digit from all peers
    peers = get_peers(row, col)
    peers.each do |peer_row, peer_col|
      return false if !eliminate(grid, peer_row, peer_col, digit)
    end

    true
  end

  # Propagate constraints using singleton and hidden single strategies
  def self.propagate(grid : CPGrid) : Bool
    changed = true

    while changed
      changed = false

      # Strategy 1: Singleton elimination
      9.times do |row|
        9.times do |col|
          if grid.values[row][col] == 0
            num_candidates = count_cand(grid.candidates[row][col])
            return false if num_candidates == 0

            if num_candidates == 1
              digit = get_first_candidate(grid.candidates[row][col])
              return false if !assign(grid, row, col, digit)
              changed = true
            end
          end
        end
      end

      # Strategy 2: Hidden singles
      # Rows
      9.times do |row|
        (1..9).each do |digit|
          count = 0
          last_col = -1
          9.times do |col|
            if grid.values[row][col] == digit
              count = 0
              break
            end
            if has_cand(grid.candidates[row][col], digit)
              count += 1
              last_col = col
            end
          end

          if count == 1
            return false if !assign(grid, row, last_col, digit)
            changed = true
          elsif count == 0
            found = false
            9.times do |col|
              if grid.values[row][col] == digit
                found = true
                break
              end
            end
            return false if !found
          end
        end
      end

      # Columns
      9.times do |col|
        (1..9).each do |digit|
          count = 0
          last_row = -1
          9.times do |row|
            if grid.values[row][col] == digit
              count = 0
              break
            end
            if has_cand(grid.candidates[row][col], digit)
              count += 1
              last_row = row
            end
          end

          if count == 1
            return false if !assign(grid, last_row, col, digit)
            changed = true
          elsif count == 0
            found = false
            9.times do |row|
              if grid.values[row][col] == digit
                found = true
                break
              end
            end
            return false if !found
          end
        end
      end

      # Boxes
      9.times do |box|
        box_row = (box // 3) * 3
        box_col = (box % 3) * 3

        (1..9).each do |digit|
          count = 0
          last_r = -1
          last_c = -1

          already_in_box = false
          (box_row...box_row + 3).each do |r|
            (box_col...box_col + 3).each do |c|
              if grid.values[r][c] == digit
                already_in_box = true
                break
              end
            end
            break if already_in_box
          end

          if already_in_box
            next
          end

          (box_row...box_row + 3).each do |r|
            (box_col...box_col + 3).each do |c|
              if has_cand(grid.candidates[r][c], digit)
                count += 1
                last_r = r
                last_c = c
              end
            end
          end

          if count == 1
            return false if !assign(grid, last_r, last_c, digit)
            changed = true
          elsif count == 0
            return false
          end
        end
      end
    end

    true
  end

  # Find cell with minimum remaining values (MRV heuristic)
  def self.find_mrv_cell(grid : CPGrid) : Tuple(Int32, Int32)?
    min_candidates = 10
    mrv_row = -1
    mrv_col = -1

    9.times do |r|
      9.times do |c|
        if grid.values[r][c] == 0
          num_candidates = count_cand(grid.candidates[r][c])
          if num_candidates < min_candidates
            min_candidates = num_candidates
            mrv_row = r
            mrv_col = c
          end
        end
      end
    end

    return nil if mrv_row == -1
    {mrv_row, mrv_col}
  end

  # Search with backtracking
  def self.cp_search(grid : CPGrid) : Bool
    mrv_cell = find_mrv_cell(grid)
    return true if mrv_cell.nil?

    row, col = mrv_cell
    candidates = grid.candidates[row][col]

    (1..9).each do |digit|
      if has_cand(candidates, digit)
        grid_copy = grid.deep_copy
        if assign(grid, row, col, digit)
          if propagate(grid)
            return true if cp_search(grid)
          end
        end
        # Backtrack
        9.times do |r|
          9.times do |c|
            grid.values[r][c] = grid_copy.values[r][c]
            grid.candidates[r][c] = grid_copy.candidates[r][c]
          end
        end
      end
    end

    false
  end

  def self.parse_matrix_file(filename : String) : Array(Array(Int32))
    puzzle = Array(Array(Int32)).new(9) { Array(Int32).new(9, 0) }
    file = File.open(filename, "r")
    
    # Path normalization
    if filename.starts_with?("/app/Matrices/")
      puts "../#{filename[5..]}"
    else
      puts filename
    end

    line_count = 0
    file.each_line do |line|
      line = line.strip
      next if line.empty? || line.starts_with?('#')
      break if line_count >= 9
      parts = line.split
      if parts.size >= 9
        9.times do |i|
          puzzle[line_count][i] = parts[i].to_i32
          print "#{puzzle[line_count][i]} "
        end
        puts ""
        line_count += 1
      end
    end
    file.close
    puzzle
  end

  def self.solve_puzzle(filename : String)
    puzzle = parse_matrix_file(filename)
    
    # Initialize grid - print initial state
    puts "\nPuzzle:"
    9.times do |r|
      9.times do |c|
        print "#{puzzle[r][c]} "
      end
      puts ""
    end

    grid = init_grid(puzzle)
    @@cp_iterations = 0

    # Apply initial propagation
    if propagate(grid)
      # Search for solution
      if cp_search(grid)
        puts "\nPuzzle:" # C implementation prints "Puzzle:" again before solution? Wait, let's check output.
        # Actually C prints "Puzzle:" then input grid. Then if solved, prints solution.
        # It doesn't print "Puzzle:" before solution.
        # Let's match typical output:
        # Puzzle:
        # ...
        # ... solution grid ...
        # Solved in ...
        9.times do |r|
          9.times do |c|
            print "#{grid.values[r][c]} "
          end
          puts ""
        end
        puts "\nSolved in Iterations=#{@@cp_iterations}\n"
      else
        puts "\nNo solution found"
      end
    else
      puts "\nNo solution found (initial propagation failed)"
    end
  end

  def self.main
    start_time = Time.monotonic
    ARGV.each do |filename|
      if filename.ends_with?(".matrix")
        solve_puzzle(filename)
      end
    end
    elapsed = Time.monotonic - start_time
    puts "Seconds to process %.3f" % elapsed.total_seconds
  end
end

CP.main
