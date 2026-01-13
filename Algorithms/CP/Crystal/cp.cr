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

    # Same row (9 cells minus self = 8)
    9.times do |c|
      peers << {row, c} if c != col
    end

    # Same column (9 cells minus self = 8)
    9.times do |r|
      peers << {r, col} if r != row
    end

    # Same 3x3 box (9 cells minus self minus already counted = 4)
    box_row = (row // 3) * 3
    box_col = (col // 3) * 3
    (box_row...box_row + 3).each do |r|
      (box_col...box_col + 3).each do |c|
        peers << {r, c} if r != row && c != col
      end
    end

    peers
  end

  # Initialize grid from puzzle (matching C implementation exactly)
  def self.init_grid(puzzle : Array(Array(Int32))) : CPGrid
    grid = CPGrid.new

    # Set all cells to either clue value or empty with all candidates
    # Note: We do NOT eliminate clue digits from peers here
    # That happens during propagate via the hidden singles logic
    9.times do |row|
      9.times do |col|
        if puzzle[row][col] == 0
          # Empty cell: set all candidates 1-9 (bits 1-9 set)
          grid.values[row][col] = 0
          grid.candidates[row][col] = 0x3FE_u16  # Binary: 0011 1111 1110 (bits 1-9)
        else
          # Given clue: set single value
          digit = puzzle[row][col]
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
      # But if cell already has this digit assigned, it's OK (it's a clue)
      return true if grid.values[row][col] == digit
      return false  # Contradiction
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
      # If a cell has only one candidate, assign it
      9.times do |row|
        9.times do |col|
          if grid.values[row][col] == 0
            num_candidates = count_cand(grid.candidates[row][col])
            return false if num_candidates == 0  # Contradiction

            if num_candidates == 1
              digit = get_first_candidate(grid.candidates[row][col])
              return false if !assign(grid, row, col, digit)
              changed = true
            end
          end
        end
      end

      # Strategy 2: Hidden singles
      # For each unit (row, col, box), if a digit appears in only one cell, assign it

      # Check rows
      9.times do |row|
        (1..9).each do |digit|
          count = 0
          last_col = -1
          9.times do |col|
            if grid.values[row][col] == digit
              count = 0  # Already assigned
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
            # Check if digit is already assigned in this row
            found = false
            9.times do |col|
              if grid.values[row][col] == digit
                found = true
                break
              end
            end
            return false if !found  # Digit cannot be placed anywhere in row
          end
        end
      end

      # Check columns
      9.times do |col|
        (1..9).each do |digit|
          count = 0
          last_row = -1
          9.times do |row|
            if grid.values[row][col] == digit
              count = 0  # Already assigned
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
            # Check if digit is already assigned in this column
            found = false
            9.times do |row|
              if grid.values[row][col] == digit
                found = true
                break
              end
            end
            return false if !found  # Digit cannot be placed anywhere in column
          end
        end
      end

      # Check boxes
      9.times do |box|
        box_row = (box // 3) * 3
        box_col = (box % 3) * 3

        (1..9).each do |digit|
          count = 0
          last_r = -1
          last_c = -1

          (box_row...box_row + 3).each do |r|
            (box_col...box_col + 3).each do |c|
              if grid.values[r][c] == digit
                count = 0  # Already assigned
                break
              end
              if has_cand(grid.candidates[r][c], digit)
                count += 1
                last_r = r
                last_c = c
              end
            end
            break if count == 0 && grid.values.any? { |row| row.any? { |v| v == digit } }
          end

          if count == 1
            return false if !assign(grid, last_r, last_c, digit)
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
            return false if !found  # Digit cannot be placed anywhere in box
          end
        end
      end
    end

    true  # Success - reached fixpoint
  end

  # Find cell with minimum remaining values (MRV heuristic)
  def self.find_mrv_cell(grid : CPGrid) : Tuple(Int32, Int32)?
    min_candidates = 10  # More than 9, so any cell will be smaller
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

    return nil if mrv_row == -1  # No empty cells (grid complete)
    {mrv_row, mrv_col}
  end

  # Search with backtracking
  def self.cp_search(grid : CPGrid) : Bool
    # Base case: check if grid is complete
    mrv_cell = find_mrv_cell(grid)
    if mrv_cell.nil?
      # No empty cells - grid is complete
      return true
    end

    mrv_row, mrv_col = mrv_cell

    # Recursive case: try each candidate for the MRV cell
    candidates = grid.candidates[mrv_row][mrv_col]

    (1..9).each do |digit|
      if has_cand(candidates, digit)
        # Save grid state for backtracking
        grid_copy = grid.deep_copy

        # Try assigning this digit
        if assign(grid, mrv_row, mrv_col, digit)
          # Assignment succeeded, propagate constraints
          if propagate(grid)
            # Propagation succeeded, recurse
            return true if cp_search(grid)
          end
        end

        # Failed - restore grid state and try next candidate
        9.times do |r|
          9.times do |c|
            grid.values[r][c] = grid_copy.values[r][c]
            grid.candidates[r][c] = grid_copy.candidates[r][c]
          end
        end
      end
    end

    # All candidates exhausted - dead end
    false
  end

  # Parse matrix file
  def self.parse_matrix_file(filename : String) : Array(Array(Int32))
    puzzle = Array(Array(Int32)).new(9) { Array(Int32).new(9, 0) }

    begin
      file = File.open(filename, "r")

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
            puzzle[line_count][i] = parts[i].to_i32
            print "#{puzzle[line_count][i]} "
          end
          puts ""
          line_count += 1
        end
      end

      file.close
    rescue ex
      STDERR.puts "Error opening file '#{filename}': #{ex.message}"
      exit(1)
    end

    puzzle
  end

  # Print puzzle
  def self.print_puzzle(grid : CPGrid)
    puts "\nPuzzle:"
    9.times do |row|
      9.times do |col|
        print "#{grid.values[row][col]} "
      end
      puts ""
    end
  end

  # Main solving function
  def self.solve_puzzle(filename : String)
    puzzle = parse_matrix_file(filename)

    # Initialize grid
    grid = init_grid(puzzle)
    print_puzzle(grid)

    # Reset iteration counter
    @@cp_iterations = 0

    # Apply initial propagation
    if propagate(grid)
      # Search for solution
      if cp_search(grid)
        print_puzzle(grid)
        puts "\nSolved in Iterations=#{@@cp_iterations}\n"
      else
        puts "No solution found"
      end
    else
      puts "No solution found (initial propagation failed)"
    end
  end

  # Main entry point
  def self.main
    start_time = Time.monotonic

    ARGV.each do |filename|
      if filename.size >= 7 && filename[-7..] == ".matrix"
        solve_puzzle(filename)
      end
    end

    elapsed = Time.monotonic - start_time
    puts "Seconds to process %.3f" % elapsed.total_seconds
  end
end

CP.main
