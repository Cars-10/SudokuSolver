# Dancing Links (DLX) Sudoku Solver
# Implements Algorithm X using Knuth's Dancing Links technique

module DLX
  class DlxNode
    property left : DlxNode?
    property right : DlxNode?
    property up : DlxNode?
    property down : DlxNode?
    property column : DlxNode?
    property size : Int32
    property row_id : Int32
    property col_id : Int32

    def initialize
      @left = nil
      @right = nil
      @up = nil
      @down = nil
      @column = nil
      @size = 0
      @row_id = -1
      @col_id = -1
    end
  end

  # Row metadata to map DLX rows back to Sudoku (row, col, num)
  class RowInfo
    property row : Int32
    property col : Int32
    property num : Int32

    def initialize(@row : Int32, @col : Int32, @num : Int32)
    end
  end

  # Global iteration counter
  @@dlx_iterations = 0
  @@row_info = Array(RowInfo).new
  @@row_starts = Array(DlxNode?).new

  # Cover a column in the DLX matrix
  def self.cover_column(c : DlxNode)
    # Remove column header from the header list
    c.right.not_nil!.left = c.left
    c.left.not_nil!.right = c.right

    # For each row in this column
    row_node = c.down
    while row_node != c
      # For each node in this row (excluding the column itself)
      right_node = row_node.not_nil!.right
      while right_node != row_node
        # Remove this node from its column
        right_node.not_nil!.down.not_nil!.up = right_node.not_nil!.up
        right_node.not_nil!.up.not_nil!.down = right_node.not_nil!.down
        col = right_node.not_nil!.column
        col.not_nil!.size -= 1
        right_node = right_node.not_nil!.right
      end
      row_node = row_node.not_nil!.down
    end
  end

  # Uncover a column (exact reverse of cover)
  def self.uncover_column(c : DlxNode)
    # For each row in this column (in reverse order)
    row_node = c.up
    while row_node != c
      # For each node in this row (in reverse order)
      left_node = row_node.not_nil!.left
      while left_node != row_node
        # Restore this node to its column
        col = left_node.not_nil!.column
        col.not_nil!.size += 1
        left_node.not_nil!.down.not_nil!.up = left_node
        left_node.not_nil!.up.not_nil!.down = left_node
        left_node = left_node.not_nil!.left
      end
      row_node = row_node.not_nil!.up
    end

    # Restore column header to the header list
    c.right.not_nil!.left = c
    c.left.not_nil!.right = c
  end

  # Choose column with minimum size (Knuth's S heuristic)
  def self.choose_column(root : DlxNode) : DlxNode?
    best : DlxNode? = nil
    min_size = Int32::MAX

    col_node = root.right
    while col_node != root
      if col_node.not_nil!.size < min_size
        min_size = col_node.not_nil!.size
        best = col_node
      end
      col_node = col_node.not_nil!.right
    end

    best
  end

  # DLX Search - Algorithm X with Dancing Links
  def self.dlx_search(root : DlxNode, k : Int32, solution : Array(Int32)) : Bool
    @@dlx_iterations += 1  # Count every search call

    # If matrix is empty, we found a solution
    if root.right == root
      return true
    end

    # Choose column with minimum size
    col = choose_column(root)
    return false if col.nil?

    # If column has no rows, no solution possible
    if col.size == 0
      return false
    end

    # Cover this column
    cover_column(col)

    # Try each row in this column
    row_node = col.down
    while row_node != col
      # Add row to partial solution
      solution[k] = row_node.not_nil!.row_id

      # Cover all other columns in this row
      right_node = row_node.not_nil!.right
      while right_node != row_node
        cover_column(right_node.not_nil!.column.not_nil!)
        right_node = right_node.not_nil!.right
      end

      # Recurse
      if dlx_search(root, k + 1, solution)
        return true  # Solution found
      end

      # Backtrack: uncover all columns in this row
      left_node = row_node.not_nil!.left
      while left_node != row_node
        uncover_column(left_node.not_nil!.column.not_nil!)
        left_node = left_node.not_nil!.left
      end

      row_node = row_node.not_nil!.down
    end

    # Uncover column
    uncover_column(col)

    false  # No solution found
  end

  # Build the exact cover matrix for Sudoku
  def self.build_sudoku_matrix(puzzle : Array(Array(Int32))) : DlxNode
    # Create root node
    root = DlxNode.new

    # Create 324 column headers (constraints)
    # 81 row-column + 81 row-number + 81 column-number + 81 box-number
    columns = Array(DlxNode).new(324) { DlxNode.new }

    # Initialize column headers
    324.times do |i|
      columns[i].size = 0
      columns[i].col_id = i
      columns[i].column = columns[i]
      columns[i].up = columns[i]
      columns[i].down = columns[i]
    end

    # Link column headers horizontally
    root.left = root
    root.right = root
    324.times do |i|
      col = columns[i]
      col.left = root.left
      col.right = root
      root.left.not_nil!.right = col
      root.left = col
    end

    # Initialize row metadata storage
    @@row_info = Array(RowInfo).new
    @@row_starts = Array(DlxNode?).new

    # Create row nodes for each possible placement
    row_id = 0
    9.times do |row|
      9.times do |col|
        # If cell has a given clue, only create row for that digit
        if puzzle[row][col] != 0
          digit = puzzle[row][col]
          create_row(columns, row, col, digit, row_id)
          row_id += 1
        else
          # Empty cell - create rows for all possible digits
          (1..9).each do |digit|
            create_row(columns, row, col, digit, row_id)
            row_id += 1
          end
        end
      end
    end

    root
  end

  # Create a single row in the exact cover matrix
  def self.create_row(columns : Array(DlxNode), row : Int32, col : Int32, digit : Int32, row_id : Int32)
    # Store row metadata
    @@row_info << RowInfo.new(row, col, digit)

    # Four constraints per placement:
    # 1. Cell (row, col) must be filled
    # 2. Row must have digit
    # 3. Column must have digit
    # 4. Box must have digit

    box = (row // 3) * 3 + (col // 3)

    constraint_indices = [
      row * 9 + col,              # Cell constraint
      81 + row * 9 + (digit - 1), # Row-digit constraint
      162 + col * 9 + (digit - 1), # Column-digit constraint
      243 + box * 9 + (digit - 1)  # Box-digit constraint
    ]

    # Create 4 nodes for this row
    nodes = Array(DlxNode).new(4) { DlxNode.new }

    4.times do |i|
      node = nodes[i]
      node.row_id = row_id
      col_idx = constraint_indices[i]
      col_node = columns[col_idx]

      node.column = col_node
      node.col_id = col_idx

      # Link vertically into column
      node.up = col_node.up
      node.down = col_node
      col_node.up.not_nil!.down = node
      col_node.up = node
      col_node.size += 1
    end

    # Link nodes horizontally into a circular list
    4.times do |i|
      nodes[i].left = nodes[(i - 1) % 4]
      nodes[i].right = nodes[(i + 1) % 4]
    end

    # Store first node for this row
    @@row_starts << nodes[0]
  end

  # Cover given clues (pre-selected rows)
  def self.cover_clues(puzzle : Array(Array(Int32)))
    9.times do |r|
      9.times do |c|
        if puzzle[r][c] != 0
          n = puzzle[r][c]

          # Find the row for this clue
          @@row_info.each_with_index do |info, row_id|
            if info.row == r && info.col == c && info.num == n
              # Cover all columns in this row
              node = @@row_starts[row_id]
              next if node.nil?

              curr = node
              loop do
                cover_column(curr.not_nil!.column.not_nil!)
                curr = curr.not_nil!.right
                break if curr == node
              end
              break
            end
          end
        end
      end
    end
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
  def self.print_puzzle(puzzle : Array(Array(Int32)))
    puts "\nPuzzle:"
    9.times do |row|
      9.times do |col|
        print "#{puzzle[row][col]} "
      end
      puts ""
    end
  end

  # Extract solution from DLX solution array
  def self.extract_solution(solution : Array(Int32), puzzle : Array(Array(Int32))) : Array(Array(Int32))
    result = Array(Array(Int32)).new(9) { Array(Int32).new(9, 0) }

    # Copy initial puzzle (includes clues)
    9.times do |row|
      9.times do |col|
        result[row][col] = puzzle[row][col]
      end
    end

    # Extract placements from solution
    solution.each do |row_id|
      break if row_id < 0
      next if row_id >= @@row_info.size

      info = @@row_info[row_id]
      result[info.row][info.col] = info.num
    end

    result
  end

  # Main solving function
  def self.solve_puzzle(filename : String)
    puzzle = parse_matrix_file(filename)
    print_puzzle(puzzle)

    # Build DLX matrix
    root = build_sudoku_matrix(puzzle)

    # Cover pre-filled clues
    cover_clues(puzzle)

    # Solve
    solution = Array(Int32).new(81, -1)
    @@dlx_iterations = 0

    if dlx_search(root, 0, solution)
      solved = extract_solution(solution, puzzle)
      print_puzzle(solved)
      puts "\nSolved in Iterations=#{@@dlx_iterations}\n"
    else
      puts "No solution found"
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

DLX.main
