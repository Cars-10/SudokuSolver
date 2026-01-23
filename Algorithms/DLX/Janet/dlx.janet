# Dancing Links (DLX) Sudoku Solver - Janet Implementation
# Algorithm: Knuth's Algorithm X with Dancing Links data structure

(var dlx-iterations 0)
(var puzzle @[])
(var solution-grid @[])

# DLX Node structure
(defn make-node []
  @{:left nil
    :right nil
    :up nil
    :down nil
    :column nil
    :row-id -1})

# DLX Column structure
(defn make-column [name]
  (def node (make-node))
  (def col @{:node node
             :size 0
             :name name})
  (put node :column col)
  col)

# Row info structure
(defn make-row-info []
  @{:row 0 :col 0 :num 0})

# Global DLX structures
(var root nil)
(var columns @[])
(var nodes @[])
(var row-info @[])
(var row-starts @[])

# Constraint column calculation
(defn get-position-col [r c]
  (+ (* r 9) c))

(defn get-row-col [r n]
  (+ 81 (* r 9) (- n 1)))

(defn get-col-col [c n]
  (+ 162 (* c 9) (- n 1)))

(defn get-box-col [r c n]
  (def box (+ (* (div r 3) 3) (div c 3)))
  (+ 243 (* box 9) (- n 1)))

# Cover column operation
(defn dlx-cover-column [col]
  (def col-node (get col :node))

  # Remove column header from header list
  (put (get col-node :right) :left (get col-node :left))
  (put (get col-node :left) :right (get col-node :right))

  # For each row in this column
  (var row-node (get col-node :down))
  (while (not= row-node col-node)
    # For each node in this row
    (var right-node (get row-node :right))
    (while (not= right-node row-node)
      # Remove node from its column
      (put (get right-node :down) :up (get right-node :up))
      (put (get right-node :up) :down (get right-node :down))
      (def node-col (get right-node :column))
      (put node-col :size (- (get node-col :size) 1))
      (set right-node (get right-node :right)))
    (set row-node (get row-node :down))))

# Uncover column operation
(defn dlx-uncover-column [col]
  (def col-node (get col :node))

  # For each row in this column (reverse order)
  (var row-node (get col-node :up))
  (while (not= row-node col-node)
    # For each node in this row (reverse order)
    (var left-node (get row-node :left))
    (while (not= left-node row-node)
      # Restore node to its column
      (def node-col (get left-node :column))
      (put node-col :size (+ (get node-col :size) 1))
      (put (get left-node :down) :up left-node)
      (put (get left-node :up) :down left-node)
      (set left-node (get left-node :left)))
    (set row-node (get row-node :up)))

  # Restore column header
  (put (get col-node :right) :left col-node)
  (put (get col-node :left) :right col-node))

# Choose column with minimum size
(defn choose-column [root-col]
  (def root-node (get root-col :node))
  (var best nil)
  (var min-size math/inf)

  (var col-node (get root-node :right))
  (while (not= col-node root-node)
    (def col (get col-node :column))
    (when (< (get col :size) min-size)
      (set min-size (get col :size))
      (set best col))
    (set col-node (get col-node :right)))

  best)

# DLX search with backtracking
(defn dlx-search [root-col k solution]
  (++ dlx-iterations)

  (def root-node (get root-col :node))

  # If matrix is empty, solution found
  (when (= (get root-node :right) root-node)
    (break true))

  # Choose column with minimum size
  (def col (choose-column root-col))

  # If column has no rows, no solution
  (when (= (get col :size) 0)
    (break false))

  # Cover this column
  (dlx-cover-column col)

  # Try each row in this column
  (var row-node (get (get col :node) :down))
  (var found false)
  (while (and (not found) (not= row-node (get col :node)))
    # Add row to solution
    (put solution k (get row-node :row-id))

    # Cover all other columns in this row
    (var right-node (get row-node :right))
    (while (not= right-node row-node)
      (dlx-cover-column (get right-node :column))
      (set right-node (get right-node :right)))

    # Recurse
    (when (dlx-search root-col (+ k 1) solution)
      (set found true)
      (break))

    # Backtrack: uncover columns
    (var left-node (get row-node :left))
    (while (not= left-node row-node)
      (dlx-uncover-column (get left-node :column))
      (set left-node (get left-node :left)))

    (set row-node (get row-node :down)))

  # Uncover column
  (dlx-uncover-column col)

  found)

# Initialize DLX matrix
(defn init-dlx-matrix []
  # Create root column
  (set root (make-column "root"))
  (def root-node (get root :node))
  (put root-node :left root-node)
  (put root-node :right root-node)
  (put root-node :up root-node)
  (put root-node :down root-node)
  (put root-node :row-id -1)

  # Create 324 column headers
  (set columns @[])
  (for i 0 324
    (def col (make-column (string "C" i)))
    (def node (get col :node))

    # Initialize as circular list
    (put node :up node)
    (put node :down node)
    (put node :row-id -1)

    # Link into header list
    (put node :left (get root-node :left))
    (put node :right root-node)
    (put (get root-node :left) :right node)
    (put root-node :left node)

    (array/push columns col))

  # Initialize other structures
  (set nodes @[])
  (set row-info @[])
  (set row-starts @[])
  (for i 0 729
    (array/push row-info (make-row-info))
    (array/push row-starts nil)))

# Add node to DLX matrix
(defn add-node [col row-id]
  (def node (make-node))
  (put node :column col)
  (put node :row-id row-id)

  # Insert at end of column's circular list
  (def col-node (get col :node))
  (put node :down col-node)
  (put node :up (get col-node :up))
  (put (get col-node :up) :down node)
  (put col-node :up node)
  (put col :size (+ (get col :size) 1))

  (array/push nodes node)
  node)

# Build DLX row for cell (r,c) with value n
(defn build-dlx-row [r c n row-id]
  # Store row metadata
  (put (get row-info row-id) :row r)
  (put (get row-info row-id) :col c)
  (put (get row-info row-id) :num n)

  # Create nodes for 4 constraints
  (def n1 (add-node (get columns (get-position-col r c)) row-id))
  (def n2 (add-node (get columns (get-row-col r n)) row-id))
  (def n3 (add-node (get columns (get-col-col c n)) row-id))
  (def n4 (add-node (get columns (get-box-col r c n)) row-id))

  # Link nodes horizontally in circular list
  (put n1 :right n2)
  (put n2 :right n3)
  (put n3 :right n4)
  (put n4 :right n1)

  (put n1 :left n4)
  (put n2 :left n1)
  (put n3 :left n2)
  (put n4 :left n3)

  # Store first node for this row
  (put row-starts row-id n1))

# Build DLX matrix from puzzle
(defn build-dlx-matrix []
  (var row-id 0)

  (for r 0 9
    (for c 0 9
      (if (not= (get-in puzzle [r c]) 0)
        # Cell has clue - create one row
        (do
          (build-dlx-row r c (get-in puzzle [r c]) row-id)
          (++ row-id))
        # Cell is empty - create rows for all values
        (do
          (for n 1 10
            (build-dlx-row r c n row-id)
            (++ row-id)))))))

# Cover given clues
(defn cover-clues []
  (for r 0 9
    (for c 0 9
      (when (not= (get-in puzzle [r c]) 0)
        (def n (get-in puzzle [r c]))

        # Find row for this clue
        (for row-id 0 729
          (def row-start (get row-starts row-id))
          (def info (get row-info row-id))
          (when (and row-start
                     (= (get info :row) r)
                     (= (get info :col) c)
                     (= (get info :num) n))
            # Cover all columns in this row
            (var node row-start)
            (var first-node node)
            (while true
              (dlx-cover-column (get node :column))
              (set node (get node :right))
              (when (= node first-node) (break)))
            (break)))))))

# Extract solution from DLX
(defn extract-solution [solution]
  # Start with original puzzle
  (set solution-grid @[])
  (each row puzzle
    (array/push solution-grid (array ;row)))

  # Fill in solution
  (each row-id solution
    (when (and (>= row-id 0) (< row-id 729))
      (def info (get row-info row-id))
      (put-in solution-grid [(get info :row) (get info :col)] (get info :num)))))

# Print puzzle
(defn print-puzzle []
  (print "\nPuzzle:")
  (each row solution-grid
    (var line "")
    (each val row
      (set line (string line val " ")))
    (print line)))

# Read matrix file
(defn read-matrix-file [filename]
  (try
    (do
      (def content (slurp filename))
      (def lines (string/split "\n" content))

      # Normalize path
      (if (string/has-prefix? "/app/Matrices/" filename)
        (print (string "../" (string/slice filename 5)))
        (print filename))

      (set puzzle @[])
      (var line-count 0)
      (each line lines
        (def trimmed (string/trim line))
        (when (and (not (empty? trimmed)) (not (string/has-prefix? "#" trimmed)))
          (def parts (filter (fn [x] (not (empty? x))) (string/split " " trimmed)))
          (when (and (= (length parts) 9) (< line-count 9))
            (def row @[])
            (var line-out "")
            (each part parts
              (def val (scan-number part))
              (array/push row val)
              (set line-out (string line-out val " ")))
            (array/push puzzle row)
            (print line-out)
            (++ line-count))))
      true)
    ([err]
      (print "Error reading file: " err)
      false)))

# Main function
(defn main [& args]
  (def start (os/clock))

  (each arg (tuple/slice args 1)
    (when (string/has-suffix? ".matrix" arg)
      (when (read-matrix-file arg)
        (print-puzzle)

        # Initialize and build DLX matrix
        (init-dlx-matrix)
        (build-dlx-matrix)
        (cover-clues)

        # Solve
        (set dlx-iterations 0)
        (def solution @[])
        (for i 0 81 (array/push solution -1))

        (if (dlx-search root 0 solution)
          (do
            (extract-solution solution)
            (print-puzzle)
            (print "\nSolved in Iterations=" dlx-iterations "\n"))
          (print "\nNo solution found\n")))))

  (def elapsed (- (os/clock) start))
  (print "Seconds to process " elapsed))
