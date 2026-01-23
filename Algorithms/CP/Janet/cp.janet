# Constraint Propagation (CP) Sudoku Solver - Janet Implementation (Simplified)
# Algorithm: Basic constraint propagation with backtracking

(var cp-iterations 0)
(var puzzle @[])
(var grid @[])

# Check if value is valid at position
(defn is-valid [g row col val]
  # Check row
  (var valid true)
  (for i 0 9
    (when (= (get-in g [row i]) val)
      (set valid false)
      (break)))

  (when valid
    # Check column
    (for i 0 9
      (when (= (get-in g [i col]) val)
        (set valid false)
        (break))))

  (when valid
    # Check 3x3 box
    (def box-row (* (div row 3) 3))
    (def box-col (* (div col 3) 3))
    (for i 0 3
      (for j 0 3
        (when (= (get-in g [(+ box-row i) (+ box-col j)]) val)
          (set valid false)
          (break)))
      (when (not valid) (break))))

  valid)

# Get possible values for a cell using constraint propagation
(defn get-candidates [g row col]
  (if (not= (get-in g [row col]) 0)
    @[]
    (do
      (def candidates @[])
      (for val 1 10
        (when (is-valid g row col val)
          (array/push candidates val)))
      candidates)))

# Find cell with minimum remaining values (MRV heuristic)
(defn find-mrv-cell [g]
  (var min-candidates 10)
  (var best-row -1)
  (var best-col -1)
  (var found false)

  (for r 0 9
    (for c 0 9
      (when (= (get-in g [r c]) 0)
        (def candidates (get-candidates g r c))
        (def num-candidates (length candidates))
        (when (< num-candidates min-candidates)
          (set min-candidates num-candidates)
          (set best-row r)
          (set best-col c)
          (set found true)))))

  (if found [best-row best-col] nil))

# Propagate naked singles (cells with only one candidate)
(defn propagate-naked-singles [g]
  (var changed true)
  (var iterations 0)

  (while (and changed (< iterations 100))
    (set changed false)
    (++ iterations)

    (for r 0 9
      (for c 0 9
        (when (= (get-in g [r c]) 0)
          (def candidates (get-candidates g r c))
          (when (= (length candidates) 1)
            (put-in g [r c] (get candidates 0))
            (set changed true))
          (when (= (length candidates) 0)
            # Contradiction - no candidates
            (break false)))))

    (when (= changed false)
      (break true)))

  true)

# Solve with constraint propagation and backtracking
(defn solve [g]
  (++ cp-iterations)

  # Propagate constraints
  (when (not (propagate-naked-singles g))
    (break false))

  # Find empty cell with MRV
  (def mrv-cell (find-mrv-cell g))
  (when (nil? mrv-cell)
    # No empty cells - solved
    (break true))

  (def [row col] mrv-cell)
  (def candidates (get-candidates g row col))

  # If no candidates, this path is invalid
  (when (= (length candidates) 0)
    (break false))

  # Try each candidate with backtracking
  (var found false)
  (each val candidates
    (# Save current grid state
     def g-backup (array ;(map (fn [r] (array ;r)) g)))

    # Try this value
    (put-in g [row col] val)

    (if (solve g)
      (do
        (set found true)
        (break))
      # Backtrack - restore grid state
      (do
        (for r 0 9
          (for c 0 9
            (put-in g [r c] (get-in g-backup [r c])))))))

  found)

# Print puzzle
(defn print-puzzle [g]
  (print "\nPuzzle:")
  (each row g
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
        (print-puzzle puzzle)

        # Copy puzzle to working grid
        (set grid (array ;(map (fn [row] (array ;row)) puzzle)))

        # Solve
        (set cp-iterations 0)

        (if (solve grid)
          (do
            (print-puzzle grid)
            (print "\nSolved in Iterations=" cp-iterations "\n"))
          (print "\nNo solution found\n")))))

  (def elapsed (- (os/clock) start))
  (print "Seconds to process " elapsed))
