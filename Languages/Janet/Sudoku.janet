(var puzzle @[])
(var count 0)

(defn print-puzzle []
  (print "\nPuzzle:")
  (each row puzzle
    (var line "")
    (each col row
      (set line (string line col " ")))
    (print (string line))))

(defn is-valid [row col val]
  # Check row
  (var valid true)
  (for i 0 9
    (when (= (get-in puzzle [row i]) val)
      (set valid false)
      (break)))
  
  (when valid
    # Check column
    (for i 0 9
      (when (= (get-in puzzle [i col]) val)
        (set valid false)
        (break))))
  
  (when valid
    # Check 3x3 box
    (def box-row (* (div row 3) 3))
    (def box-col (* (div col 3) 3))
    (for i 0 3
      (for j 0 3
        (when (= (get-in puzzle [(+ box-row i) (+ box-col j)]) val)
          (set valid false)
          (break)))))
  valid)

(defn solve []
  # Find empty cell (row-major)
  (var row -1)
  (var col -1)
  (var found-empty false)
  
  (for r 0 9
    (for c 0 9
      (when (= (get-in puzzle [r c]) 0)
        (set row r)
        (set col c)
        (set found-empty true)
        (break)))
    (when found-empty (break)))

  (if (not found-empty)
    (do
      (print-puzzle)
      (print "\nSolved in Iterations=" count "\n")
      true)
    (do
      (var result false)
      (for val 1 10
        (++ count)
        (when (is-valid row col val)
          (put-in puzzle [row col] val)
          (if (solve)
            (do
              (set result true)
              (break)))
          (put-in puzzle [row col] 0)))
      result)))

(defn read-matrix-file [filename]
  (try
    (do
      (def content (slurp filename))
      (def lines (string/split "\n" content))
      
      # Normalize path for output
      (if (string/has-prefix? "/app/Matrices/" filename)
        (print (string "../" (string/slice filename 5)))
        (print filename))

      (var line-count 0)
      (each line lines
        (def trimmed (string/trim line))
        (when (and (not (empty? trimmed)) (not (string/has-prefix? "#" trimmed)))
          (def parts (filter (fn [x] (not (empty? x))) (string/split " " trimmed)))
          (when (= (length parts) 9)
            (if (< line-count 9)
              (do
                (def row @[])
                (var line-out "")
                (each part parts
                  (def val (scan-number part))
                  (array/push row val)
                  (set line-out (string line-out val " ")))
                (array/push puzzle row)
                (print line-out)
                (++ line-count))))))
      true)
    ([err]
      (print "Error reading file: " err)
      false)))

(defn main [& args]
  (def start (os/clock))
  (each arg (tuple/slice args 1)
    (set puzzle @[])
    (set count 0)
    (when (read-matrix-file arg)
      (print-puzzle)
      (solve)))
  (def end (os/clock))
  (print "Seconds to process " (- end start)))