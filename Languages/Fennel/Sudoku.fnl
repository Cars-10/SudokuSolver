(var iterations 0)
(var grid [])

(fn grid-get [r c]
  (. grid (+ (* (- r 1) 9) c)))

(fn grid-set [r c val]
  (tset grid (+ (* (- r 1) 9) c) val))

(fn print-grid []
  (for [r 1 9]
    (var line "")
    (for [c 1 9]
      (set line (.. line (grid-get r c) " ")))
    (print line)))

(fn is-valid [r c val]
  (var valid true)
  ;; Check row
  (for [i 1 9]
    (if (= (grid-get r i) val) (set valid false)))
  
  (if valid
      (for [i 1 9]
        (if (= (grid-get i c) val) (set valid false))))
  
  (if valid
      (let [br (+ (* (math.floor (/ (- r 1) 3)) 3) 1)
            bc (+ (* (math.floor (/ (- c 1) 3)) 3) 1)]
        (for [i 0 2]
          (for [j 0 2]
            (if (= (grid-get (+ br i) (+ bc j)) val) (set valid false))))))
  valid)

(fn solve []
  (var r -1)
  (var c -1)
  (var found false)
  
  ;; Find first empty cell
  (for [row 1 9]
    (if (not found)
        (for [col 1 9]
          (if (and (not found) (= (grid-get row col) 0))
              (do (set r row) (set c col) (set found true))))))
  
  (if (not found)
      true
      (do
        (var solved false)
        (for [val 1 9]
          (if (not solved)
              (do
                (set iterations (+ iterations 1))
                (if (is-valid r c val)
                    (do
                      (grid-set r c val)
                      (if (solve) (set solved true)
                          (grid-set r c 0)))))))
        solved)))

(fn read-matrix [filename]
  (let [f (io.open filename :r)]
    (if (not f)
        false
        (do
          (print filename)
          (var row 1)
          (each [line (f:lines)]
            (if (and (<= row 9) (> (length line) 0) (not (= (line:sub 1 1) "#")))
                (let [parts []]
                  (each [part (line:gmatch "%S+")]
                    (table.insert parts (tonumber part)))
                  (if (>= (length parts) 9)
                      (do
                        (for [col 1 9]
                          (grid-set row col (. parts col))
                          (io.write (. parts col) " "))
                        (print "")
                        (set row (+ row 1)))))))
          (f:close)
          (= row 10)))))

(fn main [args]
  (let [filename (. args 1)]
    (if (not filename)
        (do (print "Usage: Sudoku <matrix_file>") (os.exit 1))
        (do
          (set grid [])
          (for [i 1 81] (tset grid i 0))
          (if (read-matrix filename)
              (do
                (print "\nPuzzle:")
                (print-grid)
                (set iterations 0)
                (if (solve)
                    (do
                      (print "\nPuzzle:")
                      (print-grid)
                      (print (.. "\nSolved in Iterations=" iterations)))
                    (print "No solution found.")))
              (print (.. "Error reading matrix file: " filename)))))))

(main arg)
