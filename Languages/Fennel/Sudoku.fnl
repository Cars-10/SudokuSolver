(var puzzle {})
(var count 0)

(fn print-puzzle []
  (print "\nPuzzle:")
  (each [i row (pairs puzzle)]
    (var line "")
    (each [j col (pairs row)]
      (set line (.. line col " ")))
    (print line)))

(fn is-valid [row col val]
  (var valid true)
  (for [i 1 9 &until (not valid)]
    (when (= (. (. puzzle row) i) val)
      (set valid false)))
  
  (when valid
    (for [i 1 9 &until (not valid)]
      (when (= (. (. puzzle i) col) val)
        (set valid false))))
  
  (when valid
    (local box-row (+ (* (math.floor (/ (- row 1) 3)) 3) 1))
    (local box-col (+ (* (math.floor (/ (- col 1) 3)) 3) 1))
    (for [i 0 2 &until (not valid)]
      (for [j 0 2 &until (not valid)]
        (when (= (. (. puzzle (+ box-row i)) (+ box-col j)) val)
          (set valid false)))))
  valid)

(fn solve []
  (var row -1)
  (var col -1)
  (var found-empty false)
  
  (for [r 1 9 &until found-empty]
    (for [c 1 9 &until found-empty]
      (when (= (. (. puzzle r) c) 0)
        (set row r)
        (set col c)
        (set found-empty true))))
    
  (if (not found-empty)
    (do
      (print-puzzle)
      (print (.. "\nSolved in Iterations=" count "\n")))
    (do
      (var result false)
      (for [val 1 9 &until result]
        (set count (+ count 1))
        (when (is-valid row col val)
          (tset (. puzzle row) col val)
          (when (solve)
            (set result true))
          (when (not result)
            (tset (. puzzle row) col 0))))
      result)))

(fn read-matrix-file [filename]
  (let [f (io.open filename)]
    (when f
      (let [content (f:read "*a")]
        (f:close)
        (let [lines (-> content
                        (string.gsub "\r" "")
                        (string.split "\n"))]
          
          (if (string.match filename "^/app/Matrices/")
              (print (.. "../" (string.sub filename 6)))
              (print filename))
              
          (var line-count 1)
          (each [i line (ipairs lines)]
            (let [trimmed (string.gsub line "%s+" "")]
              (when (and (not (= trimmed "")) (not (= (string.sub line 1 1) "#")))
                (let [parts {}]
                  (each [part (string.gmatch line "%S+")]
                    (table.insert parts (tonumber part)))
                  (when (= (length parts) 9)
                    (tset puzzle line-count parts)
                    (print (table.concat parts " "))
                    (set line-count (+ line-count 1))))))))
      true)))

(local start (os.clock))
(each [i arg (ipairs arg)]
  (set puzzle {})
  (for [i 1 9] (tset puzzle i {}))
  (set count 0)
  (when (read-matrix-file arg)
    (print-puzzle)
    (solve)))
(local end (os.clock))
(print (.. "Seconds to process " (- end start)))