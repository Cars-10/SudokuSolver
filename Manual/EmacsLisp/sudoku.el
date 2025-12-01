;; Sudoku Solver in Emacs Lisp

(defvar sudoku-board (make-vector 9 nil))
(defvar sudoku-iterations 0)

(defun sudoku-init-board ()
  (dotimes (i 9)
    (aset sudoku-board i (make-vector 9 0))))

(defun sudoku-read-matrix (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (goto-char (point-min))
    (let ((row 0))
      (while (and (< row 9) (not (eobp)))
        (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
          (unless (or (string-empty-p line) (string-prefix-p "#" line))
            (let ((col 0)
                  (i 0))
              (while (and (< col 9) (< i (length line)))
                (let ((char (aref line i)))
                  (cond
                   ((and (>= char ?0) (<= char ?9))
                    (aset (aref sudoku-board row) col (- char ?0))
                    (setq col (1+ col)))
                   ((= char ?.)
                    (aset (aref sudoku-board row) col 0)
                    (setq col (1+ col)))))
                (setq i (1+ i)))
              (setq row (1+ row)))))
        (forward-line 1)))))

(defun sudoku-print-board ()
  (dotimes (r 9)
    (dotimes (c 9)
      (princ (format "%d " (aref (aref sudoku-board r) c))))
    (princ "\n")))

(defun sudoku-is-valid (row col num)
  (catch 'invalid
    ;; Row check
    (dotimes (c 9)
      (when (= (aref (aref sudoku-board row) c) num)
        (throw 'invalid nil)))
    
    ;; Col check
    (dotimes (r 9)
      (when (= (aref (aref sudoku-board r) col) num)
        (throw 'invalid nil)))
    
    ;; Box check
    (let ((box-row (* (/ row 3) 3))
          (box-col (* (/ col 3) 3)))
      (dotimes (r 3)
        (dotimes (c 3)
          (when (= (aref (aref sudoku-board (+ box-row r)) (+ box-col c)) num)
            (throw 'invalid nil)))))
    t))

(defun sudoku-find-empty ()
  (catch 'found
    (dotimes (r 9)
      (dotimes (c 9)
        (when (= (aref (aref sudoku-board r) c) 0)
          (throw 'found (list r c)))))
    nil))

(defun sudoku-solve ()
  (let ((empty (sudoku-find-empty)))
    (unless empty
      (throw 'solved t))
    
    (let ((row (nth 0 empty))
          (col (nth 1 empty)))
      (dotimes (i 9)
        (let ((num (1+ i)))
          (setq sudoku-iterations (1+ sudoku-iterations))
          (when (sudoku-is-valid row col num)
            (aset (aref sudoku-board row) col num)
            (catch 'solved
              (when (sudoku-solve)
                (throw 'solved t)))
            (aset (aref sudoku-board row) col 0))))
      nil)))

(defun sudoku-main ()
  (let ((args command-line-args-left))
    (if (< (length args) 1)
        (princ "Usage: emacs --script sudoku.el <matrix_file>\n")
      (let ((filename (car args)))
        (sudoku-init-board)
        (sudoku-read-matrix filename)
        (princ "Puzzle:\n")
        (sudoku-print-board)
        
        (if (catch 'solved (sudoku-solve))
            (progn
              (princ "Puzzle:\n")
              (sudoku-print-board)
              (princ (format "Solved in Iterations=%d\n" sudoku-iterations)))
          (princ "No solution found.\n"))))))

(sudoku-main)
