;; Constraint Propagation Sudoku Solver - Emacs Lisp Implementation
;; Uses bitsets for candidate tracking and constraint propagation

(defvar cp-iterations 0)
(defvar cp-grid nil)
(defvar cp-candidates nil)

;; Grid is a flat vector of 81 cells (0 = empty)
;; Candidates is a flat vector of 81 bitsets (bits 1-9 for each digit)
;; Bitset: bit N is set if digit N is a candidate (bit 0 unused)

(defun cp-make-grid ()
  "Create a new empty grid."
  (make-vector 81 0))

(defun cp-make-candidates ()
  "Create a new candidates array with all bits set."
  (let ((cands (make-vector 81 0)))
    (dotimes (i 81)
      (aset cands i #b1111111110))  ; bits 1-9 set
    cands))

(defun cp-index (r c)
  "Convert (row, col) to flat index."
  (+ (* r 9) c))

(defun cp-get-grid (grid r c)
  "Get cell value at (r,c)."
  (aref grid (cp-index r c)))

(defun cp-set-grid (grid r c val)
  "Set cell value at (r,c)."
  (aset grid (cp-index r c) val))

(defun cp-get-cands (cands r c)
  "Get candidates at (r,c)."
  (aref cands (cp-index r c)))

(defun cp-set-cands (cands r c val)
  "Set candidates at (r,c)."
  (aset cands (cp-index r c) val))

;; Bitset operations
(defun cp-has-candidate (cands digit)
  "Check if digit is a candidate in bitset."
  (not (zerop (logand cands (ash 1 digit)))))

(defun cp-remove-candidate (cands digit)
  "Remove digit from candidate bitset."
  (logand cands (lognot (ash 1 digit))))

(defun cp-count-candidates (cands)
  "Count number of set bits in candidate bitset."
  (let ((count 0)
        (bits cands))
    (dotimes (d 10)
      (when (not (zerop (logand bits 1)))
        (setq count (1+ count)))
      (setq bits (ash bits -1)))
    count))

(defun cp-first-candidate (cands)
  "Get first set bit (candidate digit) in bitset."
  (catch 'found
    (dotimes (d 10)
      (when (cp-has-candidate cands d)
        (throw 'found d)))
    nil))

(defun cp-get-all-candidates (cands)
  "Get list of all candidate digits from bitset."
  (let ((result nil))
    (dotimes (d 10)
      (when (cp-has-candidate cands d)
        (setq result (cons d result))))
    (nreverse result)))

;; Get peers (all cells in same row, column, or box)
(defun cp-get-peers (r c)
  "Get list of all peer cells for (r,c)."
  (let ((peers nil))
    ;; Row peers
    (dotimes (col 9)
      (unless (= col c)
        (setq peers (cons (list r col) peers))))
    ;; Column peers
    (dotimes (row 9)
      (unless (= row r)
        (setq peers (cons (list row c) peers))))
    ;; Box peers
    (let ((box-row (* (/ r 3) 3))
          (box-col (* (/ c 3) 3)))
      (dotimes (dr 3)
        (dotimes (dc 3)
          (let ((pr (+ box-row dr))
                (pc (+ box-col dc)))
            (unless (or (= pr r) (= pc c))
              (setq peers (cons (list pr pc) peers)))))))
    peers))

;; Eliminate a digit from a cell
(defun cp-eliminate (grid cands r c digit)
  "Eliminate digit from candidates at (r,c). Returns t on success, nil on contradiction."
  (let ((cell-cands (cp-get-cands cands r c)))
    (if (not (cp-has-candidate cell-cands digit))
        t  ; Already eliminated
      (progn
        ;; Remove digit
        (let ((new-cands (cp-remove-candidate cell-cands digit)))
          (cp-set-cands cands r c new-cands)

          ;; Check for contradiction
          (let ((remaining (cp-count-candidates new-cands)))
            (cond
             ((= remaining 0) nil)  ; Contradiction
             ((= remaining 1)
              ;; Only one candidate left - assign it
              (let ((last-digit (cp-first-candidate new-cands)))
                (if (= (cp-get-grid grid r c) 0)
                    (cp-assign grid cands r c last-digit)
                  t)))
             (t t))))))))

;; Assign a digit to a cell
(defun cp-assign (grid cands r c digit)
  "Assign digit to cell at (r,c). Returns t on success, nil on contradiction."
  ;; Increment iteration counter
  (setq cp-iterations (1+ cp-iterations))

  ;; Set value
  (cp-set-grid grid r c digit)
  (cp-set-cands cands r c (ash 1 digit))

  ;; Eliminate digit from all peers
  (let ((peers (cp-get-peers r c))
        (success t))
    (while (and peers success)
      (let* ((peer (car peers))
             (pr (nth 0 peer))
             (pc (nth 1 peer)))
        (setq success (cp-eliminate grid cands pr pc digit))
        (setq peers (cdr peers))))
    success))

;; Initialize grid and candidates from puzzle
(defun cp-init-from-puzzle (puzzle)
  "Initialize grid and candidates from puzzle array."
  (setq cp-grid (cp-make-grid))
  (setq cp-candidates (cp-make-candidates))

  ;; Set given clues
  (let ((success t))
    (catch 'init-failed
      (dotimes (r 9)
        (dotimes (c 9)
          (let ((val (aref (aref puzzle r) c)))
            (when (not (= val 0))
              ;; Given clue - assign it
              (unless (cp-assign cp-grid cp-candidates r c val)
                (throw 'init-failed nil)))))))
    success))

;; Find cell with minimum remaining values (MRV heuristic)
(defun cp-find-mrv-cell ()
  "Find empty cell with minimum candidates. Returns (r c) or nil."
  (let ((best-r nil)
        (best-c nil)
        (min-cands 10))
    (dotimes (r 9)
      (dotimes (c 9)
        (when (= (cp-get-grid cp-grid r c) 0)
          (let ((num-cands (cp-count-candidates (cp-get-cands cp-candidates r c))))
            (when (< num-cands min-cands)
              (setq min-cands num-cands)
              (setq best-r r)
              (setq best-c c))))))
    (if best-r
        (list best-r best-c)
      nil)))

;; Copy grid and candidates for backtracking
(defun cp-copy-state ()
  "Create a copy of current grid and candidates."
  (list (vconcat cp-grid) (vconcat cp-candidates)))

(defun cp-restore-state (state)
  "Restore grid and candidates from saved state."
  (setq cp-grid (aref state 0))
  (setq cp-candidates (aref state 1)))

;; Check if solved
(defun cp-is-solved ()
  "Check if grid is completely filled."
  (catch 'not-solved
    (dotimes (i 81)
      (when (= (aref cp-grid i) 0)
        (throw 'not-solved nil)))
    t))

;; Search with backtracking
(defun cp-search ()
  "Search for solution using constraint propagation and backtracking."
  (if (cp-is-solved)
      t
    (let ((cell (cp-find-mrv-cell)))
      (if (null cell)
          nil  ; No empty cells but not solved - contradiction
        (let* ((r (nth 0 cell))
               (c (nth 1 cell))
               (cands-bits (cp-get-cands cp-candidates r c))
               (digits (cp-get-all-candidates cands-bits)))
          ;; Try each candidate digit
          (catch 'found
            (dolist (d digits)
              ;; Save state for backtracking
              (let ((saved-state (cp-copy-state)))
                ;; Try assigning this digit
                (when (cp-assign cp-grid cp-candidates r c d)
                  ;; Recurse
                  (when (cp-search)
                    (throw 'found t)))
                ;; Backtrack
                (cp-restore-state saved-state)))
            nil))))))

;; Read matrix file
(defun cp-read-matrix (filename)
  "Read Sudoku puzzle from matrix file."
  (let ((puzzle (make-vector 9 nil)))
    (dotimes (i 9)
      (aset puzzle i (make-vector 9 0)))

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
                      (aset (aref puzzle row) col (- char ?0))
                      (setq col (1+ col)))
                     ((= char ?.)
                      (aset (aref puzzle row) col 0)
                      (setq col (1+ col)))))
                  (setq i (1+ i)))
                (setq row (1+ row)))))
          (forward-line 1))))
    puzzle))

;; Print board
(defun cp-print-board (grid-vec)
  "Print Sudoku board from flat grid vector."
  (dotimes (r 9)
    (dotimes (c 9)
      (princ (format "%d " (aref grid-vec (cp-index r c)))))
    (princ "\n")))

(defun cp-print-board-2d (puzzle)
  "Print Sudoku board from 2D puzzle array."
  (dotimes (r 9)
    (dotimes (c 9)
      (princ (format "%d " (aref (aref puzzle r) c))))
    (princ "\n")))

;; Main entry point
(let ((start-time (current-time)))
  (let ((args command-line-args-left))
    (if (< (length args) 1)
        (princ "Usage: emacs --batch --script cp.el <matrix_file>\n")
      (let ((filename (car args)))
        (princ (format "%s\n" filename))
        (let ((puzzle (cp-read-matrix filename)))

          ;; Print input puzzle
          (cp-print-board-2d puzzle)
          (princ "Puzzle:\n")
          (cp-print-board-2d puzzle)

          ;; Initialize
          (setq cp-iterations 0)
          (cp-init-from-puzzle puzzle)

          ;; Solve
          (if (cp-search)
              (progn
                (princ "\nPuzzle:\n")
                (cp-print-board cp-grid)
                (princ (format "\nSolved in Iterations=%d\n\n" cp-iterations)))
            (princ "No solution found.\n"))))))
  (let ((end-time (current-time)))
    (princ (format "Seconds to process %f\n" (float-time (time-subtract end-time start-time))))))
