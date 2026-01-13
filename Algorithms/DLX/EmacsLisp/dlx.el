;; Dancing Links (DLX) Sudoku Solver - Emacs Lisp Implementation
;; Implements Knuth's Algorithm X with Dancing Links

(defvar dlx-iterations 0)
(defvar dlx-node-pool nil)
(defvar dlx-root nil)
(defvar dlx-columns nil)
(defvar dlx-row-info nil)
(defvar dlx-puzzle nil)
(defvar dlx-solution nil)

;; Node structure: [left right up down column size row-id col-id]
(defun dlx-make-node (row-id col-id)
  "Create a new DLX node with row-id and col-id."
  (let ((node (make-vector 8 nil)))
    (aset node 6 row-id)
    (aset node 7 col-id)
    (aset node 5 0)  ; size
    node))

(defun dlx-node-left (node) (aref node 0))
(defun dlx-node-right (node) (aref node 1))
(defun dlx-node-up (node) (aref node 2))
(defun dlx-node-down (node) (aref node 3))
(defun dlx-node-column (node) (aref node 4))
(defun dlx-node-size (node) (aref node 5))
(defun dlx-node-row-id (node) (aref node 6))
(defun dlx-node-col-id (node) (aref node 7))

(defun dlx-set-left (node val) (aset node 0 val))
(defun dlx-set-right (node val) (aset node 1 val))
(defun dlx-set-up (node val) (aset node 2 val))
(defun dlx-set-down (node val) (aset node 3 val))
(defun dlx-set-column (node val) (aset node 4 val))
(defun dlx-set-size (node val) (aset node 5 val))

;; Constraint column indices
(defun dlx-position-col (r c)
  "Position constraint: cell (r,c) must be filled."
  (+ (* r 9) c))

(defun dlx-row-col (r n)
  "Row constraint: row r must have number n."
  (+ 81 (* r 9) (- n 1)))

(defun dlx-col-col (c n)
  "Column constraint: column c must have number n."
  (+ 162 (* c 9) (- n 1)))

(defun dlx-box-col (r c n)
  "Box constraint: box must have number n."
  (let ((box (+ (* (/ r 3) 3) (/ c 3))))
    (+ 243 (* box 9) (- n 1))))

;; Initialize DLX matrix
(defun dlx-init-matrix ()
  "Initialize the DLX matrix with root and column headers."
  (setq dlx-root (dlx-make-node -1 -1))
  (dlx-set-left dlx-root dlx-root)
  (dlx-set-right dlx-root dlx-root)
  (dlx-set-up dlx-root dlx-root)
  (dlx-set-down dlx-root dlx-root)
  (dlx-set-column dlx-root dlx-root)

  ;; Create 324 column headers
  (setq dlx-columns (make-vector 324 nil))
  (dotimes (i 324)
    (let ((col (dlx-make-node -1 i)))
      (dlx-set-column col col)
      (dlx-set-up col col)
      (dlx-set-down col col)
      (aset dlx-columns i col)))

  ;; Link columns in circular list with root
  (let ((prev dlx-root))
    (dotimes (i 324)
      (let ((col (aref dlx-columns i)))
        (dlx-set-right prev col)
        (dlx-set-left col prev)
        (setq prev col)))
    (dlx-set-right prev dlx-root)
    (dlx-set-left dlx-root prev)))

;; Add node to column
(defun dlx-add-node-to-column (node col)
  "Add node to the bottom of column's circular list."
  (dlx-set-column node col)
  (let ((col-up (dlx-node-up col)))
    (dlx-set-down col-up node)
    (dlx-set-up node col-up)
    (dlx-set-down node col)
    (dlx-set-up col node)
    (dlx-set-size col (1+ (dlx-node-size col)))))

;; Add a row to DLX matrix
(defun dlx-add-row (r c d)
  "Add a row representing placing digit d at (r,c)."
  (let* ((pos-col (dlx-position-col r c))
         (row-col-idx (dlx-row-col r d))
         (col-col-idx (dlx-col-col c d))
         (box-col-idx (dlx-box-col r c d))
         (row-id (+ (* r 81) (* c 9) (- d 1)))
         (nodes (list
                 (dlx-make-node row-id pos-col)
                 (dlx-make-node row-id row-col-idx)
                 (dlx-make-node row-id col-col-idx)
                 (dlx-make-node row-id box-col-idx))))

    ;; Link nodes in circular row list
    (let ((n0 (nth 0 nodes))
          (n1 (nth 1 nodes))
          (n2 (nth 2 nodes))
          (n3 (nth 3 nodes)))
      (dlx-set-right n0 n1)
      (dlx-set-right n1 n2)
      (dlx-set-right n2 n3)
      (dlx-set-right n3 n0)
      (dlx-set-left n0 n3)
      (dlx-set-left n1 n0)
      (dlx-set-left n2 n1)
      (dlx-set-left n3 n2))

    ;; Add each node to its column
    (dlx-add-node-to-column (nth 0 nodes) (aref dlx-columns pos-col))
    (dlx-add-node-to-column (nth 1 nodes) (aref dlx-columns row-col-idx))
    (dlx-add-node-to-column (nth 2 nodes) (aref dlx-columns col-col-idx))
    (dlx-add-node-to-column (nth 3 nodes) (aref dlx-columns box-col-idx))

    (cons r (cons c (cons d nil)))))

;; Build DLX matrix
(defun dlx-build-matrix ()
  "Build the DLX matrix for the Sudoku puzzle."
  (setq dlx-row-info nil)
  (let ((given-rows nil))
    (dotimes (r 9)
      (dotimes (c 9)
        (let ((val (aref (aref dlx-puzzle r) c)))
          (if (= val 0)
              ;; Empty cell: add rows for all possible digits
              (dotimes (d 9)
                (let ((info (dlx-add-row r c (+ d 1))))
                  (setq dlx-row-info (cons info dlx-row-info))))
            ;; Given clue: add only one row and mark for pre-covering
            (let ((info (dlx-add-row r c val)))
              (setq dlx-row-info (cons info dlx-row-info))
              (setq given-rows (cons (list r c val) given-rows)))))))

    ;; Pre-cover given clues
    (dolist (given given-rows)
      (let* ((r (nth 0 given))
             (c (nth 1 given))
             (d (nth 2 given))
             (row-id (+ (* r 81) (* c 9) (- d 1))))
        (dlx-pre-cover-row row-id)))))

;; Pre-cover a given clue
(defun dlx-pre-cover-row (row-id)
  "Pre-cover columns for a given clue row."
  ;; Find the first node with this row-id
  (catch 'found-node
    (dotimes (i 324)
      (let ((col (aref dlx-columns i)))
        (let ((node (dlx-node-down col)))
          (while (not (eq node col))
            (when (= (dlx-node-row-id node) row-id)
              ;; Found the row - add to solution and cover columns
              (setq dlx-solution (cons row-id dlx-solution))

              ;; Cover all columns in this row
              (let ((current node))
                (catch 'done-row
                  (while t
                    (dlx-cover (dlx-node-column current))
                    (setq current (dlx-node-right current))
                    (when (eq current node)
                      (throw 'done-row nil)))))

              (throw 'found-node nil))
            (setq node (dlx-node-down node))))))))

;; Cover column
(defun dlx-cover (col)
  "Cover a column in the DLX matrix."
  ;; Remove column header from header list
  (let ((col-left (dlx-node-left col))
        (col-right (dlx-node-right col)))
    (dlx-set-right col-left col-right)
    (dlx-set-left col-right col-left))

  ;; Remove all rows in this column
  (let ((row-node (dlx-node-down col)))
    (while (not (eq row-node col))
      ;; For each node in this row
      (let ((right-node (dlx-node-right row-node)))
        (while (not (eq right-node row-node))
          (let ((r-up (dlx-node-up right-node))
                (r-down (dlx-node-down right-node))
                (r-col (dlx-node-column right-node)))
            (dlx-set-down r-up r-down)
            (dlx-set-up r-down r-up)
            (dlx-set-size r-col (- (dlx-node-size r-col) 1)))
          (setq right-node (dlx-node-right right-node))))
      (setq row-node (dlx-node-down row-node)))))

;; Uncover column
(defun dlx-uncover (col)
  "Uncover a column (exact reverse of cover)."
  ;; Restore all rows in this column (in reverse order)
  (let ((row-node (dlx-node-up col)))
    (while (not (eq row-node col))
      ;; For each node in this row (in reverse order)
      (let ((left-node (dlx-node-left row-node)))
        (while (not (eq left-node row-node))
          (let ((l-up (dlx-node-up left-node))
                (l-down (dlx-node-down left-node))
                (l-col (dlx-node-column left-node)))
            (dlx-set-size l-col (+ (dlx-node-size l-col) 1))
            (dlx-set-down l-up left-node)
            (dlx-set-up l-down left-node))
          (setq left-node (dlx-node-left left-node))))
      (setq row-node (dlx-node-up row-node))))

  ;; Restore column header to header list
  (let ((col-left (dlx-node-left col))
        (col-right (dlx-node-right col)))
    (dlx-set-right col-left col)
    (dlx-set-left col-right col)))

;; Choose column with minimum size
(defun dlx-choose-column ()
  "Choose column with minimum size (Knuth's S heuristic)."
  (let ((current (dlx-node-right dlx-root)))
    (if (eq current dlx-root)
        nil
      (let ((best current)
            (min-size (dlx-node-size current)))
        (setq current (dlx-node-right current))
        (while (not (eq current dlx-root))
          (let ((size (dlx-node-size current)))
            (when (< size min-size)
              (setq best current)
              (setq min-size size)))
          (setq current (dlx-node-right current)))
        best))))

;; DLX search
(defun dlx-search ()
  "DLX search (Algorithm X) - returns t if solution found."
  ;; Increment iteration counter at start of search call
  (setq dlx-iterations (1+ dlx-iterations))

  ;; Check if matrix is empty (solution found)
  (if (eq (dlx-node-right dlx-root) dlx-root)
      t
    ;; Choose column with minimum size
    (let ((col (dlx-choose-column)))
      (if (or (null col) (= (dlx-node-size col) 0))
          nil
        (progn
          ;; Cover this column
          (dlx-cover col)

          ;; Try each row in this column
          (catch 'found
            (let ((row-node (dlx-node-down col)))
              (while (not (eq row-node col))

                ;; Add row to solution
                (let ((row-id (dlx-node-row-id row-node)))
                  (setq dlx-solution (cons row-id dlx-solution))

                  ;; Cover all other columns in this row
                  (let ((right-node (dlx-node-right row-node)))
                    (while (not (eq right-node row-node))
                      (dlx-cover (dlx-node-column right-node))
                      (setq right-node (dlx-node-right right-node))))

                  ;; Recurse
                  (when (dlx-search)
                    (throw 'found t))

                  ;; Backtrack: uncover columns in reverse
                  (let ((left-node (dlx-node-left row-node)))
                    (while (not (eq left-node row-node))
                      (dlx-uncover (dlx-node-column left-node))
                      (setq left-node (dlx-node-left left-node))))

                  ;; Remove row from solution
                  (setq dlx-solution (cdr dlx-solution)))

                (setq row-node (dlx-node-down row-node))))

            ;; No solution found
            (dlx-uncover col)
            nil))))))

;; Reconstruct solution from row IDs
(defun dlx-reconstruct-solution ()
  "Reconstruct Sudoku grid from solution row IDs."
  (let ((solution (make-vector 9 nil)))
    ;; Initialize solution grid with zeros
    (dotimes (r 9)
      (let ((row (make-vector 9 0)))
        (aset solution r row)))

    ;; Apply solution rows
    (dolist (row-id dlx-solution)
      (let* ((r (/ row-id 81))
             (remainder (mod row-id 81))
             (c (/ remainder 9))
             (d (+ (mod remainder 9) 1)))
        (aset (aref solution r) c d)))

    solution))

;; Read matrix file
(defun dlx-read-matrix (filename)
  "Read Sudoku puzzle from matrix file."
  (setq dlx-puzzle (make-vector 9 nil))
  (dotimes (i 9)
    (aset dlx-puzzle i (make-vector 9 0)))

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
                    (aset (aref dlx-puzzle row) col (- char ?0))
                    (setq col (1+ col)))
                   ((= char ?.)
                    (aset (aref dlx-puzzle row) col 0)
                    (setq col (1+ col)))))
                (setq i (1+ i)))
              (setq row (1+ row)))))
        (forward-line 1)))))

;; Print board
(defun dlx-print-board (board)
  "Print Sudoku board."
  (dotimes (r 9)
    (dotimes (c 9)
      (princ (format "%d " (aref (aref board r) c))))
    (princ "\n")))

;; Main entry point
(let ((start-time (current-time)))
  (let ((args command-line-args-left))
    (if (< (length args) 1)
        (princ "Usage: emacs --batch --script dlx.el <matrix_file>\n")
      (let ((filename (car args)))
        (princ (format "%s\n" filename))
        (dlx-read-matrix filename)

        ;; Print input puzzle
        (dlx-print-board dlx-puzzle)
        (princ "Puzzle:\n")
        (dlx-print-board dlx-puzzle)

        ;; Initialize DLX structures
        (setq dlx-iterations 0)
        (setq dlx-solution nil)
        (dlx-init-matrix)
        (dlx-build-matrix)

        ;; Solve
        (if (dlx-search)
            (let ((solution (dlx-reconstruct-solution)))
              (princ "\nPuzzle:\n")
              (dlx-print-board solution)
              (princ (format "\nSolved in Iterations=%d\n\n" dlx-iterations)))
          (princ "No solution found.\n")))))
  (let ((end-time (current-time)))
    (princ (format "Seconds to process %f\n" (float-time (time-subtract end-time start-time))))))
