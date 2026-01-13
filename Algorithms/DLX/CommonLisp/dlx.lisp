#!/usr/bin/env sbcl --script
;;; Sudoku DLX Solver in Common Lisp (SBCL)
;;; Dancing Links (Algorithm X) implementation using mutable structs

;; ============================================================================
;; NODE REPRESENTATION USING DEFSTRUCT
;; ============================================================================

(defstruct dlx-node
  (left nil)
  (right nil)
  (up nil)
  (down nil)
  (column nil)
  (size 0 :type fixnum)
  (row-id -1 :type fixnum)
  (col-id -1 :type fixnum))

;; ============================================================================
;; GLOBAL DATA STRUCTURES
;; ============================================================================

(defvar *dlx-iterations* 0)
(defvar *puzzle* (make-array 81 :element-type 'fixnum :initial-element 0))
(defvar *root-node* nil)
(defvar *column-headers* (make-array 324 :initial-element nil))
(defvar *row-infos* (make-array 729 :initial-element nil))
(defvar *row-starts* (make-array 729 :initial-element nil))

;; Helper to get/set puzzle cells
(defun get-puzzle-cell (r c)
  (aref *puzzle* (+ (* r 9) c)))

(defun set-puzzle-cell (r c val)
  (setf (aref *puzzle* (+ (* r 9) c)) val))

;; ============================================================================
;; CONSTRAINT COLUMN CALCULATIONS
;; ============================================================================

(defun get-position-col (r c)
  (+ (* r 9) c))

(defun get-row-col (r n)
  (+ 81 (* r 9) (- n 1)))

(defun get-col-col (c n)
  (+ 162 (* c 9) (- n 1)))

(defun get-box-col (r c n)
  (let ((box (+ (* (floor r 3) 3) (floor c 3))))
    (+ 243 (* box 9) (- n 1))))

;; ============================================================================
;; DLX MATRIX INITIALIZATION
;; ============================================================================

(defun init-dlx-matrix ()
  ;; Create root node
  (setf *root-node* (make-dlx-node))
  (setf (dlx-node-left *root-node*) *root-node*)
  (setf (dlx-node-right *root-node*) *root-node*)
  (setf (dlx-node-up *root-node*) *root-node*)
  (setf (dlx-node-down *root-node*) *root-node*)
  (setf (dlx-node-column *root-node*) *root-node*)

  ;; Create 324 column headers
  (dotimes (i 324)
    (let ((col (make-dlx-node)))
      (setf (dlx-node-up col) col)
      (setf (dlx-node-down col) col)
      (setf (dlx-node-column col) col)
      (setf (dlx-node-col-id col) i)

      ;; Link into header list (insert before root)
      (let ((left-node (dlx-node-left *root-node*)))
        (setf (dlx-node-left col) left-node)
        (setf (dlx-node-right col) *root-node*)
        (setf (dlx-node-right left-node) col)
        (setf (dlx-node-left *root-node*) col))

      (setf (aref *column-headers* i) col))))

;; ============================================================================
;; NODE OPERATIONS
;; ============================================================================

(defun add-node-to-column (col row-id)
  (let ((node (make-dlx-node)))
    (setf (dlx-node-column node) col)
    (setf (dlx-node-row-id node) row-id)

    ;; Insert at end of column's circular list
    (let ((up-node (dlx-node-up col)))
      (setf (dlx-node-down node) col)
      (setf (dlx-node-up node) up-node)
      (setf (dlx-node-down up-node) node)
      (setf (dlx-node-up col) node))

    ;; Increment column size
    (incf (dlx-node-size col))
    node))

(defun build-dlx-row (r c n row-id)
  ;; Store row info
  (setf (aref *row-infos* row-id) (list r c n))

  ;; Create 4 nodes for the 4 constraints
  (let ((n1 (add-node-to-column (aref *column-headers* (get-position-col r c)) row-id))
        (n2 (add-node-to-column (aref *column-headers* (get-row-col r n)) row-id))
        (n3 (add-node-to-column (aref *column-headers* (get-col-col c n)) row-id))
        (n4 (add-node-to-column (aref *column-headers* (get-box-col r c n)) row-id)))

    ;; Link nodes horizontally in circular list
    (setf (dlx-node-right n1) n2)
    (setf (dlx-node-right n2) n3)
    (setf (dlx-node-right n3) n4)
    (setf (dlx-node-right n4) n1)

    (setf (dlx-node-left n1) n4)
    (setf (dlx-node-left n2) n1)
    (setf (dlx-node-left n3) n2)
    (setf (dlx-node-left n4) n3)

    ;; Store first node of this row
    (setf (aref *row-starts* row-id) n1)
    n1))

(defun build-dlx-matrix-from-puzzle ()
  (let ((row-id 0))
    (dotimes (r 9)
      (dotimes (c 9)
        (let ((val (get-puzzle-cell r c)))
          (if (= val 0)
              ;; Empty cell - create rows for all values 1-9
              (loop for n from 1 to 9 do
                    (build-dlx-row r c n row-id)
                    (incf row-id))
              ;; Clue - create only one row
              (progn
                (build-dlx-row r c val row-id)
                (incf row-id))))))))

;; ============================================================================
;; DLX COVER/UNCOVER OPERATIONS
;; ============================================================================

(defun cover-column (col)
  ;; Remove column from header list
  (let ((left-node (dlx-node-left col))
        (right-node (dlx-node-right col)))
    (setf (dlx-node-right left-node) right-node)
    (setf (dlx-node-left right-node) left-node))

  ;; For each row in this column
  (do ((row-node (dlx-node-down col) (dlx-node-down row-node)))
      ((eq row-node col))
    ;; For each node in this row
    (do ((right-node (dlx-node-right row-node) (dlx-node-right right-node)))
        ((eq right-node row-node))
      ;; Remove node from its column
      (let ((up-node (dlx-node-up right-node))
            (down-node (dlx-node-down right-node))
            (col-node (dlx-node-column right-node)))
        (setf (dlx-node-down up-node) down-node)
        (setf (dlx-node-up down-node) up-node)
        (decf (dlx-node-size col-node))))))

(defun uncover-column (col)
  ;; For each row in this column (in reverse)
  (do ((row-node (dlx-node-up col) (dlx-node-up row-node)))
      ((eq row-node col))
    ;; For each node in this row (in reverse)
    (do ((left-node (dlx-node-left row-node) (dlx-node-left left-node)))
        ((eq left-node row-node))
      ;; Restore node to its column
      (let ((up-node (dlx-node-up left-node))
            (down-node (dlx-node-down left-node))
            (col-node (dlx-node-column left-node)))
        (incf (dlx-node-size col-node))
        (setf (dlx-node-down up-node) left-node)
        (setf (dlx-node-up down-node) left-node))))

  ;; Restore column to header list
  (let ((left-node (dlx-node-left col))
        (right-node (dlx-node-right col)))
    (setf (dlx-node-right left-node) col)
    (setf (dlx-node-left right-node) col)))

;; ============================================================================
;; DLX SEARCH (ALGORITHM X)
;; ============================================================================

(defun choose-column ()
  ;; Choose column with minimum size (Knuth's S heuristic)
  (let ((best nil)
        (min-size 999999))
    (do ((col-node (dlx-node-right *root-node*) (dlx-node-right col-node)))
        ((eq col-node *root-node*))
      (let ((size (dlx-node-size col-node)))
        (when (< size min-size)
          (setf min-size size)
          (setf best col-node))))
    best))

(defun dlx-search (solution depth)
  (incf *dlx-iterations*)

  ;; If matrix is empty, we found a solution
  (if (eq (dlx-node-right *root-node*) *root-node*)
      t
      (let ((col (choose-column)))
        (if (or (not col) (= (dlx-node-size col) 0))
            nil
            (progn
              ;; Cover this column
              (cover-column col)

              ;; Try each row in this column
              (do ((row-node (dlx-node-down col) (dlx-node-down row-node)))
                  ((eq row-node col)
                   ;; No solution found, uncover and backtrack
                   (uncover-column col)
                   nil)
                ;; Add row to solution
                (setf (aref solution depth) (dlx-node-row-id row-node))

                ;; Cover all other columns in this row
                (do ((right-node (dlx-node-right row-node) (dlx-node-right right-node)))
                    ((eq right-node row-node))
                  (cover-column (dlx-node-column right-node)))

                ;; Recurse
                (when (dlx-search solution (+ depth 1))
                  (return-from dlx-search t))

                ;; Backtrack: uncover columns in reverse
                (do ((left-node (dlx-node-left row-node) (dlx-node-left left-node)))
                    ((eq left-node row-node))
                  (uncover-column (dlx-node-column left-node)))))))))

;; ============================================================================
;; PUZZLE I/O
;; ============================================================================

(defun print-puzzle-grid (grid)
  (format t "~%Puzzle:~%")
  (dotimes (r 9)
    (dotimes (c 9)
      (format t "~D " (aref grid (+ (* r 9) c))))
    (format t "~%")))

(defun split-string (string delimiter)
  "Split STRING by DELIMITER character."
  (let ((result '())
        (start 0))
    (loop for i from 0 to (length string)
          do (when (or (= i (length string))
                       (char= (char string i) delimiter))
               (when (> i start)
                 (push (subseq string start i) result))
               (setf start (1+ i))))
    (nreverse result)))

(defun read-matrix (filename)
  ;; Print filename (normalize /app/Matrices to ../Matrices)
  (if (and (>= (length filename) 14)
           (string= (subseq filename 0 14) "/app/Matrices/"))
      (format t "../Matrices/~A~%" (subseq filename 14))
      (format t "~A~%" filename))

  (with-open-file (stream filename :direction :input)
    (let ((row 0))
      (loop while (< row 9)
            for line = (read-line stream nil nil)
            while line
            do (let ((trimmed (string-trim '(#\Space #\Tab #\Return) line)))
                 (when (and (> (length trimmed) 0)
                            (char/= (char trimmed 0) #\#))
                   (let ((col 0))
                     (dolist (num-str (split-string trimmed #\Space))
                       (when (and (< col 9) (> (length num-str) 0))
                         (let ((num (parse-integer num-str)))
                           (set-puzzle-cell row col num)
                           (format t "~D " num)
                           (incf col))))
                     (format t "~%")
                     (incf row))))))))

(defun extract-solution (solution)
  (let ((result (make-array 81 :element-type 'fixnum :initial-element 0)))
    ;; Copy original puzzle (clues)
    (dotimes (i 81)
      (setf (aref result i) (aref *puzzle* i)))

    ;; Fill in solution
    (dotimes (i 81)
      (let ((row-id (aref solution i)))
        (when (>= row-id 0)
          (let ((info (aref *row-infos* row-id)))
            (when info
              (destructuring-bind (r c n) info
                (setf (aref result (+ (* r 9) c)) n)))))))
    result))

(defun cover-clues ()
  ;; Cover columns for given clues
  (dotimes (r 9)
    (dotimes (c 9)
      (let ((val (get-puzzle-cell r c)))
        (when (> val 0)
          ;; Find the row for this clue and cover its columns
          (dotimes (row-id 729)
            (let ((info (aref *row-infos* row-id)))
              (when (and info
                        (= (first info) r)
                        (= (second info) c)
                        (= (third info) val))
                ;; Found the row - cover all its columns
                (let ((first-node (aref *row-starts* row-id)))
                  (when first-node
                    (cover-column (dlx-node-column first-node))
                    (do ((node (dlx-node-right first-node) (dlx-node-right node)))
                        ((eq node first-node))
                      (cover-column (dlx-node-column node)))))
                (return)))))))))

;; ============================================================================
;; MAIN
;; ============================================================================

(defun get-script-args ()
  "Get command line arguments after --"
  (let ((args sb-ext:*posix-argv*)
        (result '()))
    (loop for arg in (cdr args)  ; Skip program name
          with past-separator = nil
          do (if (string= arg "--")
                 (setf past-separator t)
                 (when past-separator
                   (push arg result))))
    (nreverse result)))

(defun main-dlx ()
  (let ((args (get-script-args)))
    (if (null args)
        (progn
          (format *error-output* "Usage: sbcl --script dlx.lisp -- <matrix_file>~%")
          (sb-ext:exit :code 1))
        (dolist (filename args)
          (when (search ".matrix" filename)
            ;; Reset state
            (setf *dlx-iterations* 0)
            (setf *puzzle* (make-array 81 :element-type 'fixnum :initial-element 0))
            (setf *row-infos* (make-array 729 :initial-element nil))
            (setf *row-starts* (make-array 729 :initial-element nil))

            ;; Read puzzle
            (read-matrix filename)
            (print-puzzle-grid *puzzle*)

            ;; Build DLX matrix
            (init-dlx-matrix)
            (build-dlx-matrix-from-puzzle)
            (cover-clues)

            ;; Solve
            (let ((solution (make-array 81 :element-type 'fixnum :initial-element -1)))
              (if (dlx-search solution 0)
                  (let ((solved (extract-solution solution)))
                    (print-puzzle-grid solved)
                    (format t "~%Solved in Iterations=~D~%~%" *dlx-iterations*))
                  (format t "~%No solution found~%"))))))))

(main-dlx)
