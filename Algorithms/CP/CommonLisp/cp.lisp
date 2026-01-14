#!/usr/bin/env sbcl --script
;;; Sudoku CP Solver in Common Lisp (SBCL)
;;; Constraint Propagation with backtracking search

;; ============================================================================
;; GLOBAL STATE
;; ============================================================================

(defvar *cp-iterations* 0)

;; ============================================================================
;; GRID STRUCTURE
;; ============================================================================

(defstruct cp-grid
  (vals (make-array '(9 9) :element-type 'fixnum :initial-element 0))
  (cands (make-array '(9 9) :element-type 'fixnum :initial-element 0)))

(defun get-val (grid r c)
  (aref (cp-grid-vals grid) r c))

(defun set-val (grid r c v)
  (setf (aref (cp-grid-vals grid) r c) v))

(defun get-cand (grid r c)
  (aref (cp-grid-cands grid) r c))

(defun set-cand (grid r c v)
  (setf (aref (cp-grid-cands grid) r c) v))

;; ============================================================================
;; BITSET OPERATIONS
;; ============================================================================

(defun has-cand (set d)
  (not (zerop (logand set (ash 1 d)))))

(defun rem-cand (set d)
  (logand set (lognot (ash 1 d))))

(defun count-cands (set)
  (logcount set))

(defun first-cand (set)
  (loop for d from 1 to 9
        when (has-cand set d)
          return d
        finally (return 0)))

;; ============================================================================
;; GRID OPERATIONS
;; ============================================================================

(defun copy-grid (grid)
  (let ((new-grid (make-cp-grid)))
    (dotimes (r 9)
      (dotimes (c 9)
        (set-val new-grid r c (get-val grid r c))
        (set-cand new-grid r c (get-cand grid r c))))
    new-grid))

(defun init-grid (grid puzzle)
  (dotimes (r 9)
    (dotimes (c 9)
      (let ((v (aref puzzle r c)))
        (if (zerop v)
            (progn
              (set-val grid r c 0)
              (set-cand grid r c #x3FE))  ; bits 1-9 set
            (progn
              (set-val grid r c v)
              (set-cand grid r c (ash 1 v))))))))

;; ============================================================================
;; CONSTRAINT PROPAGATION
;; ============================================================================

(defun get-peers (r c)
  "Return list of (row . col) pairs for all peers of cell (r,c)"
  (let ((peers '()))
    ;; Same row
    (dotimes (col 9)
      (unless (= col c)
        (push (cons r col) peers)))
    ;; Same column
    (dotimes (row 9)
      (unless (= row r)
        (push (cons row c) peers)))
    ;; Same box
    (let ((br (* (floor r 3) 3))
          (bc (* (floor c 3) 3)))
      (loop for rr from br below (+ br 3) do
            (loop for cc from bc below (+ bc 3) do
                  (unless (or (= rr r) (= cc c))
                    (push (cons rr cc) peers)))))
    peers))

(defun eliminate (grid r c d)
  "Eliminate candidate d from cell (r,c). Returns nil on contradiction."
  (if (not (has-cand (get-cand grid r c) d))
      t  ; Already eliminated
      (progn
        (set-cand grid r c (rem-cand (get-cand grid r c) d))
        (let ((rem (count-cands (get-cand grid r c))))
          (cond
            ((zerop rem) nil)  ; Contradiction
            ((and (= rem 1) (zerop (get-val grid r c)))
             ;; Only one candidate left - assign it
             (assign grid r c (first-cand (get-cand grid r c))))
            (t t))))))

(defun assign (grid r c d)
  "Assign digit d to cell (r,c) and propagate constraints."
  (incf *cp-iterations*)
  (set-val grid r c d)
  (set-cand grid r c (ash 1 d))

  ;; Eliminate d from all peers
  (let ((peers (get-peers r c)))
    (dolist (peer peers t)
      (let ((pr (car peer))
            (pc (cdr peer)))
        (unless (eliminate grid pr pc d)
          (return-from assign nil)))))
  t)

(defun propagate (grid)
  "Apply constraint propagation strategies. Returns nil on contradiction."
  (let ((changed t))
    (loop while changed do
          (setf changed nil)

          ;; Strategy 1: Naked singles (cells with only one candidate)
          (dotimes (row 9)
            (dotimes (col 9)
              (when (zerop (get-val grid row col))
                (let ((rem (count-cands (get-cand grid row col))))
                  (cond
                    ((zerop rem) (return-from propagate nil))  ; Contradiction
                    ((= rem 1)
                     (when (assign grid row col (first-cand (get-cand grid row col)))
                       (setf changed t))))))))

          ;; Strategy 2: Hidden singles in rows
          (dotimes (row 9)
            (loop for d from 1 to 9 do
                  (let ((cnt 0)
                        (lcol -1)
                        (assigned nil))
                    (dotimes (col 9)
                      (when (= (get-val grid row col) d)
                        (setf assigned t))
                      (when (and (not assigned) (has-cand (get-cand grid row col) d))
                        (incf cnt)
                        (setf lcol col)))
                    (when (and (not assigned) (= cnt 1))
                      (when (assign grid row lcol d)
                        (setf changed t))))))

          ;; Hidden singles in columns
          (dotimes (col 9)
            (loop for d from 1 to 9 do
                  (let ((cnt 0)
                        (lrow -1)
                        (assigned nil))
                    (dotimes (row 9)
                      (when (= (get-val grid row col) d)
                        (setf assigned t))
                      (when (and (not assigned) (has-cand (get-cand grid row col) d))
                        (incf cnt)
                        (setf lrow row)))
                    (when (and (not assigned) (= cnt 1))
                      (when (assign grid lrow col d)
                        (setf changed t))))))

          ;; Hidden singles in boxes
          (dotimes (box 9)
            (let ((br (* (floor box 3) 3))
                  (bc (* (mod box 3) 3)))
              (loop for d from 1 to 9 do
                    (let ((cnt 0)
                          (lr -1)
                          (lc -1)
                          (assigned nil))
                      (block box-loop
                        (loop for r from br below (+ br 3) do
                              (loop for c from bc below (+ bc 3) do
                                    (when (= (get-val grid r c) d)
                                      (setf assigned t)
                                      (return-from box-loop))
                                    (when (and (not assigned) (has-cand (get-cand grid r c) d))
                                      (incf cnt)
                                      (setf lr r)
                                      (setf lc c)))))
                      (when (and (not assigned) (= cnt 1))
                        (when (assign grid lr lc d)
                          (setf changed t)))))))))
  t)

;; ============================================================================
;; SEARCH
;; ============================================================================

(defun find-mrv (grid)
  "Find cell with minimum remaining values. Returns (row . col) or nil if complete."
  (let ((min-c 10)
        (best-r -1)
        (best-c -1))
    (dotimes (r 9)
      (dotimes (c 9)
        (when (zerop (get-val grid r c))
          (let ((nc (count-cands (get-cand grid r c))))
            (when (< nc min-c)
              (setf min-c nc)
              (setf best-r r)
              (setf best-c c))))))
    (if (= best-r -1)
        nil
        (cons best-r best-c))))

(defun cp-search (grid)
  "Search with backtracking. Returns solution grid or nil."
  (let ((mrv (find-mrv grid)))
    (if (null mrv)
        grid  ; Complete - return solution
        (let ((r (car mrv))
              (c (cdr mrv))
              (cands (get-cand grid r c)))
          ;; Try each candidate digit
          (loop for d from 1 to 9 do
                (when (has-cand cands d)
                  (let ((g-copy (copy-grid grid)))
                    (when (and (assign grid r c d) (propagate grid))
                      (let ((res (cp-search grid)))
                        (when res
                          (return-from cp-search res))))
                    ;; Restore and try next
                    (setf grid (copy-grid g-copy)))))
          nil))))

;; ============================================================================
;; PUZZLE I/O
;; ============================================================================

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

(defun read-puzzle (filename)
  "Read puzzle from file. Returns 2D array."
  (let ((puzzle (make-array '(9 9) :element-type 'fixnum :initial-element 0)))
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
                             (setf (aref puzzle row col) num)
                             (format t "~D " num)
                             (incf col))))
                       (format t "~%")
                       (incf row)))))))
    puzzle))

(defun print-grid (grid)
  (format t "~%Puzzle:~%")
  (dotimes (r 9)
    (dotimes (c 9)
      (format t "~D " (get-val grid r c)))
    (format t "~%")))

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

(defun main-cp ()
  (let ((args (get-script-args)))
    (if (null args)
        (progn
          (format *error-output* "Usage: sbcl --script cp.lisp -- <matrix_file>~%")
          (sb-ext:exit :code 1))
        (dolist (filename args)
          (when (search ".matrix" filename)
            ;; Reset state
            (setf *cp-iterations* 0)

            ;; Read puzzle
            (let* ((puzzle (read-puzzle filename))
                   (grid (make-cp-grid)))
              (init-grid grid puzzle)
              (print-grid grid)

              ;; Solve (propagate will find initial clues as hidden singles and assign them)
              (if (propagate grid)
                  (let ((sol (cp-search grid)))
                    (if sol
                        (progn
                          (print-grid sol)
                          (format t "~%Solved in Iterations=~D~%~%" *cp-iterations*))
                        (format t "~%No solution found~%")))
                  (format t "~%Contradiction during initial propagation~%"))))))))

(main-cp)
