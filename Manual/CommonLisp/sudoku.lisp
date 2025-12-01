(defpackage :sudoku
  (:use :cl))
(in-package :sudoku)

(defparameter *board* (make-array '(9 9) :element-type 'integer :initial-element 0))
(defparameter *iterations* 0)

(defun read-matrix (filename)
  (with-open-file (stream filename)
    (let ((row 0))
      (loop for line = (read-line stream nil)
            while (and line (< row 9))
            do (unless (or (string= line "") (char= (char line 0) #\#))
                 (loop for char across line
                       with col = 0
                       do (cond ((digit-char-p char)
                                 (setf (aref *board* row col) (digit-char-p char))
                                 (incf col))
                                ((char= char #\.)
                                 (setf (aref *board* row col) 0)
                                 (incf col))))
                 (incf row))))))


(defun print-board ()
  (dotimes (r 9)
    (dotimes (c 9)
      (format t "~d " (aref *board* r c)))
    (format t "~%")))

(defun is-valid (row col num)
  ;; Row check
  (dotimes (c 9)
    (when (= (aref *board* row c) num)
      (return-from is-valid nil)))
  
  ;; Col check
  (dotimes (r 9)
    (when (= (aref *board* r col) num)
      (return-from is-valid nil)))
  
  ;; Box check
  (let ((box-row (* (floor row 3) 3))
        (box-col (* (floor col 3) 3)))
    (dotimes (r 3)
      (dotimes (c 3)
        (when (= (aref *board* (+ box-row r) (+ box-col c)) num)
          (return-from is-valid nil)))))
  
  t)

(defun find-empty ()
  (dotimes (r 9)
    (dotimes (c 9)
      (when (= (aref *board* r c) 0)
        (return-from find-empty (list r c)))))
  nil)

(defun solve ()
  (let ((empty (find-empty)))
    (unless empty
      (return-from solve t))
    
    (let ((row (first empty))
          (col (second empty)))
      (loop for num from 1 to 9 do
        (incf *iterations*)
        ;; (format t "Checking ~d at (~d, ~d). Iteration: ~d~%" num row col *iterations*)
        (when (is-valid row col num)
          (setf (aref *board* row col) num)
          (when (solve)
            (return-from solve t))
          (setf (aref *board* row col) 0)))
      nil)))

(defun main ()
  (handler-case
    (let ((args sb-ext:*posix-argv*))
      (if (< (length args) 2)
          (format t "Usage: sbcl --script sudoku.lisp <matrix_file>~%")
          (let ((filename (second args)))
            (read-matrix filename)
            (format t "Puzzle:~%")
            (print-board)
            
            (if (solve)
                (progn
                  (format t "Puzzle:~%")
                  (print-board)
                  (format t "Solved in Iterations=~d~%" *iterations*))
                (format t "No solution found.~%")))))
    (error (e)
      (format t "Error: ~a~%" e))))

(main)
