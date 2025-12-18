;;; Sudoku Solver in Common Lisp (SBCL)
;;; Brute-force backtracking algorithm matching C reference exactly

(defvar *puzzle* (make-array '(9 9) :initial-element 0))
(defvar *count* 0)

(defun get-cell (row col)
  (aref *puzzle* row col))

(defun set-cell (row col val)
  (setf (aref *puzzle* row col) val))

(defun print-puzzle ()
  (format t "~%Puzzle:~%")
  (dotimes (r 9)
    (dotimes (c 9)
      (format t "~D " (get-cell r c)))
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
                           (set-cell row col num)
                           (format t "~D " num)
                           (incf col))))
                     (format t "~%")
                     (incf row))))))))

(defun is-valid (row col val)
  ;; Check row
  (dotimes (c 9)
    (when (= (get-cell row c) val)
      (return-from is-valid nil)))
  ;; Check column
  (dotimes (r 9)
    (when (= (get-cell r col) val)
      (return-from is-valid nil)))
  ;; Check 3x3 box
  (let ((box-row (* (floor row 3) 3))
        (box-col (* (floor col 3) 3)))
    (dotimes (br 3)
      (dotimes (bc 3)
        (when (= (get-cell (+ box-row br) (+ box-col bc)) val)
          (return-from is-valid nil)))))
  t)

(defun find-empty ()
  "Find first empty cell in row-major order. Returns (row col) or nil."
  (dotimes (r 9)
    (dotimes (c 9)
      (when (= (get-cell r c) 0)
        (return-from find-empty (list r c)))))
  nil)

(defun solve ()
  (let ((empty (find-empty)))
    (if (null empty)
        ;; No empty cell - puzzle is solved
        (progn
          (print-puzzle)
          (format t "~%Solved in Iterations=~D~%~%" *count*)
          t)
        ;; Try values 1-9
        (let ((row (first empty))
              (col (second empty)))
          (loop for val from 1 to 9
                do (incf *count*)  ; Count EVERY attempt
                   (when (is-valid row col val)
                     (set-cell row col val)
                     (when (solve)
                       (return-from solve t))
                     (set-cell row col 0)))  ; Backtrack
          nil))))

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

(defun main ()
  (let ((args (get-script-args)))
    (if (null args)
        (progn
          (format *error-output* "Usage: sbcl --script sudoku.lisp -- <matrix_file>~%")
          (sb-ext:exit :code 1))
        (dolist (filename args)
          (when (search ".matrix" filename)
            (setf *count* 0)
            (setf *puzzle* (make-array '(9 9) :initial-element 0))
            (read-matrix filename)
            (print-puzzle)
            (unless (solve)
              (format t "No solution found~%")))))))

(main)
