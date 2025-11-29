;; Sudoku Solver in Emacs Lisp

(defvar puzzle (make-vector 81 0))
(defvar count-iter 0)

(defun print-puzzle ()
  (message "\nPuzzle:")
  (dotimes (r 9)
    (let ((line ""))
      (dotimes (c 9)
        (setq line (concat line (number-to-string (aref puzzle (+ (* r 9) c))) " ")))
      (message "%s" line))))

(defun read-matrix-file (filename)
  (message "%s" filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (goto-char (point-min))
    (let ((row 0))
      (while (and (< row 9) (not (eobp)))
        (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
          (if (or (string-prefix-p "#" line) (string-empty-p (string-trim line)))
              (forward-line 1)
            (let ((parts (split-string line)))
              (if (= (length parts) 9)
                  (progn
                    (dotimes (col 9)
                      (aset puzzle (+ (* row 9) col) (string-to-number (nth col parts))))
                    (setq row (1+ row)))
                ))
            (forward-line 1)))))))

(defun is-possible (r c val)
  (catch 'found
    (dotimes (i 9)
      (if (= (aref puzzle (+ (* i 9) c)) val) (throw 'found nil))
      (if (= (aref puzzle (+ (* r 9) i)) val) (throw 'found nil)))
    
    (let ((r0 (* (/ r 3) 3))
          (c0 (* (/ c 3) 3)))
      (dotimes (i 3)
        (dotimes (j 3)
          (if (= (aref puzzle (+ (* (+ r0 i) 9) (+ c0 j))) val) (throw 'found nil)))))
    t))

(defun solve ()
  (catch 'solved
    (dotimes (r 9)
      (dotimes (c 9)
        (if (= (aref puzzle (+ (* r 9) c)) 0)
            (progn
              (dotimes (val 9)
                (let ((v (1+ val)))
                  (setq count-iter (1+ count-iter))
                  (if (is-possible r c v)
                      (progn
                        (aset puzzle (+ (* r 9) c) v)
                        (if (solve) (throw 'solved t))
                        (aset puzzle (+ (* r 9) c) 0)))))
              (throw 'solved nil)))))
    (print-puzzle)
    (message "\nSolved in Iterations=%d\n" count-iter)
    t))

(defun main ()
  (let ((start (float-time)))
    (dolist (arg command-line-args-left)
      (if (string-suffix-p ".matrix" arg)
          (progn
            (read-matrix-file arg)
            (print-puzzle)
            (setq count-iter 0)
            (solve))))
    (let ((end (float-time)))
      (message "Seconds to process %.3f" (- end start)))))

(main)
