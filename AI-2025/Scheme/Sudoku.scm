#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim)
             (ice-9 format)
             (ice-9 match))

;; Global iteration counter
(define iterations 0)

;; Read matrix from file
(define (read-matrix filename)
  (let ((port (open-input-file filename))
        (board (make-vector 9)))
    (let loop ((i 0))
      (if (< i 9)
          (let ((line (read-line port)))
            (if (eof-object? line)
                board
                (if (or (string-null? line) (char=? (string-ref line 0) #\#))
                    (loop i) ;; Skip comments/empty
                    (let ((parts (string-split line #\space)))
                      (vector-set! board i 
                                   (list->vector (map string->number 
                                                      (filter (lambda (s) (not (string-null? s))) parts))))
                      (loop (+ i 1))))))
          (begin
            (close-input-port port)
            board)))))

;; Print board
(define (print-board board)
  (do ((i 0 (+ i 1)))
      ((= i 9))
    (do ((j 0 (+ j 1)))
        ((= j 9))
      (format #t " ~a " (vector-ref (vector-ref board i) j)))
    (newline)))

;; Check validity
(define (is-valid? board row col num)
  (let ((valid #t))
    ;; Row check
    (do ((j 0 (+ j 1)))
        ((= j 9))
      (if (= (vector-ref (vector-ref board row) j) num)
          (set! valid #f)))
    
    ;; Col check
    (if valid
        (do ((i 0 (+ i 1)))
            ((= i 9))
          (if (= (vector-ref (vector-ref board i) col) num)
              (set! valid #f))))
    
    ;; Box check
    (if valid
        (let ((start-row (* (quotient row 3) 3))
              (start-col (* (quotient col 3) 3)))
          (do ((i 0 (+ i 1)))
              ((= i 3))
            (do ((j 0 (+ j 1)))
                ((= j 3))
              (if (= (vector-ref (vector-ref board (+ start-row i)) (+ start-col j)) num)
                  (set! valid #f))))))
    valid))

;; Solve
(define (solve board)
  (let loop-row ((row 0))
    (if (= row 9)
        #t ;; Solved
        (let loop-col ((col 0))
          (if (= col 9)
              (loop-row (+ row 1))
              (if (= (vector-ref (vector-ref board row) col) 0)
                  (let try-num ((num 1))
                    (if (> num 9)
                        #f ;; Backtrack
                        (begin
                          (set! iterations (+ iterations 1))
                          (if (is-valid? board row col num)
                              (begin
                                (vector-set! (vector-ref board row) col num)
                                (if (solve board)
                                    #t
                                    (begin
                                      (vector-set! (vector-ref board row) col 0)
                                      (try-num (+ num 1)))))
                              (try-num (+ num 1))))))
                  (loop-col (+ col 1))))))))

;; Main
(define (main args)
  (if (null? (cdr args))
      (format #t "Usage: sudoku.scm input_file\n")
      (let ((filename (cadr args)))
        (format #t "~a\n" filename)
        (let ((board (read-matrix filename)))
          (format #t "Puzzle:\n")
          (print-board board)
          (if (solve board)
              (begin
                (format #t "Puzzle:\n")
                (print-board board)
                (format #t "Solved in Iterations=~a\n" iterations))
              (format #t "No solution found.\n"))))))

(main (command-line))
