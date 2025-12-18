#!/usr/bin/env guile
!#
;;; Sudoku Solver in GNU Guile (Scheme)
;;; Brute-force backtracking algorithm matching C reference exactly

(use-modules (ice-9 rdelim))

;; Global puzzle grid (flat vector) and counter
(define puzzle (make-vector 81 0))
(define count 0)

;; Grid access
(define (get-cell row col)
  (vector-ref puzzle (+ (* row 9) col)))

(define (set-cell! row col val)
  (vector-set! puzzle (+ (* row 9) col) val))

;; Print puzzle
(define (print-puzzle)
  (display "\nPuzzle:\n")
  (let row-loop ((r 0))
    (if (< r 9)
        (begin
          (let col-loop ((c 0))
            (if (< c 9)
                (begin
                  (display (get-cell r c))
                  (display " ")
                  (col-loop (+ c 1)))))
          (newline)
          (row-loop (+ r 1))))))

;; Read matrix
(define (read-matrix filename)
  ;; Print filename
  (if (string-prefix? "/app/Matrices/" filename)
      (display (string-append "../Matrices/" (substring filename 14) "\n"))
      (display (string-append filename "\n")))

  (call-with-input-file filename
    (lambda (port)
      (let row-loop ((row 0))
        (if (< row 9)
            (let ((line (read-line port)))
              (if (eof-object? line)
                  #t
                  (let ((trimmed (string-trim-both line)))
                    (cond
                      ((string-null? trimmed) (row-loop row))
                      ((char=? (string-ref trimmed 0) #\#) (row-loop row))
                      (else
                       (let ((numbers (filter (lambda (s) (not (string-null? s)))
                                              (string-split trimmed #\space))))
                         (let col-loop ((nums numbers) (col 0))
                           (if (and (< col 9) (pair? nums))
                               (let ((num (string->number (car nums))))
                                 (set-cell! row col num)
                                 (display num)
                                 (display " ")
                                 (col-loop (cdr nums) (+ col 1)))))
                         (newline)
                         (row-loop (+ row 1)))))))))))))

;; Check validity
(define (is-valid row col val)
  (and
   ;; Row check
   (let check-row ((c 0))
     (cond
       ((= c 9) #t)
       ((= (get-cell row c) val) #f)
       (else (check-row (+ c 1)))))
   ;; Column check
   (let check-col ((r 0))
     (cond
       ((= r 9) #t)
       ((= (get-cell r col) val) #f)
       (else (check-col (+ r 1)))))
   ;; Box check
   (let ((box-row (* (quotient row 3) 3))
         (box-col (* (quotient col 3) 3)))
     (let br-loop ((br 0))
       (cond
         ((= br 3) #t)
         (else
          (let bc-loop ((bc 0))
            (cond
              ((= bc 3) (br-loop (+ br 1)))
              ((= (get-cell (+ box-row br) (+ box-col bc)) val) #f)
              (else (bc-loop (+ bc 1)))))))))))

;; Find empty cell
(define (find-empty)
  (let loop ((r 0) (c 0))
    (cond
      ((= r 9) (cons -1 -1))
      ((= c 9) (loop (+ r 1) 0))
      ((= (get-cell r c) 0) (cons r c))
      (else (loop r (+ c 1))))))

;; Solve
(define (solve)
  (let ((pos (find-empty)))
    (let ((row (car pos)) (col (cdr pos)))
      (if (= row -1)
          (begin
            (print-puzzle)
            (display "\nSolved in Iterations=")
            (display count)
            (display "\n\n")
            #t)
          (let try-val ((val 1))
            (cond
              ((> val 9) #f)
              (else
               (set! count (+ count 1))
               (if (is-valid row col val)
                   (begin
                     (set-cell! row col val)
                     (if (solve)
                         #t
                         (begin
                           (set-cell! row col 0)
                           (try-val (+ val 1)))))
                   (try-val (+ val 1))))))))))

;; Main
(let ((args (command-line)))
  (if (< (length args) 2)
      (begin
        (display "Usage: guile sudoku.scm <matrix_file>\n" (current-error-port))
        (exit 1))
      (begin
        (set! count 0)
        (read-matrix (cadr args))
        (print-puzzle)
        (if (not (solve))
            (display "No solution found\n")))))
