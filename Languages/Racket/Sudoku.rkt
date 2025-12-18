#lang racket
(require racket/function)

;; Global state
(define puzzle (make-vector 81 0))  ; 9x9 flattened
(define count 0)
(define start-time (current-milliseconds))

;; Helper: 2D index to 1D
(define (idx row col) (+ (* row 9) col))

;; Get/set cell value
(define (get-cell row col) (vector-ref puzzle (idx row col)))
(define (set-cell! row col val) (vector-set! puzzle (idx row col) val))

;; Print puzzle in C format (trailing space after each digit)
(define (print-puzzle)
  (displayln "\nPuzzle:")
  (for ([row 9])
    (for ([col 9])
      (display (get-cell row col))
      (display " "))
    (displayln "")))

;; Print row without header (for initial input)
(define (print-row row)
  (for ([col 9])
    (display (get-cell row col))
    (display " "))
  (displayln ""))

;; Read matrix file
(define (read-matrix-file filename)
  ;; Normalize path for display
  (define display-path
    (if (string-prefix? filename "/app/Matrices/")
        (string-append "../" (substring filename 5))
        filename))
  (displayln display-path)

  (when (file-exists? filename)
    (define row 0)
    (with-input-from-file filename
      (thunk
        (for ([line (in-lines)])
          (define trimmed (string-trim line))
          (unless (or (string-prefix? trimmed "#") (equal? trimmed ""))
            (define nums (string-split trimmed))
            (when (and (= (length nums) 9) (< row 9))
              (for ([col 9])
                (set-cell! row col (string->number (list-ref nums col))))
              ;; Print input line as we read (like C does)
              (print-row row)
              (set! row (+ row 1)))))))))

;; Check if placing val at (row, col) is valid
(define (is-valid? row col val)
  (let/ec return
    ;; Check row
    (for ([i 9])
      (when (= (get-cell row i) val) (return #f)))
    ;; Check column
    (for ([i 9])
      (when (= (get-cell i col) val) (return #f)))
    ;; Check 3x3 box
    (define box-row (* (quotient row 3) 3))
    (define box-col (* (quotient col 3) 3))
    (for ([r 3])
      (for ([c 3])
        (when (= (get-cell (+ box-row r) (+ box-col c)) val) (return #f))))
    (return #t)))

;; Solve using brute-force backtracking
(define (solve)
  (let/ec return
    ;; Find first empty cell (row-major order)
    (for ([row 9])
      (for ([col 9])
        (when (= (get-cell row col) 0)
          ;; Try values 1-9
          (for ([val (in-range 1 10)])
            (set! count (+ count 1))  ; COUNT BEFORE validity check
            (when (is-valid? row col val)
              (set-cell! row col val)
              (when (= (solve) 1)
                (return 1))
              (set-cell! row col 0)))
          (return 0))))  ; Backtrack
    ;; No empty cell found - puzzle solved
    (print-puzzle)
    (displayln (string-append "\nSolved in Iterations=" (number->string count) "\n"))
    (return 1)))

;; Main
(define args (current-command-line-arguments))
(for ([i (vector-length args)])
  (define file (vector-ref args i))
  (when (string-suffix? file ".matrix")
    (read-matrix-file file)
    (print-puzzle)  ; Print input puzzle with header (like C does before solve)
    (set! count 0)
    (solve)
    (displayln (string-append "Seconds to process "
                             (~r (/ (- (current-milliseconds) start-time) 1000.0)
                                 #:precision 3)))))
