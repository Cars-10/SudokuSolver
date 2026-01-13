#!/usr/bin/env guile
!#
;;; Sudoku DLX Solver in GNU Guile (Scheme)
;;; Dancing Links (Algorithm X) implementation using vectors

(use-modules (ice-9 rdelim))

;; ============================================================================
;; NODE REPRESENTATION USING VECTORS
;; ============================================================================
;; Node vector: [left right up down column size row-id col-id]
;; Indices: 0=left, 1=right, 2=up, 3=down, 4=column, 5=size, 6=row-id, 7=col-id

(define (make-node)
  (vector 0 0 0 0 0 0 -1 -1))

(define (node-left n) (vector-ref n 0))
(define (node-right n) (vector-ref n 1))
(define (node-up n) (vector-ref n 2))
(define (node-down n) (vector-ref n 3))
(define (node-column n) (vector-ref n 4))
(define (node-size n) (vector-ref n 5))
(define (node-row-id n) (vector-ref n 6))
(define (node-col-id n) (vector-ref n 7))

(define (set-node-left! n v) (vector-set! n 0 v))
(define (set-node-right! n v) (vector-set! n 1 v))
(define (set-node-up! n v) (vector-set! n 2 v))
(define (set-node-down! n v) (vector-set! n 3 v))
(define (set-node-column! n v) (vector-set! n 4 v))
(define (set-node-size! n v) (vector-set! n 5 v))
(define (set-node-row-id! n v) (vector-set! n 6 v))
(define (set-node-col-id! n v) (vector-set! n 7 v))

;; ============================================================================
;; GLOBAL DATA STRUCTURES
;; ============================================================================

(define dlx-iterations 0)
(define puzzle (make-vector 81 0))
(define root-node #f)
(define column-headers (make-vector 324 #f))
(define row-infos (make-vector 729 #f))  ;; Vector of (row col num) lists
(define row-starts (make-vector 729 #f))  ;; Vector of first node in each row

;; Helper to get/set puzzle cells
(define (get-puzzle-cell r c)
  (vector-ref puzzle (+ (* r 9) c)))

(define (set-puzzle-cell! r c val)
  (vector-set! puzzle (+ (* r 9) c) val))

;; ============================================================================
;; CONSTRAINT COLUMN CALCULATIONS
;; ============================================================================

(define (get-position-col r c)
  (+ (* r 9) c))

(define (get-row-col r n)
  (+ 81 (* r 9) (- n 1)))

(define (get-col-col c n)
  (+ 162 (* c 9) (- n 1)))

(define (get-box-col r c n)
  (let ((box (+ (* (quotient r 3) 3) (quotient c 3))))
    (+ 243 (* box 9) (- n 1))))

;; ============================================================================
;; DLX MATRIX INITIALIZATION
;; ============================================================================

(define (init-dlx-matrix!)
  ;; Create root node
  (set! root-node (make-node))
  (set-node-left! root-node root-node)
  (set-node-right! root-node root-node)
  (set-node-up! root-node root-node)
  (set-node-down! root-node root-node)
  (set-node-column! root-node root-node)
  (set-node-size! root-node 0)

  ;; Create 324 column headers
  (let loop ((i 0))
    (when (< i 324)
      (let ((col (make-node)))
        (set-node-up! col col)
        (set-node-down! col col)
        (set-node-column! col col)
        (set-node-size! col 0)
        (set-node-col-id! col i)

        ;; Link into header list (insert before root)
        (let ((left-node (node-left root-node)))
          (set-node-left! col left-node)
          (set-node-right! col root-node)
          (set-node-right! left-node col)
          (set-node-left! root-node col))

        (vector-set! column-headers i col))
      (loop (+ i 1)))))

;; ============================================================================
;; NODE OPERATIONS
;; ============================================================================

(define (add-node-to-column! col row-id)
  (let ((node (make-node)))
    (set-node-column! node col)
    (set-node-row-id! node row-id)

    ;; Insert at end of column's circular list
    (let ((up-node (node-up col)))
      (set-node-down! node col)
      (set-node-up! node up-node)
      (set-node-down! up-node node)
      (set-node-up! col node))

    ;; Increment column size
    (set-node-size! col (+ (node-size col) 1))
    node))

(define (build-dlx-row! r c n row-id)
  ;; Store row info
  (vector-set! row-infos row-id (list r c n))

  ;; Create 4 nodes for the 4 constraints
  (let ((n1 (add-node-to-column! (vector-ref column-headers (get-position-col r c)) row-id))
        (n2 (add-node-to-column! (vector-ref column-headers (get-row-col r n)) row-id))
        (n3 (add-node-to-column! (vector-ref column-headers (get-col-col c n)) row-id))
        (n4 (add-node-to-column! (vector-ref column-headers (get-box-col r c n)) row-id)))

    ;; Link nodes horizontally in circular list
    (set-node-right! n1 n2)
    (set-node-right! n2 n3)
    (set-node-right! n3 n4)
    (set-node-right! n4 n1)

    (set-node-left! n1 n4)
    (set-node-left! n2 n1)
    (set-node-left! n3 n2)
    (set-node-left! n4 n3)

    ;; Store first node of this row
    (vector-set! row-starts row-id n1)
    n1))

(define (build-dlx-matrix-from-puzzle!)
  (let ((row-id 0))
    (let r-loop ((r 0))
      (when (< r 9)
        (let c-loop ((c 0))
          (when (< c 9)
            (let ((val (get-puzzle-cell r c)))
              (if (= val 0)
                  ;; Empty cell - create rows for all values 1-9
                  (let n-loop ((n 1))
                    (when (<= n 9)
                      (build-dlx-row! r c n row-id)
                      (set! row-id (+ row-id 1))
                      (n-loop (+ n 1))))
                  ;; Clue - create only one row
                  (begin
                    (build-dlx-row! r c val row-id)
                    (set! row-id (+ row-id 1)))))
            (c-loop (+ c 1))))
        (r-loop (+ r 1))))))

;; ============================================================================
;; DLX COVER/UNCOVER OPERATIONS
;; ============================================================================

(define (cover-column! col)
  ;; Remove column from header list
  (let ((left-node (node-left col))
        (right-node (node-right col)))
    (set-node-right! left-node right-node)
    (set-node-left! right-node left-node))

  ;; For each row in this column
  (let row-loop ((row-node (node-down col)))
    (when (not (eq? row-node col))
      ;; For each node in this row
      (let node-loop ((right-node (node-right row-node)))
        (when (not (eq? right-node row-node))
          ;; Remove node from its column
          (let ((up-node (node-up right-node))
                (down-node (node-down right-node))
                (col-node (node-column right-node)))
            (set-node-down! up-node down-node)
            (set-node-up! down-node up-node)
            (set-node-size! col-node (- (node-size col-node) 1)))
          (node-loop (node-right right-node))))
      (row-loop (node-down row-node)))))

(define (uncover-column! col)
  ;; For each row in this column (in reverse)
  (let row-loop ((row-node (node-up col)))
    (when (not (eq? row-node col))
      ;; For each node in this row (in reverse)
      (let node-loop ((left-node (node-left row-node)))
        (when (not (eq? left-node row-node))
          ;; Restore node to its column
          (let ((up-node (node-up left-node))
                (down-node (node-down left-node))
                (col-node (node-column left-node)))
            (set-node-size! col-node (+ (node-size col-node) 1))
            (set-node-down! up-node left-node)
            (set-node-up! down-node left-node))
          (node-loop (node-left left-node))))
      (row-loop (node-up row-node))))

  ;; Restore column to header list
  (let ((left-node (node-left col))
        (right-node (node-right col)))
    (set-node-right! left-node col)
    (set-node-left! right-node col)))

;; ============================================================================
;; DLX SEARCH (ALGORITHM X)
;; ============================================================================

(define (choose-column)
  ;; Choose column with minimum size (Knuth's S heuristic)
  (let ((best #f)
        (min-size 999999))
    (let loop ((col-node (node-right root-node)))
      (when (not (eq? col-node root-node))
        (let ((size (node-size col-node)))
          (when (< size min-size)
            (set! min-size size)
            (set! best col-node)))
        (loop (node-right col-node))))
    best))

(define (dlx-search! solution depth)
  (set! dlx-iterations (+ dlx-iterations 1))

  ;; If matrix is empty, we found a solution
  (if (eq? (node-right root-node) root-node)
      #t
      (let ((col (choose-column)))
        (if (or (not col) (= (node-size col) 0))
            #f
            (begin
              ;; Cover this column
              (cover-column! col)

              ;; Try each row in this column
              (let try-row ((row-node (node-down col)))
                (if (eq? row-node col)
                    (begin
                      ;; No solution found, uncover and backtrack
                      (uncover-column! col)
                      #f)
                    (begin
                      ;; Add row to solution
                      (vector-set! solution depth (node-row-id row-node))

                      ;; Cover all other columns in this row
                      (let cover-loop ((right-node (node-right row-node)))
                        (when (not (eq? right-node row-node))
                          (cover-column! (node-column right-node))
                          (cover-loop (node-right right-node))))

                      ;; Recurse
                      (if (dlx-search! solution (+ depth 1))
                          #t
                          (begin
                            ;; Backtrack: uncover columns in reverse
                            (let uncover-loop ((left-node (node-left row-node)))
                              (when (not (eq? left-node row-node))
                                (uncover-column! (node-column left-node))
                                (uncover-loop (node-left left-node))))

                            ;; Try next row
                            (try-row (node-down row-node))))))))))))

;; ============================================================================
;; PUZZLE I/O
;; ============================================================================

(define (print-puzzle)
  (display "\nPuzzle:\n")
  (let r-loop ((r 0))
    (when (< r 9)
      (let c-loop ((c 0))
        (when (< c 9)
          (display (get-puzzle-cell r c))
          (display " ")
          (c-loop (+ c 1))))
      (newline)
      (r-loop (+ r 1)))))

(define (read-matrix filename)
  ;; Print filename
  (if (string-prefix? "/app/Matrices/" filename)
      (display (string-append "../Matrices/" (substring filename 14) "\n"))
      (display (string-append filename "\n")))

  (call-with-input-file filename
    (lambda (port)
      (let row-loop ((row 0))
        (when (< row 9)
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
                         (when (and (< col 9) (pair? nums))
                           (let ((num (string->number (car nums))))
                             (set-puzzle-cell! row col num)
                             (display num)
                             (display " ")
                             (col-loop (cdr nums) (+ col 1)))))
                       (newline)
                       (row-loop (+ row 1)))))))))))))

(define (extract-solution solution)
  (let ((result (make-vector 81 0)))
    ;; Copy original puzzle (clues)
    (let i-loop ((i 0))
      (when (< i 81)
        (vector-set! result i (vector-ref puzzle i))
        (i-loop (+ i 1))))

    ;; Fill in solution
    (let sol-loop ((i 0))
      (when (< i 81)
        (let ((row-id (vector-ref solution i)))
          (when (>= row-id 0)
            (let ((info (vector-ref row-infos row-id)))
              (when info
                (let ((r (car info))
                      (c (cadr info))
                      (n (caddr info)))
                  (vector-set! result (+ (* r 9) c) n)))))
          (sol-loop (+ i 1)))))
    result))

(define (cover-clues!)
  ;; Cover columns for given clues
  (let r-loop ((r 0))
    (when (< r 9)
      (let c-loop ((c 0))
        (when (< c 9)
          (let ((val (get-puzzle-cell r c)))
            (when (> val 0)
              ;; Find the row for this clue and cover its columns
              (let find-row ((row-id 0))
                (when (< row-id 729)
                  (let ((info (vector-ref row-infos row-id)))
                    (if (and info
                             (= (car info) r)
                             (= (cadr info) c)
                             (= (caddr info) val))
                        ;; Found the row - cover all its columns
                        (let ((first-node (vector-ref row-starts row-id)))
                          (when first-node
                            (let cover-loop ((node first-node))
                              (cover-column! (node-column node))
                              (let ((next (node-right node)))
                                (when (not (eq? next first-node))
                                  (cover-loop next))))))
                        (find-row (+ row-id 1))))))))
          (c-loop (+ c 1))))
      (r-loop (+ r 1)))))

;; ============================================================================
;; MAIN
;; ============================================================================

(let ((args (command-line)))
  (if (< (length args) 2)
      (begin
        (display "Usage: guile dlx.scm <matrix_file>\n" (current-error-port))
        (exit 1))
      (begin
        ;; Process each matrix file
        (let process-file ((i 1))
          (when (< i (length args))
            (let ((filename (list-ref args i)))
              (when (string-suffix? ".matrix" filename)
                ;; Reset state
                (set! dlx-iterations 0)
                (set! puzzle (make-vector 81 0))
                (set! row-infos (make-vector 729 #f))
                (set! row-starts (make-vector 729 #f))

                ;; Read puzzle
                (read-matrix filename)
                (print-puzzle)

                ;; Build DLX matrix
                (init-dlx-matrix!)
                (build-dlx-matrix-from-puzzle!)
                (cover-clues!)

                ;; Solve
                (let ((solution (make-vector 81 -1)))
                  (if (dlx-search! solution 0)
                      (let ((solved (extract-solution solution)))
                        ;; Print solved puzzle
                        (display "\nPuzzle:\n")
                        (let r-loop ((r 0))
                          (when (< r 9)
                            (let c-loop ((c 0))
                              (when (< c 9)
                                (display (vector-ref solved (+ (* r 9) c)))
                                (display " ")
                                (c-loop (+ c 1))))
                            (newline)
                            (r-loop (+ r 1))))
                        (display "\nSolved in Iterations=")
                        (display dlx-iterations)
                        (display "\n\n"))
                      (display "\nNo solution found\n")))))
            (process-file (+ i 1)))))))
