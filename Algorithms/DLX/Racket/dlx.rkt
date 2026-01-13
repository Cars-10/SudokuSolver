#!/usr/bin/env racket
#lang racket

;;; Sudoku DLX Solver in Racket
;;; Dancing Links (Algorithm X) implementation using structs

;; ============================================================================
;; NODE REPRESENTATION USING STRUCTS
;; ============================================================================

(struct node ([left #:mutable]
              [right #:mutable]
              [up #:mutable]
              [down #:mutable]
              [column #:mutable]
              [size #:mutable]
              [row-id #:mutable]
              [col-id #:mutable])
  #:transparent)

;; ============================================================================
;; GLOBAL DATA STRUCTURES
;; ============================================================================

(define dlx-iterations 0)
(define puzzle (make-vector 81 0))
(define root-node #f)
(define column-headers (make-vector 324 #f))
(define row-infos (make-vector 729 #f))  ; Vector of (row col num) lists
(define row-starts (make-vector 729 #f))  ; Vector of first node in each row

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
  (let ([box (+ (* (quotient r 3) 3) (quotient c 3))])
    (+ 243 (* box 9) (- n 1))))

;; ============================================================================
;; DLX MATRIX INITIALIZATION
;; ============================================================================

(define (init-dlx-matrix!)
  ;; Create root node
  (set! root-node (node #f #f #f #f #f 0 -1 -1))
  (set-node-left! root-node root-node)
  (set-node-right! root-node root-node)
  (set-node-up! root-node root-node)
  (set-node-down! root-node root-node)
  (set-node-column! root-node root-node)

  ;; Create 324 column headers
  (for ([i (in-range 324)])
    (define col (node #f #f #f #f #f 0 -1 i))
    (set-node-up! col col)
    (set-node-down! col col)
    (set-node-column! col col)

    ;; Link into header list (insert before root)
    (define left-node (node-left root-node))
    (set-node-left! col left-node)
    (set-node-right! col root-node)
    (set-node-right! left-node col)
    (set-node-left! root-node col)

    (vector-set! column-headers i col)))

;; ============================================================================
;; NODE OPERATIONS
;; ============================================================================

(define (add-node-to-column! col row-id)
  (define new-node (node #f #f #f #f col 0 row-id -1))

  ;; Insert at end of column's circular list
  (define up-node (node-up col))
  (set-node-down! new-node col)
  (set-node-up! new-node up-node)
  (set-node-down! up-node new-node)
  (set-node-up! col new-node)

  ;; Increment column size
  (set-node-size! col (+ (node-size col) 1))
  new-node)

(define (build-dlx-row! r c n row-id)
  ;; Store row info
  (vector-set! row-infos row-id (list r c n))

  ;; Create 4 nodes for the 4 constraints
  (define n1 (add-node-to-column! (vector-ref column-headers (get-position-col r c)) row-id))
  (define n2 (add-node-to-column! (vector-ref column-headers (get-row-col r n)) row-id))
  (define n3 (add-node-to-column! (vector-ref column-headers (get-col-col c n)) row-id))
  (define n4 (add-node-to-column! (vector-ref column-headers (get-box-col r c n)) row-id))

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
  n1)

(define (build-dlx-matrix-from-puzzle!)
  (define row-id 0)
  (for ([r (in-range 9)])
    (for ([c (in-range 9)])
      (define val (get-puzzle-cell r c))
      (if (= val 0)
          ;; Empty cell - create rows for all values 1-9
          (for ([n (in-range 1 10)])
            (build-dlx-row! r c n row-id)
            (set! row-id (+ row-id 1)))
          ;; Clue - create only one row
          (begin
            (build-dlx-row! r c val row-id)
            (set! row-id (+ row-id 1)))))))

;; ============================================================================
;; DLX COVER/UNCOVER OPERATIONS
;; ============================================================================

(define (cover-column! col)
  ;; Remove column from header list
  (define left-node (node-left col))
  (define right-node (node-right col))
  (set-node-right! left-node right-node)
  (set-node-left! right-node left-node)

  ;; For each row in this column
  (let row-loop ([row-node (node-down col)])
    (when (not (eq? row-node col))
      ;; For each node in this row
      (let node-loop ([right-node (node-right row-node)])
        (when (not (eq? right-node row-node))
          ;; Remove node from its column
          (define up-node (node-up right-node))
          (define down-node (node-down right-node))
          (define col-node (node-column right-node))
          (set-node-down! up-node down-node)
          (set-node-up! down-node up-node)
          (set-node-size! col-node (- (node-size col-node) 1))
          (node-loop (node-right right-node))))
      (row-loop (node-down row-node)))))

(define (uncover-column! col)
  ;; For each row in this column (in reverse)
  (let row-loop ([row-node (node-up col)])
    (when (not (eq? row-node col))
      ;; For each node in this row (in reverse)
      (let node-loop ([left-node (node-left row-node)])
        (when (not (eq? left-node row-node))
          ;; Restore node to its column
          (define up-node (node-up left-node))
          (define down-node (node-down left-node))
          (define col-node (node-column left-node))
          (set-node-size! col-node (+ (node-size col-node) 1))
          (set-node-down! up-node left-node)
          (set-node-up! down-node left-node)
          (node-loop (node-left left-node))))
      (row-loop (node-up row-node))))

  ;; Restore column to header list
  (define left-node (node-left col))
  (define right-node (node-right col))
  (set-node-right! left-node col)
  (set-node-left! right-node col))

;; ============================================================================
;; DLX SEARCH (ALGORITHM X)
;; ============================================================================

(define (choose-column)
  ;; Choose column with minimum size (Knuth's S heuristic)
  (define best #f)
  (define min-size 999999)
  (let loop ([col-node (node-right root-node)])
    (when (not (eq? col-node root-node))
      (define size (node-size col-node))
      (when (< size min-size)
        (set! min-size size)
        (set! best col-node))
      (loop (node-right col-node))))
  best)

(define (dlx-search! solution depth)
  (set! dlx-iterations (+ dlx-iterations 1))

  ;; If matrix is empty, we found a solution
  (if (eq? (node-right root-node) root-node)
      #t
      (let ([col (choose-column)])
        (if (or (not col) (= (node-size col) 0))
            #f
            (begin
              ;; Cover this column
              (cover-column! col)

              ;; Try each row in this column
              (let try-row ([row-node (node-down col)])
                (if (eq? row-node col)
                    (begin
                      ;; No solution found, uncover and backtrack
                      (uncover-column! col)
                      #f)
                    (begin
                      ;; Add row to solution
                      (vector-set! solution depth (node-row-id row-node))

                      ;; Cover all other columns in this row
                      (let cover-loop ([right-node (node-right row-node)])
                        (when (not (eq? right-node row-node))
                          (cover-column! (node-column right-node))
                          (cover-loop (node-right right-node))))

                      ;; Recurse
                      (if (dlx-search! solution (+ depth 1))
                          #t
                          (begin
                            ;; Backtrack: uncover columns in reverse
                            (let uncover-loop ([left-node (node-left row-node)])
                              (when (not (eq? left-node row-node))
                                (uncover-column! (node-column left-node))
                                (uncover-loop (node-left left-node))))

                            ;; Try next row
                            (try-row (node-down row-node))))))))))))

;; ============================================================================
;; PUZZLE I/O
;; ============================================================================

(define (print-puzzle)
  (displayln "\nPuzzle:")
  (for ([r (in-range 9)])
    (for ([c (in-range 9)])
      (display (get-puzzle-cell r c))
      (display " "))
    (displayln "")))

(define (read-matrix filename)
  ;; Print filename
  (if (string-prefix? filename "/app/Matrices/")
      (displayln (string-append "../Matrices/" (substring filename 14)))
      (displayln filename))

  (with-input-from-file filename
    (lambda ()
      (let row-loop ([row 0])
        (when (< row 9)
          (define line (read-line))
          (if (eof-object? line)
              #t
              (let ([trimmed (string-trim line)])
                (cond
                  [(or (string=? trimmed "") (string-prefix? trimmed "#"))
                   (row-loop row)]
                  [else
                   (define numbers (filter (lambda (s) (not (string=? s "")))
                                          (string-split trimmed)))
                   (for ([col (in-range 9)])
                     (when (< col (length numbers))
                       (define num (string->number (list-ref numbers col)))
                       (set-puzzle-cell! row col num)
                       (display num)
                       (display " ")))
                   (displayln "")
                   (row-loop (+ row 1))]))))))))

(define (extract-solution solution)
  (define result (make-vector 81 0))
  ;; Copy original puzzle (clues)
  (for ([i (in-range 81)])
    (vector-set! result i (vector-ref puzzle i)))

  ;; Fill in solution
  (for ([i (in-range 81)])
    (define row-id (vector-ref solution i))
    (when (>= row-id 0)
      (define info (vector-ref row-infos row-id))
      (when info
        (match-define (list r c n) info)
        (vector-set! result (+ (* r 9) c) n))))
  result)

(define (cover-clues!)
  ;; Cover columns for given clues
  (for ([r (in-range 9)])
    (for ([c (in-range 9)])
      (define val (get-puzzle-cell r c))
      (when (> val 0)
        ;; Find the row for this clue and cover its columns
        (let find-row ([row-id 0])
          (when (< row-id 729)
            (define info (vector-ref row-infos row-id))
            (if (and info
                     (match info
                       [(list ir ic iv) (and (= ir r) (= ic c) (= iv val))]
                       [_ #f]))
                ;; Found the row - cover all its columns
                (let ([first-node (vector-ref row-starts row-id)])
                  (when first-node
                    (let cover-loop ([node first-node])
                      (cover-column! (node-column node))
                      (define next (node-right node))
                      (when (not (eq? next first-node))
                        (cover-loop next)))))
                (find-row (+ row-id 1)))))))))

;; ============================================================================
;; MAIN
;; ============================================================================

(define args (current-command-line-arguments))
(when (< (vector-length args) 1)
  (displayln "Usage: racket dlx.rkt <matrix_file>" (current-error-port))
  (exit 1))

;; Process each matrix file
(for ([i (in-range (vector-length args))])
  (define filename (vector-ref args i))
  (when (string-suffix? filename ".matrix")
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
    (define solution (make-vector 81 -1))
    (if (dlx-search! solution 0)
        (let ([solved (extract-solution solution)])
          ;; Print solved puzzle
          (displayln "\nPuzzle:")
          (for ([r (in-range 9)])
            (for ([c (in-range 9)])
              (display (vector-ref solved (+ (* r 9) c)))
              (display " "))
            (displayln ""))
          (displayln (string-append "\nSolved in Iterations=" (number->string dlx-iterations) "\n")))
        (displayln "\nNo solution found\n"))))
