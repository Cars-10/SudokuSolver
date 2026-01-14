#!/usr/bin/env racket
#lang racket

;;; Sudoku CP Solver in Racket
;;; Constraint Propagation with MRV heuristic using vectors and bitsets

;; ============================================================================
;; GLOBAL STATE
;; ============================================================================

(define cp-iterations 0)

;; ============================================================================
;; GRID STRUCTURE
;; ============================================================================
;; Grid is a 2-element vector: [values candidates]
;; values[r][c] = assigned digit (0 if unassigned)
;; candidates[r][c] = bitset where bit d is set if digit d is possible

(define (make-grid)
  (define g (vector (make-vector 9) (make-vector 9)))
  (for ([i (in-range 9)])
    (vector-set! (vector-ref g 0) i (make-vector 9 0))
    (vector-set! (vector-ref g 1) i (make-vector 9 0)))
  g)

(define (grid-vals g) (vector-ref g 0))
(define (grid-cands g) (vector-ref g 1))
(define (get-val g r c) (vector-ref (vector-ref (grid-vals g) r) c))
(define (set-val! g r c v) (vector-set! (vector-ref (grid-vals g) r) c v))
(define (get-cand g r c) (vector-ref (vector-ref (grid-cands g) r) c))
(define (set-cand! g r c v) (vector-set! (vector-ref (grid-cands g) r) c v))

;; ============================================================================
;; BITSET OPERATIONS
;; ============================================================================

(define (has-cand? set d) (not (= (bitwise-and set (arithmetic-shift 1 d)) 0)))
(define (rem-cand set d) (bitwise-and set (bitwise-not (arithmetic-shift 1 d))))
(define (count-cands set)
  (let loop ([s set] [cnt 0])
    (if (= s 0)
        cnt
        (loop (arithmetic-shift s -1) (+ cnt (bitwise-and s 1))))))
(define (first-cand set)
  (let loop ([d 1])
    (cond
      [(> d 9) 0]
      [(has-cand? set d) d]
      [else (loop (+ d 1))])))

;; ============================================================================
;; GRID OPERATIONS
;; ============================================================================

(define (copy-grid g)
  (define new-g (make-grid))
  (for ([r (in-range 9)])
    (for ([c (in-range 9)])
      (set-val! new-g r c (get-val g r c))
      (set-cand! new-g r c (get-cand g r c))))
  new-g)

(define (init-grid! g puzzle)
  (for ([r (in-range 9)])
    (for ([c (in-range 9)])
      (define v (vector-ref (vector-ref puzzle r) c))
      (if (= v 0)
          (begin
            (set-val! g r c 0)
            (set-cand! g r c #x3FE))  ; bits 1-9 set
          (begin
            (set-val! g r c v)
            (set-cand! g r c (arithmetic-shift 1 v)))))))

;; ============================================================================
;; CONSTRAINT PROPAGATION
;; ============================================================================

(define (get-peers r c)
  (define peers '())
  ;; Row peers
  (for ([col (in-range 9)])
    (when (not (= col c))
      (set! peers (cons (cons r col) peers))))
  ;; Column peers
  (for ([row (in-range 9)])
    (when (not (= row r))
      (set! peers (cons (cons row c) peers))))
  ;; Box peers
  (define br (* (quotient r 3) 3))
  (define bc (* (quotient c 3) 3))
  (for ([rr (in-range br (+ br 3))])
    (for ([cc (in-range bc (+ bc 3))])
      (when (and (not (= rr r)) (not (= cc c)))
        (set! peers (cons (cons rr cc) peers)))))
  peers)

(define (eliminate! g r c d)
  (if (not (has-cand? (get-cand g r c) d))
      #t
      (begin
        (set-cand! g r c (rem-cand (get-cand g r c) d))
        (let ([rem (count-cands (get-cand g r c))])
          (cond
            [(= rem 0) #f]
            [(and (= rem 1) (= (get-val g r c) 0))
             (assign! g r c (first-cand (get-cand g r c)))]
            [else #t])))))

(define (assign! g r c d)
  (set! cp-iterations (+ cp-iterations 1))
  (set-val! g r c d)
  (set-cand! g r c (arithmetic-shift 1 d))
  (let loop ([peers (get-peers r c)])
    (if (null? peers)
        #t
        (let* ([p (car peers)]
               [pr (car p)]
               [pc (cdr p)])
          (if (eliminate! g pr pc d)
              (loop (cdr peers))
              #f)))))

(define (propagate! g)
  (let outer ([changed #t])
    (if (not changed)
        #t
        (begin
          (set! changed #f)
          ;; Strategy 1: Naked singles (cells with only one candidate)
          (for ([row (in-range 9)])
            (for ([col (in-range 9)])
              (when (= (get-val g row col) 0)
                (let ([rem (count-cands (get-cand g row col))])
                  (when (= rem 1)
                    (when (assign! g row col (first-cand (get-cand g row col)))
                      (set! changed #t)))))))
          ;; Strategy 2: Check rows for hidden singles
          (for ([row (in-range 9)])
            (for ([d (in-range 1 10)])
              (let ([cnt 0]
                    [lcol -1]
                    [assigned #f])
                (for ([col (in-range 9)])
                  (when (= (get-val g row col) d)
                    (set! assigned #t))
                  (when (and (not assigned) (has-cand? (get-cand g row col) d))
                    (set! cnt (+ cnt 1))
                    (set! lcol col)))
                (when (and (not assigned) (= cnt 1))
                  (when (assign! g row lcol d)
                    (set! changed #t))))))
          ;; Check columns for hidden singles
          (for ([col (in-range 9)])
            (for ([d (in-range 1 10)])
              (let ([cnt 0]
                    [lrow -1]
                    [assigned #f])
                (for ([row (in-range 9)])
                  (when (= (get-val g row col) d)
                    (set! assigned #t))
                  (when (and (not assigned) (has-cand? (get-cand g row col) d))
                    (set! cnt (+ cnt 1))
                    (set! lrow row)))
                (when (and (not assigned) (= cnt 1))
                  (when (assign! g lrow col d)
                    (set! changed #t))))))
          ;; Check boxes for hidden singles
          (for ([box (in-range 9)])
            (let ([br (* (quotient box 3) 3)]
                  [bc (* (modulo box 3) 3)])
              (for ([d (in-range 1 10)])
                (let ([cnt 0]
                      [lr -1]
                      [lc -1]
                      [assigned #f])
                  (let/ec break
                    (for ([r (in-range br (+ br 3))])
                      (for ([c (in-range bc (+ bc 3))])
                        (when (= (get-val g r c) d)
                          (set! assigned #t)
                          (break #t))
                        (when (and (not assigned) (has-cand? (get-cand g r c) d))
                          (set! cnt (+ cnt 1))
                          (set! lr r)
                          (set! lc c)))))
                  (when (and (not assigned) (= cnt 1))
                    (when (assign! g lr lc d)
                      (set! changed #t)))))))
          (outer changed)))))

;; ============================================================================
;; SEARCH WITH MRV HEURISTIC
;; ============================================================================

(define (find-mrv g)
  (let ([min-c 10]
        [best-r -1]
        [best-c -1])
    (for ([r (in-range 9)])
      (for ([c (in-range 9)])
        (when (= (get-val g r c) 0)
          (let ([nc (count-cands (get-cand g r c))])
            (when (< nc min-c)
              (set! min-c nc)
              (set! best-r r)
              (set! best-c c))))))
    (if (= best-r -1) #f (cons best-r best-c))))

(define (cp-search! g)
  (define mrv (find-mrv g))
  (if (not mrv)
      g
      (let* ([r (car mrv)]
             [c (cdr mrv)]
             [cands (get-cand g r c)])
        (let try ([d 1])
          (cond
            [(> d 9) #f]
            [(has-cand? cands d)
             (let ([g-copy (copy-grid g)])
               (if (and (assign! g r c d) (propagate! g))
                   (let ([res (cp-search! g)])
                     (if res
                         res
                         (begin
                           (set! g (copy-grid g-copy))
                           (try (+ d 1)))))
                   (begin
                     (set! g (copy-grid g-copy))
                     (try (+ d 1)))))]
            [else (try (+ d 1))])))))

;; ============================================================================
;; PUZZLE I/O
;; ============================================================================

(define (read-puzzle fname)
  (define puz (make-vector 9))
  (for ([i (in-range 9)])
    (vector-set! puz i (make-vector 9 0)))

  (if (string-prefix? fname "/app/Matrices/")
      (displayln (string-append "../../../Matrices/" (substring fname 14)))
      (displayln fname))

  (with-input-from-file fname
    (lambda ()
      (let row-loop ([row 0])
        (when (< row 9)
          (let ([line (read-line)])
            (if (eof-object? line)
                #t
                (let ([trimmed (string-trim line)])
                  (cond
                    [(or (string=? trimmed "") (string-prefix? trimmed "#"))
                     (row-loop row)]
                    [else
                     (let ([nums (filter (lambda (s) (not (string=? s "")))
                                        (string-split trimmed))])
                       (for ([col (in-range 9)])
                         (when (< col (length nums))
                           (let ([n (string->number (list-ref nums col))])
                             (vector-set! (vector-ref puz row) col n)
                             (display n)
                             (display " "))))
                       (displayln "")
                       (row-loop (+ row 1)))]))))))))
  puz)

(define (print-grid g)
  (displayln "\nPuzzle:")
  (for ([r (in-range 9)])
    (for ([c (in-range 9)])
      (display (get-val g r c))
      (display " "))
    (displayln "")))

;; ============================================================================
;; MAIN
;; ============================================================================

(define args (current-command-line-arguments))
(when (< (vector-length args) 1)
  (displayln "Usage: racket cp.rkt <matrix_file>" (current-error-port))
  (exit 1))

;; Process each matrix file
(for ([i (in-range (vector-length args))])
  (let ([fname (vector-ref args i)])
    (when (string-suffix? fname ".matrix")
      (set! cp-iterations 0)
      (let ([puz (read-puzzle fname)]
            [g (make-grid)])
        (init-grid! g puz)
        (print-grid g)

        ;; Solve (propagate will find initial clues as hidden singles and assign them)
        (if (propagate! g)
            (let ([sol (cp-search! g)])
              (if sol
                  (begin
                    (print-grid sol)
                    (displayln (string-append "\nSolved in Iterations=" (number->string cp-iterations) "\n")))
                  (displayln "\nNo solution\n")))
            (displayln "\nContradiction\n"))))))
