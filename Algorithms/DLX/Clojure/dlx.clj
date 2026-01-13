;;; Dancing Links (DLX) Sudoku Solver - Clojure Implementation
;;; Port of Algorithm X with Dancing Links from C reference
;;;
;;; Algorithm X: Exact cover problem solver using Knuth's Dancing Links technique
;;; Sudoku as Exact Cover: 324 constraints (81 cells, 81 rows, 81 cols, 81 boxes)
;;; Expected iterations for Matrix 1: 43

(ns dlx
  (:require [clojure.string :as str])
  (:gen-class))

;; ============================================================================
;; DATA STRUCTURES
;; ============================================================================

;; Using Java arrays for mutable node structures
;; Each node is a 6-element array: [up down left right column rowId]
;; Each column is an 8-element array: [up down left right column rowId name size]

(defn create-node ^"[Ljava.lang.Object;" []
  (let [node (object-array 6)]
    ;; Self-reference initially
    (aset node 0 node) ;; up
    (aset node 1 node) ;; down
    (aset node 2 node) ;; left
    (aset node 3 node) ;; right
    (aset node 4 nil)  ;; column
    (aset node 5 0)    ;; rowId
    node))

(defn create-column ^"[Ljava.lang.Object;" [^String name]
  (let [col (object-array 8)]
    (aset col 0 col)   ;; up
    (aset col 1 col)   ;; down
    (aset col 2 col)   ;; left
    (aset col 3 col)   ;; right
    (aset col 4 col)   ;; column (self)
    (aset col 5 -1)    ;; rowId
    (aset col 6 name)  ;; name
    (aset col 7 0)     ;; size
    col))

;; Field accessors
(defn node-up [node] (aget node 0))
(defn node-down [node] (aget node 1))
(defn node-left [node] (aget node 2))
(defn node-right [node] (aget node 3))
(defn node-column [node] (aget node 4))
(defn node-rowId [node] (int (aget node 5)))

(defn set-node-up! [node val] (aset node 0 val))
(defn set-node-down! [node val] (aset node 1 val))
(defn set-node-left! [node val] (aset node 2 val))
(defn set-node-right! [node val] (aset node 3 val))
(defn set-node-column! [node val] (aset node 4 val))
(defn set-node-rowId! [node val] (aset node 5 (int val)))

(defn col-size [col] (int (aget col 7)))
(defn set-col-size! [col val] (aset col 7 (int val)))

;; RowInfo - metadata to map DLX rows back to Sudoku placements
(defrecord RowInfo [row col num])

;; ============================================================================
;; GLOBAL STATE
;; ============================================================================

(def puzzle (make-array Integer/TYPE 9 9))
(def solution-grid (make-array Integer/TYPE 9 9))
(def dlx-iterations (atom 0))

(def root (atom nil))
(def columns (make-array Object 324))
(def row-info (make-array RowInfo 729))
(def row-starts (make-array Object 729))

;; ============================================================================
;; CONSTRAINT COLUMN CALCULATIONS
;; ============================================================================

(defn get-position-col ^long [^long r ^long c]
  (+ (* r 9) c))

(defn get-row-col ^long [^long r ^long n]
  (+ 81 (* r 9) (dec n)))

(defn get-col-col ^long [^long c ^long n]
  (+ 162 (* c 9) (dec n)))

(defn get-box-col ^long [^long r ^long c ^long n]
  (let [box (+ (* (quot r 3) 3) (quot c 3))]
    (+ 243 (* box 9) (dec n))))

;; ============================================================================
;; DLX MATRIX INITIALIZATION
;; ============================================================================

(defn init-dlx-matrix!
  "Initialize the DLX matrix with root and 324 column headers"
  []
  (let [root-col (create-column "root")]
    (reset! root root-col)

    ;; Create 324 column headers
    (dotimes [i 324]
      (let [col (create-column (str "C" i))]
        (aset columns i col)

        ;; Link into header list (insert at end, before root)
        (set-node-left! col (node-left root-col))
        (set-node-right! col root-col)
        (set-node-right! (node-left root-col) col)
        (set-node-left! root-col col)))))

(defn add-node!
  "Add a node to a column's circular list"
  [col ^long row-id]
  (let [node (create-node)]
    (set-node-column! node col)
    (set-node-rowId! node row-id)

    ;; Insert at end of column's circular list (before column header)
    (set-node-down! node col)
    (set-node-up! node (node-up col))
    (set-node-down! (node-up col) node)
    (set-node-up! col node)
    (set-col-size! col (inc (col-size col)))

    node))

(defn build-dlx-row!
  "Build a DLX row for Sudoku cell (r,c) with value n"
  [^long r ^long c ^long n ^long row-id]
  ;; Store row metadata
  (aset row-info row-id (->RowInfo r c n))

  ;; Create nodes for the 4 constraints
  (let [n1 (add-node! (aget columns (get-position-col r c)) row-id)
        n2 (add-node! (aget columns (get-row-col r n)) row-id)
        n3 (add-node! (aget columns (get-col-col c n)) row-id)
        n4 (add-node! (aget columns (get-box-col r c n)) row-id)]

    ;; Link nodes horizontally in a circular list
    (set-node-right! n1 n2)
    (set-node-right! n2 n3)
    (set-node-right! n3 n4)
    (set-node-right! n4 n1)
    (set-node-left! n2 n1)
    (set-node-left! n3 n2)
    (set-node-left! n4 n3)
    (set-node-left! n1 n4)

    ;; Store first node of this row
    (aset row-starts row-id n1)))

(defn build-dlx-matrix-from-puzzle!
  "Build the DLX matrix based on puzzle constraints"
  []
  (init-dlx-matrix!)

  ;; Build rows based on puzzle state
  (let [row-id (atom 0)]
    (dotimes [r 9]
      (dotimes [c 9]
        (let [val (aget puzzle r c)]
          (if (not= val 0)
            ;; Cell has a clue - create only one row for that value
            (do
              (build-dlx-row! r c val @row-id)
              (swap! row-id inc))
            ;; Cell is empty - create rows for all possible values
            (dotimes [n 9]
              (build-dlx-row! r c (inc n) @row-id)
              (swap! row-id inc))))))))

;; ============================================================================
;; COVER/UNCOVER OPERATIONS
;; ============================================================================

(defn cover-column!
  "Cover a column (remove from matrix)"
  [c]
  ;; Remove column header from header list
  (set-node-right! (node-left c) (node-right c))
  (set-node-left! (node-right c) (node-left c))

  ;; Cover all rows in this column
  (loop [i (node-down c)]
    (when (not (identical? i c))
      ;; For each node in this row
      (loop [j (node-right i)]
        (when (not (identical? j i))
          ;; Remove node from its column
          (let [col (node-column j)]
            (set-node-down! (node-up j) (node-down j))
            (set-node-up! (node-down j) (node-up j))
            (set-col-size! col (dec (col-size col))))
          (recur (node-right j))))
      (recur (node-down i)))))

(defn uncover-column!
  "Uncover a column (restore to matrix) - reverse of cover"
  [c]
  ;; Uncover all rows in this column (in reverse order)
  (loop [i (node-up c)]
    (when (not (identical? i c))
      ;; For each node in this row (reverse direction)
      (loop [j (node-left i)]
        (when (not (identical? j i))
          ;; Restore node to its column
          (let [col (node-column j)]
            (set-col-size! col (inc (col-size col)))
            (set-node-down! (node-up j) j)
            (set-node-up! (node-down j) j))
          (recur (node-left j))))
      (recur (node-up i))))

  ;; Restore column header to header list
  (set-node-right! (node-left c) c)
  (set-node-left! (node-right c) c))

;; ============================================================================
;; SEARCH ALGORITHM
;; ============================================================================

(defn choose-column
  "Choose column with minimum size (S heuristic)"
  []
  (let [root-node @root
        first-col (node-right root-node)]
    (loop [min-col first-col
           j (node-right first-col)]
      (if (identical? j root-node)
        min-col
        (if (< (col-size j) (col-size min-col))
          (recur j (node-right j))
          (recur min-col (node-right j)))))))

(defn dlx-search!
  "Recursive DLX search with backtracking"
  [^long k solution]
  (swap! dlx-iterations inc)

  (let [root-node @root]
    (if (identical? (node-right root-node) root-node)
      ;; Success: all columns covered
      (do
        ;; Extract solution - start with puzzle (includes clues)
        (dotimes [r 9]
          (dotimes [c 9]
            (aset solution-grid r c (aget puzzle r c))))
        ;; Fill in solved cells
        (dotimes [i k]
          (let [row-id (aget solution i)]
            (when (and (>= row-id 0) (< row-id 729))
              (let [info (aget row-info row-id)]
                (aset solution-grid (:row info) (:col info) (:num info))))))
        true)

      ;; Choose column and try all rows
      (let [c (choose-column)]
        (cover-column! c)

        ;; Try each row in this column
        (loop [r (node-down c)]
          (if (identical? r c)
            ;; Backtrack: no solution found
            (do
              (uncover-column! c)
              false)

            ;; Try this row
            (do
              (aset solution k (node-rowId r))

              ;; Cover all columns in this row
              (loop [j (node-right r)]
                (when (not (identical? j r))
                  (cover-column! (node-column j))
                  (recur (node-right j))))

              ;; Recursively search
              (if (dlx-search! (inc k) solution)
                true

                ;; Backtrack: uncover columns
                (do
                  (loop [j (node-left r)]
                    (when (not (identical? j r))
                      (uncover-column! (node-column j))
                      (recur (node-left j))))

                  (recur (node-down r)))))))))))

;; ============================================================================
;; PUZZLE I/O
;; ============================================================================

(defn print-puzzle
  "Print puzzle in standard format"
  []
  (println)
  (println "Puzzle:")
  (dotimes [r 9]
    (dotimes [c 9]
      (print (str (aget puzzle r c) " ")))
    (println)))

(defn print-solution
  "Print solution grid"
  []
  (println)
  (println "Puzzle:")
  (dotimes [r 9]
    (dotimes [c 9]
      (print (str (aget solution-grid r c) " ")))
    (println)))

(defn normalize-path [filename]
  "Normalize path for output (convert /app/Matrices to ../Matrices)"
  (if (str/starts-with? filename "/app/Matrices/")
    (str "../Matrices/" (subs filename 14))
    filename))

(defn read-matrix
  "Read matrix file and populate puzzle array"
  [filename]
  (println (normalize-path filename))
  (let [lines (str/split-lines (slurp filename))]
    (loop [lines lines
           row 0]
      (when (and (< row 9) (seq lines))
        (let [line (str/trim (first lines))]
          (if (and (not (str/blank? line))
                   (not (str/starts-with? line "#")))
            (let [numbers (mapv #(Integer/parseInt %)
                                (filter #(not (str/blank? %))
                                        (str/split line #"\s+")))]
              (when (= (count numbers) 9)
                (dotimes [col 9]
                  (aset puzzle row col (nth numbers col))
                  (print (str (nth numbers col) " ")))
                (println)
                (recur (rest lines) (inc row))))
            (recur (rest lines) row)))))))

;; ============================================================================
;; CONSTRAINT PRE-COVERING
;; ============================================================================

(defn cover-clues!
  "Cover rows for given clues"
  []
  (dotimes [r 9]
    (dotimes [c 9]
      (let [val (aget puzzle r c)]
        (when (not= val 0)
          ;; Find the row for this clue and cover its columns
          (loop [row-id 0]
            (when (< row-id 729)
              (let [node (aget row-starts row-id)
                    info (aget row-info row-id)]
                (when (and node info
                           (= (:row info) r)
                           (= (:col info) c)
                           (= (:num info) val))
                  ;; Cover all columns in this row
                  (loop [curr node]
                    (cover-column! (node-column curr))
                    (let [next-node (node-right curr)]
                      (when (not (identical? next-node node))
                        (recur next-node)))))))
              (recur (inc row-id))))))))

;; ============================================================================
;; MAIN SOLVER
;; ============================================================================

(defn solve-sudoku!
  "Main solver entry point"
  []
  (build-dlx-matrix-from-puzzle!)
  (cover-clues!)

  (let [solution (make-array Integer/TYPE 81)]
    (reset! dlx-iterations 0)
    (let [result (dlx-search! 0 solution)]
      (when result
        (print-solution)
        (println)
        (println (str "Solved in Iterations=" @dlx-iterations))
        (println)))))

(defn process-matrix
  "Process a single matrix file"
  [filename]
  (when (str/ends-with? filename ".matrix")
    (read-matrix filename)
    (print-puzzle)
    (solve-sudoku!)))

;; ============================================================================
;; MAIN ENTRY POINT
;; ============================================================================

(defn -main [& args]
  (when (empty? args)
    (binding [*out* *err*]
      (println "Usage: clojure -M dlx.clj <matrix_file>")
      (System/exit 1)))

  (doseq [filename args]
    (process-matrix filename)))

;; Run main with command line args
(apply -main *command-line-args*)
