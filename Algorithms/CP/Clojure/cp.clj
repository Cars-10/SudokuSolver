;;; Constraint Propagation (CP) Sudoku Solver - Clojure Implementation
;;; Port of CP algorithm from C reference
;;;
;;; Algorithm: Backtracking with constraint propagation
;;; - Uses bitsets to track candidate values per cell
;;; - MRV (Minimum Remaining Values) heuristic for cell selection
;;; - Propagates constraints after each assignment
;;; Expected iterations for Matrix 1: 67

(ns cp
  (:require [clojure.string :as str])
  (:gen-class))

;; ============================================================================
;; DATA STRUCTURES
;; ============================================================================

;; Using Java arrays for mutable grid state
;; values: 2D int array - 0 = empty, 1-9 = assigned
;; candidates: 2D short array - bitset where bit N represents digit N is possible

(def puzzle (make-array Integer/TYPE 9 9))
(def cp-iterations (atom 0))

;; ============================================================================
;; BITSET OPERATIONS
;; ============================================================================

(defn has-candidate? [bits digit]
  (not= 0 (bit-and bits (bit-shift-left 1 digit))))

(defn add-candidate [bits digit]
  (unchecked-short (bit-or bits (bit-shift-left 1 digit))))

(defn remove-candidate [bits digit]
  (unchecked-short (bit-and bits (bit-not (bit-shift-left 1 digit)))))

(defn count-candidates [bits]
  (Integer/bitCount (bit-and bits 0xFFFF)))

(defn get-first-candidate [bits]
  (loop [digit 1]
    (if (<= digit 9)
      (if (has-candidate? bits digit)
        digit
        (recur (inc digit)))
      0)))

;; ============================================================================
;; GRID OPERATIONS
;; ============================================================================

(defn create-grid []
  {:values (make-array Integer/TYPE 9 9)
   :candidates (make-array Short/TYPE 9 9)})

;; Helper functions for array access
(defn get-value [grid row col]
  (aget ^ints (aget ^objects (:values grid) row) col))

(defn set-value! [grid row col val]
  (aset ^ints (aget ^objects (:values grid) row) col val))

(defn get-candidates [grid row col]
  (aget ^shorts (aget ^objects (:candidates grid) row) col))

(defn set-candidates! [grid row col cands]
  (aset ^shorts (aget ^objects (:candidates grid) row) col cands))

(defn copy-grid [grid]
  (let [new-grid (create-grid)]
    (dotimes [r 9]
      (dotimes [c 9]
        (set-value! new-grid r c (get-value grid r c))
        (set-candidates! new-grid r c (get-candidates grid r c))))
    new-grid))

(defn init-grid! [grid puzzle-array]
  "Initialize grid from puzzle - empty cells get all candidates (0x3FE)"
  (dotimes [row 9]
    (dotimes [col 9]
      (let [val (aget puzzle-array row col)]
        (if (= val 0)
          (do
            (set-value! grid row col 0)
            (set-candidates! grid row col (short 0x3FE))) ; bits 1-9 set
          (do
            (set-value! grid row col val)
            (set-candidates! grid row col (short (bit-shift-left 1 val)))))))))

;; ============================================================================
;; PEER OPERATIONS
;; ============================================================================

(defn get-peers [^long row ^long col]
  "Get all 20 peers for a cell (same row, col, or 3x3 box)"
  (let [peers (java.util.ArrayList.)]
    ;; Same row (8 cells, excluding self)
    (dotimes [c 9]
      (when (not= c col)
        (.add peers [row c])))

    ;; Same column (8 cells, excluding self)
    (dotimes [r 9]
      (when (not= r row)
        (.add peers [r col])))

    ;; Same 3x3 box (4 cells, excluding self and already counted)
    (let [box-row (* (quot row 3) 3)
          box-col (* (quot col 3) 3)]
      (dotimes [r 3]
        (dotimes [c 3]
          (let [pr (+ box-row r)
                pc (+ box-col c)]
            (when (and (not= pr row) (not= pc col))
              (.add peers [pr pc]))))))

    peers))

;; ============================================================================
;; CONSTRAINT PROPAGATION
;; ============================================================================

;; Forward declaration
(declare assign!)

(defn eliminate! [grid row col digit]
  "Remove digit from candidates at (row,col) and propagate"
  (let [curr-candidates (get-candidates grid row col)]

    (if (not (has-candidate? curr-candidates digit))
      ;; Already eliminated
      true

      ;; Remove the candidate
      (let [new-candidates (remove-candidate curr-candidates digit)]
        (set-candidates! grid row col new-candidates)

        (cond
          ;; Contradiction: no candidates left
          (= 0 new-candidates)
          false

          ;; Singleton: only one candidate left, assign it (if not already assigned)
          (= 1 (count-candidates new-candidates))
          (if (= 0 (get-value grid row col))
            (let [value (get-first-candidate new-candidates)]
              (assign! grid row col value))
            true)

          ;; Still multiple candidates
          :else true)))))

(defn assign! [grid row col digit]
  "Assign digit to (row,col) and propagate constraints"
  (swap! cp-iterations inc)

  ;; Set value and candidates
  (set-value! grid row col digit)
  (set-candidates! grid row col (short (bit-shift-left 1 digit)))

  ;; Eliminate digit from all peers
  (let [peers (get-peers row col)]
    (loop [i 0]
      (if (< i (.size peers))
        (let [[pr pc] (.get peers i)]
          (if (eliminate! grid pr pc digit)
            (recur (inc i))
            false))
        true))))

(defn propagate! [grid]
  "Initial constraint propagation - eliminate given clues from their peers"
  ;; First, eliminate clue values from their peers
  (loop [row 0]
    (when (< row 9)
      (loop [col 0]
        (when (< col 9)
          (let [val (get-value grid row col)]
            (when (not= val 0)
              ;; Eliminate this value from all peers
              (let [peers (get-peers row col)]
                (loop [i 0]
                  (when (< i (.size peers))
                    (let [[pr pc] (.get peers i)]
                      (when-not (eliminate! grid pr pc val)
                        (throw (Exception. "Contradiction during initial propagation"))))
                    (recur (inc i)))))))
          (recur (inc col))))
      (recur (inc row))))

  ;; Then loop until no more singleton eliminations
  (loop [changed true]
    (when changed
      (let [new-changed (atom false)]
        (dotimes [row 9]
          (dotimes [col 9]
            (when (= 0 (get-value grid row col))
              (let [num-cands (count-candidates (get-candidates grid row col))]
                (cond
                  (= 0 num-cands)
                  (throw (Exception. "Contradiction: no candidates left"))

                  (= 1 num-cands)
                  (let [digit (get-first-candidate (get-candidates grid row col))]
                    (when (assign! grid row col digit)
                      (reset! new-changed true))))))))
        (recur @new-changed))))

  true)

;; ============================================================================
;; SEARCH
;; ============================================================================

(defn find-mrv-cell [grid]
  "Find empty cell with minimum remaining values (MRV heuristic)"
  (loop [r 0
         min-count 10
         min-cell nil]
    (if (< r 9)
      (loop [c 0
             min-count min-count
             min-cell min-cell]
        (if (< c 9)
          (if (= 0 (get-value grid r c))
            (let [count (count-candidates (get-candidates grid r c))]
              (if (and (> count 0) (< count min-count))
                (recur (inc c) count [r c])
                (recur (inc c) min-count min-cell)))
            (recur (inc c) min-count min-cell))
          (recur (inc r) min-count min-cell)))
      min-cell)))

(defn cp-search! [grid]
  "Recursive search with backtracking and constraint propagation"
  (let [cell (find-mrv-cell grid)]
    (if (nil? cell)
      ;; No empty cells - solved!
      grid

      ;; Try each candidate for this cell
      (let [[row col] cell
            candidates (get-candidates grid row col)]
        (loop [digit 1]
          (when (<= digit 9)
            (if (has-candidate? candidates digit)
              ;; Try this digit
              (let [new-grid (copy-grid grid)]
                (if (assign! new-grid row col digit)
                  ;; Assignment succeeded, recurse
                  (let [result (cp-search! new-grid)]
                    (if result
                      result
                      (recur (inc digit))))
                  ;; Assignment failed, try next
                  (recur (inc digit))))
              ;; Not a candidate, try next
              (recur (inc digit)))))))))

;; ============================================================================
;; PUZZLE I/O
;; ============================================================================

(defn print-grid [grid]
  "Print grid in standard format"
  (println)
  (println "Puzzle:")
  (dotimes [r 9]
    (dotimes [c 9]
      (print (str (get-value grid r c) " ")))
    (println)))

(defn normalize-path [filename]
  "Normalize path for output (convert /app/Matrices to ../Matrices)"
  (if (str/starts-with? filename "/app/Matrices/")
    (str "../Matrices/" (subs filename 14))
    filename))

(defn read-matrix! [filename]
  "Read matrix file and populate puzzle array"
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
;; MAIN SOLVER
;; ============================================================================

(defn solve-sudoku! []
  "Main solver entry point"
  (let [grid (create-grid)]
    ;; Initialize grid from puzzle
    (init-grid! grid puzzle)
    (print-grid grid)

    ;; Reset iteration counter
    (reset! cp-iterations 0)

    ;; Initial propagation
    (if (propagate! grid)
      ;; Search for solution
      (let [solution (cp-search! grid)]
        (when solution
          (print-grid solution)
          (println)
          (println (str "Solved in Iterations=" @cp-iterations))
          (println)))
      (println "No solution found (initial propagation failed)"))))

(defn process-matrix [filename]
  "Process a single matrix file"
  (when (str/ends-with? filename ".matrix")
    (read-matrix! filename)
    (solve-sudoku!)))

;; ============================================================================
;; MAIN ENTRY POINT
;; ============================================================================

(defn -main [& args]
  (when (empty? args)
    (binding [*out* *err*]
      (println "Usage: clojure -M cp.clj <matrix_file>")
      (System/exit 1)))

  (doseq [filename args]
    (process-matrix filename)))

;; Run main with command line args
(apply -main *command-line-args*)
