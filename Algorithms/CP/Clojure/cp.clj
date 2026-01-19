;;; Constraint Propagation (CP) Sudoku Solver - Clojure Implementation
;;; DEBUG VERSION

(ns cp
  (:require [clojure.string :as str])
  (:gen-class))

(def puzzle (make-array Integer/TYPE 9 9))
(def cp-iterations (atom 0))

(defn create-grid []
  {:values (make-array Integer/TYPE 9 9)
   :candidates (make-array Short/TYPE 9 9)})

(defn get-value [grid row col]
  (aget ^ints (aget ^objects (:values grid) row) col))

(defn set-value! [grid row col val]
  (aset ^ints (aget ^objects (:values grid) row) col val))

(defn get-candidates [grid row col]
  (aget ^shorts (aget ^objects (:candidates grid) row) col))

(defn set-candidates! [grid row col cands]
  (aset ^shorts (aget ^objects (:candidates grid) row) col cands))

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

(defn get-peers [^long row ^long col]
  (let [peers (java.util.ArrayList.)]
    (dotimes [c 9] (when (not= c col) (.add peers [row c])))
    (dotimes [r 9] (when (not= r row) (.add peers [r col])))
    (let [box-row (* (quot row 3) 3) box-col (* (quot col 3) 3)]
      (dotimes [r 3] (dotimes [c 3]
        (let [pr (+ box-row r) pc (+ box-col c)]
          (when (and (not= pr row) (not= pc col)) (.add peers [pr pc]))))))
    peers))

(declare assign!)

(defn eliminate! [grid row col digit]
  (let [curr-candidates (get-candidates grid row col)]
    (if (not (has-candidate? curr-candidates digit))
      true
      (let [new-candidates (remove-candidate curr-candidates digit)]
        (set-candidates! grid row col new-candidates)
        (cond
          (= 0 new-candidates) false
          (= 1 (count-candidates new-candidates))
          (if (= 0 (get-value grid row col))
            (let [value (get-first-candidate new-candidates)]
              (assign! grid row col value))
            true)
          :else true)))))

(defn assign! [grid row col digit]
  (swap! cp-iterations inc)
  (set-value! grid row col digit)
  (set-candidates! grid row col (short (bit-shift-left 1 digit)))
  (let [peers (get-peers row col)]
    (loop [i 0]
      (if (< i (.size peers))
        (let [[pr pc] (.get peers i)]
          (if (eliminate! grid pr pc digit)
            (recur (inc i))
            false))
        true))))

(defn propagate! [grid]
  (println "Propagating...")
  (loop [changed true passes 0]
    (when changed
      ;;(println "Pass" passes)
      (let [new-changed (atom false)]
        (dotimes [row 9]
          (dotimes [col 9]
            (when (= 0 (get-value grid row col))
              (let [num-cands (count-candidates (get-candidates grid row col))]
                (cond
                  (= 0 num-cands) (throw (Exception. "Contradiction: no candidates"))
                  (= 1 num-cands) (let [digit (get-first-candidate (get-candidates grid row col))]
                                    (when (assign! grid row col digit) (reset! new-changed true))))))))
        ;; Strategy 2: Hidden Singles (Rows)
        (dotimes [row 9]
          (dotimes [digit 9]
            (let [d (inc digit) cnt (atom 0) last-c (atom -1) assigned (atom false)]
              (dotimes [col 9]
                (if (= (get-value grid row col) d)
                  (reset! assigned true)
                  (when (has-candidate? (get-candidates grid row col) d)
                    (swap! cnt inc) (reset! last-c col))))
              (if (not @assigned)
                (cond
                  (= 0 @cnt) (throw (Exception. "Contradiction: digit nowhere in row"))
                  (= 1 @cnt) (if (assign! grid row @last-c d) (reset! new-changed true) (throw (Exception. "Contradiction in hidden single"))))))))
        ;; Strategy 2: Hidden Singles (Columns)
        (dotimes [col 9]
          (dotimes [digit 9]
            (let [d (inc digit) cnt (atom 0) last-r (atom -1) assigned (atom false)]
              (dotimes [row 9]
                (if (= (get-value grid row col) d)
                  (reset! assigned true)
                  (when (has-candidate? (get-candidates grid row col) d)
                    (swap! cnt inc) (reset! last-r row))))
              (if (not @assigned)
                (cond
                  (= 0 @cnt) (throw (Exception. "Contradiction: digit nowhere in col"))
                  (= 1 @cnt) (if (assign! grid @last-r col d) (reset! new-changed true) (throw (Exception. "Contradiction in hidden single"))))))))
        ;; Strategy 2: Hidden Singles (Boxes)
        (dotimes [box 9]
          (let [box-r (* (quot box 3) 3) box-c (* (mod box 3) 3)]
            (dotimes [digit 9]
              (let [d (inc digit) cnt (atom 0) last-r (atom -1) last-c (atom -1) assigned (atom false)]
                (dotimes [r 3]
                  (dotimes [c 3]
                    (let [row (+ box-r r) col (+ box-c c)]
                      (if (= (get-value grid row col) d)
                        (reset! assigned true)
                        (when (has-candidate? (get-candidates grid row col) d)
                          (swap! cnt inc) (reset! last-r row) (reset! last-c col))))))
                (if (not @assigned)
                  (cond
                    (= 0 @cnt) (throw (Exception. "Contradiction: digit nowhere in box"))
                    (= 1 @cnt) (if (assign! grid @last-r @last-c d) (reset! new-changed true) (throw (Exception. "Contradiction in hidden single")))))))))
        (recur @new-changed (inc passes)))))
  (println "Propagate done")
  true)

(defn init-grid! [grid puzzle-array]
  (dotimes [row 9]
    (dotimes [col 9]
      (let [val (aget puzzle-array row col)]
        (if (= val 0)
          (do (set-value! grid row col 0) (set-candidates! grid row col (short 0x3FE)))
          (do (set-value! grid row col val) (set-candidates! grid row col (short (bit-shift-left 1 val)))))))))

(defn copy-grid [grid]
  (let [new-grid (create-grid)]
    (dotimes [r 9] (dotimes [c 9]
      (set-value! new-grid r c (get-value grid r c))
      (set-candidates! new-grid r c (get-candidates grid r c))))
    new-grid))

(defn find-mrv-cell [grid]
  (loop [r 0 min-count 10 min-cell nil]
    (if (< r 9)
      (loop [c 0 min-count min-count min-cell min-cell]
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
  (let [cell (find-mrv-cell grid)]
    (if (nil? cell) grid
      (let [[row col] cell candidates (get-candidates grid row col)]
        (loop [digit 1]
          (when (<= digit 9)
            (if (has-candidate? candidates digit)
              (let [new-grid (copy-grid grid)]
                (if (assign! new-grid row col digit)
                  (do 
                    (try 
                      (if (propagate! new-grid)
                        (let [result (cp-search! new-grid)]
                          (if result result (recur (inc digit))))
                        (recur (inc digit)))
                      (catch Exception e (recur (inc digit)))))
                  (recur (inc digit))))
              (recur (inc digit))))))))

(defn solve-sudoku! []
  (let [grid (create-grid)]
    (init-grid! grid puzzle)
    (println "Init done")
    (reset! cp-iterations 0)
    (try
      (if (propagate! grid)
        (do
          (println "Initial propagate success")
          (let [empty-count (atom 0)]
            (dotimes [r 9] (dotimes [c 9] (when (= 0 (get-value grid r c)) (swap! empty-count inc))))
            (if (= 0 @empty-count)
              (println (str "Solved in Iterations=" @cp-iterations))
              (let [solution (cp-search! grid)]
                (if solution (println (str "Solved in Iterations=" @cp-iterations))
                             (println "No solution found"))))))
        (println "No solution found (initial propagation failed)"))
      (catch Exception e (println "No solution found (exception) " (.getMessage e))))))

(defn read-matrix! [filename]
  (println filename)
  (let [lines (str/split-lines (slurp filename))]
    (loop [lines lines row 0]
      (when (and (< row 9) (seq lines))
        (let [line (str/trim (first lines))]
          (if (and (not (str/blank? line)) (not (str/starts-with? line "#")))
            (let [numbers (mapv #(Integer/parseInt %) (filter #(not (str/blank? %)) (str/split line #"\s+")))]
              (when (= (count numbers) 9)
                (dotimes [col 9] (aset puzzle row col (nth numbers col)))
                (recur (rest lines) (inc row))))
            (recur (rest lines) row)))))))

(defn -main [& args]
  (doseq [filename args]
    (read-matrix! filename)
    (solve-sudoku!)))

(apply -main *command-line-args*)