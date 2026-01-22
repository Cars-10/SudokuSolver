;;; Constraint Propagation (CP) Sudoku Solver - Clojure Implementation
;;; Algorithm: Constraint propagation with MRV heuristic

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

(defn get-peers [row col]
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
        (let [num (count-candidates new-candidates)]
          (cond
            (= 0 num) false
            (= 1 num) (if (= 0 (get-value grid row col))
                        (assign! grid row col (get-first-candidate new-candidates))
                        true)
            :else true))))))

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
  (loop [changed true]
    (if changed
      (let [new-changed (atom false)]
        ;; Singleton elimination
        (dotimes [r 9]
          (dotimes [c 9]
            (when (= 0 (get-value grid r c))
              (let [num (count-candidates (get-candidates grid r c))]
                (cond
                  (= 0 num) (throw (Exception. "Contradiction"))
                  (= 1 num) (when (assign! grid r c (get-first-candidate (get-candidates grid r c)))
                              (reset! new-changed true)))))))
        ;; Hidden singles
        (doseq [unit-type [:row :col :box]]
          (dotimes [i 9]
            (dotimes [digit 9]
              (let [d (inc digit) 
                    cnt (atom 0) last-pos (atom nil) assigned (atom false)]
                (dotimes [j 9]
                  (let [[r c] (case unit-type
                                :row [i j]
                                :col [j i]
                                :box [(+ (* (quot i 3) 3) (quot j 3)) (+ (* (mod i 3) 3) (mod j 3))])]
                    (if (= (get-value grid r c) d)
                      (reset! assigned true)
                      (when (has-candidate? (get-candidates grid r c) d)
                        (swap! cnt inc) (reset! last-pos [r c])))))
                (when (and (not @assigned) (= 1 @cnt))
                  (let [[r c] @last-pos]
                    (when (assign! grid r c d) (reset! new-changed true))))))))
        (recur @new-changed))
      true)))

(defn init-grid! [grid puzzle-array]
  (dotimes [r 9]
    (dotimes [c 9]
      (let [val (aget puzzle-array r c)]
        (if (= val 0)
          (do (set-value! grid r c 0) (set-candidates! grid r c (short 0x3FE)))
          (do (set-value! grid r c val) (set-candidates! grid r c (short (bit-shift-left 1 val)))))))))

(defn copy-grid [grid]
  (let [new-grid (create-grid)]
    (dotimes [r 9] (dotimes [c 9]
      (set-value! new-grid r c (get-value grid r c))
      (set-candidates! new-grid r c (get-candidates grid r c))))
    new-grid))

(defn find-mrv-cell [grid]
  (let [min-count (atom 10) min-cell (atom nil)]
    (dotimes [r 9]
      (dotimes [c 9]
        (when (= 0 (get-value grid r c))
          (let [cnt (count-candidates (get-candidates grid r c))]
            (when (< cnt @min-count)
              (reset! min-count cnt)
              (reset! min-cell [r c]))))))
    @min-cell))

(defn cp-search! [grid]
  (if-let [cell (find-mrv-cell grid)]
    (let [[r c] cell
          cands (get-candidates grid r c)]
      (loop [digit 1]
        (if (<= digit 9)
          (if (has-candidate? cands digit)
            (let [new-grid (copy-grid grid)]
              (if (try (and (assign! new-grid r c digit) (propagate! new-grid)) (catch Exception e false))
                (if-let [result (cp-search! new-grid)]
                  result
                  (recur (inc digit)))
                (recur (inc digit))))
            (recur (inc digit)))
          nil)))
    grid))

(defn solve-sudoku! []
  (let [grid (create-grid)]
    (init-grid! grid puzzle)
    (reset! cp-iterations 0)
    (try
      (if (and (propagate! grid) (cp-search! grid))
        (println (str "Solved in Iterations=" @cp-iterations))
        (println "No solution found"))
      (catch Exception e (println "No solution found")))))

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
