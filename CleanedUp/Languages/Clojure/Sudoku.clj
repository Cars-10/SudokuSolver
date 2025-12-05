(ns Sudoku
  (:gen-class))

(def ^:dynamic *iterations* (atom 0))

(defn print-board [board]
  (println "Puzzle:")
  (doseq [row board]
    (println (clojure.string/join " " row))))

(defn is-possible [board row col num]
  (let [row-vals (get board row)
        col-vals (map #(get % col) board)
        start-row (* 3 (quot row 3))
        start-col (* 3 (quot col 3))
        subgrid-vals (for [r (range start-row (+ start-row 3))
                           c (range start-col (+ start-col 3))]
                       (get-in board [r c]))]
    (not (or (some #{num} row-vals)
             (some #{num} col-vals)
             (some #{num} subgrid-vals)))))

(defn solve [board row col]
  (if (= row 9)
    board
    (let [next-row (if (= col 8) (inc row) row)
          next-col (if (= col 8) 0 (inc col))]
      (if (not= 0 (get-in board [row col]))
        (recur board next-row next-col)
        (loop [n 1]
          (if (> n 9)
            nil
            (do
              (swap! *iterations* inc)
              (if (is-possible board row col n)
                (let [new-board (assoc-in board [row col] n)
                      result (solve new-board next-row next-col)]
                  (if result
                    result
                    (recur (inc n))))
                (recur (inc n))))))))))

(defn parse-board [filename]
  (let [content (slurp filename)
        lines (clojure.string/split-lines content)
        valid-lines (filter #(and (not (clojure.string/starts-with? % "#"))
                                  (not (clojure.string/blank? %))) lines)]
    (vec (map (fn [line]
                (vec (map #(Integer/parseInt %) (clojure.string/split (clojure.string/trim line) #"\s+"))))
              (take 9 valid-lines)))))

(defn -main [& args]
  (if (empty? args)
    (println "Usage: clojure Sudoku.clj <file1> <file2> ...")
    (doseq [filename args]
      (println (str "\nProcessing " filename))
      (try
        (let [board (parse-board filename)]
          (print-board board)
          (reset! *iterations* 0)
          (let [solved-board (solve board 0 0)]
            (if solved-board
              (do
                (print-board solved-board)
                (println (str "\nSolved in Iterations=" @*iterations*)))
              (println "No solution found"))))
        (catch Exception e
          (println (str "Error processing file " filename ": " (.getMessage e))))))))

(apply -main *command-line-args*)
