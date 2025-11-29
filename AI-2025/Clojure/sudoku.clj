(ns sudoku
  (:require [clojure.string :as str]))

(def puzzle (atom []))
(def count-iter (atom 0))

(defn print-puzzle [board]
  (println "\nPuzzle:")
  (doseq [row board]
    (println (str/join " " row))))

(defn parse-board [content]
  (let [lines (str/split content #"\n")]
    (->> lines
         (remove #(or (str/starts-with? % "#") (str/blank? %)))
         (map #(str/split (str/trim %) #"\s+"))
         (filter #(= (count %) 9))
         (take 9)
         (mapv (fn [row] (mapv #(Integer/parseInt %) row))))))

(defn is-possible [board r c val]
  (and (not-any? #(= % val) (get board r))
       (not-any? #(= % val) (map #(get % c) board))
       (let [r0 (* (quot r 3) 3)
             c0 (* (quot c 3) 3)]
         (not-any? #(= % val)
                   (for [i (range 3) j (range 3)]
                     (get-in board [(+ r0 i) (+ c0 j)]))))))

(defn solve [board]
  (loop [b board]
    (if-let [[r c] (first (for [i (range 9) j (range 9)
                                :when (zero? (get-in b [i j]))]
                            [i j]))]
      (some (fn [val]
              (swap! count-iter inc)
              (if (is-possible b r c val)
                (solve (assoc-in b [r c] val))))
            (range 1 10))
      b)))

(defn process-file [filename]
  (println filename)
  (let [content (slurp filename)
        board (parse-board content)]
    (print-puzzle board)
    (reset! count-iter 0)
    (if-let [solved (solve board)]
      (do
        (print-puzzle solved)
        (println (str "Solved in Iterations=" @count-iter "\n")))
      (println "No solution found"))))

(defn -main [& args]
  (let [start (System/nanoTime)]
    (doseq [arg args]
      (when (str/ends-with? arg ".matrix")
        (process-file arg)))
    (let [end (System/nanoTime)]
      (printf "Seconds to process %.3f\n" (/ (- end start) 1e9)))))

(apply -main *command-line-args*)
