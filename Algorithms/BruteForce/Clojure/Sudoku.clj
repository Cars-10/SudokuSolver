;;; Sudoku Solver in Clojure
;;; Brute-force backtracking algorithm matching C reference exactly

(ns Sudoku
  (:require [clojure.string :as str])
  (:gen-class))

;; Mutable counter using atom
(def count-iterations (atom 0))

;; Puzzle state (vector of vectors for immutability)
(defn create-puzzle []
  (vec (repeat 9 (vec (repeat 9 0)))))

(defn get-cell [puzzle row col]
  (get-in puzzle [row col]))

(defn set-cell [puzzle row col val]
  (assoc-in puzzle [row col] val))

(defn print-puzzle [puzzle]
  (println)
  (println "Puzzle:")
  (doseq [row puzzle]
    (println (str/join " " row))))

(defn normalize-path [filename]
  "Normalize path for output (convert /app/Matrices to ../Matrices)"
  (if (str/starts-with? filename "/app/Matrices/")
    (str "../Matrices/" (subs filename 14))
    filename))

(defn read-matrix [filename]
  (println (normalize-path filename))
  (let [lines (str/split-lines (slurp filename))
        puzzle (atom (create-puzzle))
        row (atom 0)]
    (doseq [line lines]
      (let [trimmed (str/trim line)]
        (when (and (not (str/blank? trimmed))
                   (not (str/starts-with? trimmed "#"))
                   (< @row 9))
          (let [numbers (mapv #(Integer/parseInt %)
                              (filter #(not (str/blank? %))
                                      (str/split trimmed #"\s+")))]
            (when (= (count numbers) 9)
              (dotimes [col 9]
                (swap! puzzle set-cell @row col (nth numbers col))
                (print (str (nth numbers col) " ")))
              (println)
              (swap! row inc))))))
    @puzzle))

(defn is-valid [puzzle row col val]
  ;; Check row
  (let [row-valid (not (some #(= % val) (get puzzle row)))]
    ;; Check column
    (let [col-valid (not (some #(= (get-cell puzzle % col) val) (range 9)))]
      ;; Check 3x3 box
      (let [box-row (* (quot row 3) 3)
            box-col (* (quot col 3) 3)
            box-valid (not (some (fn [[br bc]]
                                   (= (get-cell puzzle (+ box-row br) (+ box-col bc)) val))
                                 (for [br (range 3) bc (range 3)] [br bc])))]
        (and row-valid col-valid box-valid)))))

(defn find-empty [puzzle]
  "Find first empty cell in row-major order. Returns [row col] or nil."
  (first
   (for [r (range 9)
         c (range 9)
         :when (= (get-cell puzzle r c) 0)]
     [r c])))

(defn solve [puzzle]
  (let [empty (find-empty puzzle)]
    (if (nil? empty)
      ;; No empty cell - puzzle is solved
      (do
        (print-puzzle puzzle)
        (println)
        (println (str "Solved in Iterations=" @count-iterations))
        (println)
        puzzle)
      ;; Try values 1-9
      (let [[row col] empty]
        (loop [val 1]
          (when (<= val 9)
            (swap! count-iterations inc)  ; Count EVERY attempt
            (if (is-valid puzzle row col val)
              (let [new-puzzle (set-cell puzzle row col val)
                    result (solve new-puzzle)]
                (if result
                  result
                  (recur (inc val))))
              (recur (inc val)))))))))

(defn process-matrix [filename]
  (when (str/ends-with? filename ".matrix")
    (let [puzzle (read-matrix filename)]
      (print-puzzle puzzle)
      (reset! count-iterations 0)
      (when-not (solve puzzle)
        (println "No solution found")))))

(defn -main [& args]
  (when (empty? args)
    (binding [*out* *err*]
      (println "Usage: clojure -M Sudoku.clj <matrix_file>")
      (System/exit 1)))

  (doseq [filename args]
    (process-matrix filename)))

;; Run main with command line args
(apply -main *command-line-args*)
