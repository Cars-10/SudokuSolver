#!/bin/bash
cd "$(dirname "$0")"
# Run
clojure -M sudoku.clj ../Matrices/*.matrix | tee run.txt
