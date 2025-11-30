#!/bin/bash
cd "$(dirname "$0")"
# Run
# Run
for file in ../Matrices/*.matrix; do
    start=$(date +%s.%N)
    awk -f Sudoku.awk "$file"
    end=$(date +%s.%N)
    dt=$(echo "$end - $start" | bc)
    printf "Seconds to process %.3f\n" "$dt"
done | tee run.txt
