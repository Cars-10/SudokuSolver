#!/bin/bash
cd "$(dirname "$0")"

# Run
for file in ../Matrices/*.matrix; do
    echo "$file"
    
    start=$(date +%s.%N)
    python3 befunge.py Sudoku.befunge "$file"
    end=$(date +%s.%N)
    
    dt=$(echo "$end - $start" | bc)
    printf "\nSeconds to process %.3f\n" "$dt"
done
