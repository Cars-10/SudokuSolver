#!/bin/bash
cd "$(dirname "$0")"

# Run
for file in ../Matrices/*.matrix; do
    # Preprocess to flat list of numbers for stats
    grep -v "^#" "$file" | tr -s ' \t\n' '\n' | grep -v "^$" > temp.dat
    
    start=$(date +%s.%N)
    gnuplot -e "filename='temp.dat'" Sudoku.gp
    end=$(date +%s.%N)
    
    dt=$(echo "$end - $start" | bc)
    printf "Seconds to process %.3f\n" "$dt"
done
rm temp.dat
