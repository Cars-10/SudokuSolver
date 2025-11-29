#!/bin/bash
cd "$(dirname "$0")"

# Compile
iverilog -g2012 -o Sudoku Sudoku.v

# Run
for file in ../Matrices/*.matrix; do
    # Verilog $fscanf is tricky with comments and newlines.
    # We need to clean the input file first to just numbers.
    grep -v "^#" "$file" | tr -s ' \t\n' ' ' > temp.txt
    
    start=$(date +%s.%N)
    ./Sudoku +FILE=temp.txt
    end=$(date +%s.%N)
    
    dt=$(echo "$end - $start" | bc)
    printf "Seconds to process %.3f\n" "$dt"
done
rm temp.txt Sudoku
