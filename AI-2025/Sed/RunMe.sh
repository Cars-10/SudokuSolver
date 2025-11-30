#!/bin/bash
cd "$(dirname "$0")"
# Run
# We need to pass the file content to sed.
for file in ../Matrices/*.matrix; do
    echo "$file"
    # Pass filename to sed
    echo "$file" | gsed -f Sudoku.sed
    
    end=$(date +%s.%N)
    
    end=$(date +%s.%N)
    
    dt=$(echo "$end - $start" | bc)
    printf "Seconds to process %.3f\n" "$dt"
done | tee run.txt
