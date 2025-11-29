#!/bin/bash
cd "$(dirname "$0")"
# Run
# We need to pass the file content to sed.
for file in ../Matrices/*.matrix; do
    echo "$file"
    # Preprocess to single line of numbers
    content=$(grep -v "^#" "$file" | tr -s ' \t\n' ' ')
    
    start=$(date +%s.%N)
    # GNU Sed required for 'e' flag
    echo "$content" | gsed -f Sudoku.sed
    end=$(date +%s.%N)
    
    dt=$(echo "$end - $start" | bc)
    printf "Seconds to process %.3f\n" "$dt"
done
