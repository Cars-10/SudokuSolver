#!/bin/bash
cd "$(dirname "$0")"

# Run
if command -v ucblogo &> /dev/null; then
    for file in ../Matrices/*.matrix; do
        echo "$file"
        
        start=$(date +%s.%N)
        # Run Logo
        ucblogo Sudoku.lg > /dev/null
        end=$(date +%s.%N)
        
        dt=$(echo "$end - $start" | bc)
        printf "Seconds to process %.3f\n" "$dt"
    done
else
    echo "ucblogo not found"
fi
