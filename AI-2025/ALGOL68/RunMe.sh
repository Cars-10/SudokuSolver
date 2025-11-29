#!/bin/bash
cd "$(dirname "$0")"

# Run
if command -v algol68g &> /dev/null; then
    for file in ../Matrices/*.matrix; do
        echo "$file"
        
        start=$(date +%s.%N)
        # Run ALGOL 68 script
        algol68g Sudoku.a68
        end=$(date +%s.%N)
        
        dt=$(echo "$end - $start" | bc)
        printf "Seconds to process %.3f\n" "$dt"
    done
else
    echo "algol68g not found"
fi
