#!/bin/bash
cd "$(dirname "$0")"

# Run
if command -v apl &> /dev/null; then
    for file in ../Matrices/*.matrix; do
        echo "$file"
        
        start=$(date +%s.%N)
        # Run APL script
        apl --script Sudoku.apl > /dev/null
        end=$(date +%s.%N)
        
        dt=$(echo "$end - $start" | bc)
        printf "Seconds to process %.3f\n" "$dt"
    done
else
    echo "apl not found"
fi
