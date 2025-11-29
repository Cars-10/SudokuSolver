#!/bin/bash
cd "$(dirname "$0")"

# Run
if command -v snobol4 &> /dev/null; then
    for file in ../Matrices/*.matrix; do
        echo "$file"
        
        start=$(date +%s.%N)
        # Run SNOBOL
        snobol4 Sudoku.sno > /dev/null
        end=$(date +%s.%N)
        
        dt=$(echo "$end - $start" | bc)
        printf "Seconds to process %.3f\n" "$dt"
    done
else
    echo "snobol4 not found"
fi
