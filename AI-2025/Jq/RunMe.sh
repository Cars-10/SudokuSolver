#!/bin/bash
cd "$(dirname "$0")"

# Run
for file in ../Matrices/*.matrix; do
    echo "$file"
    # Preprocess to JSON array
    # grep -v "^#" removes comments
    # tr -s ' \t\n' ' ' replaces whitespace with single space
    # jq -R -s 'split(" ") ...' converts to array of numbers
    
    content=$(grep -v "^#" "$file")
    # Convert to JSON array of numbers
    json=$(echo "$content" | tr -s ' \t\n' ' ' | jq -R -s 'split(" ") | map(select(length>0) | tonumber)')
    
    start=$(date +%s.%N)
    echo "$json" | jq -r -f Sudoku.jq
    end=$(date +%s.%N)
    
    dt=$(echo "$end - $start" | bc)
    printf "Seconds to process %.3f\n" "$dt"
done
