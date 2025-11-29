#!/bin/bash
cd "$(dirname "$0")"

# Run
for file in ../Matrices/*.matrix; do
    echo "$file"
    # Convert matrix to XML
    echo "<puzzle>" > temp.xml
    grep -v "^#" "$file" | tr -s ' \t\n' ' ' >> temp.xml
    echo "</puzzle>" >> temp.xml
    
    start=$(date +%s.%N)
    xsltproc Sudoku.xslt temp.xml
    end=$(date +%s.%N)
    
    dt=$(echo "$end - $start" | bc)
    printf "Seconds to process %.3f\n" "$dt"
done
rm temp.xml
