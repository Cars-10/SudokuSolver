#!/bin/bash
cd "$(dirname "$0")"

# Run
if command -v pdflatex &> /dev/null; then
    for file in ../Matrices/*.matrix; do
        echo "$file"
        # We can't easily pass file content to latex without generating a .tex file
        # So we generate a wrapper .tex
        
        content=$(grep -v "^#" "$file" | tr -s ' \t\n' ' ')
        
        echo "\documentclass{article}" > solve.tex
        echo "\begin{document}" >> solve.tex
        echo "Puzzle: $content" >> solve.tex
        echo "\end{document}" >> solve.tex
        
        start=$(date +%s.%N)
        pdflatex -interaction=nonstopmode solve.tex > /dev/null
        end=$(date +%s.%N)
        
        dt=$(echo "$end - $start" | bc)
        printf "Seconds to process %.3f\n" "$dt"
        rm solve.tex solve.log solve.aux solve.pdf
    done
else
    echo "pdflatex not found"
fi
