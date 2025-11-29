#!/bin/bash
cd "$(dirname "$0")"

# Run
for file in ../Matrices/*.matrix; do
    echo "$file"
    
    start=$(date +%s.%N)
    # Process m4 to c
    m4 Sudoku.m4 > temp.c
    # Compile
    gcc -o Sudoku temp.c
    # Run (it's a placeholder, so we just run it)
    ./Sudoku
    # In a real metaprogramming solver, we would embed the puzzle in the m4 macro
    # and the C code would solve THAT puzzle.
    # For now, we demonstrate the M4 build step.
    
    end=$(date +%s.%N)
    
    dt=$(echo "$end - $start" | bc)
    printf "Seconds to process %.3f\n" "$dt"
    rm temp.c Sudoku
done
