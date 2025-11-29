#!/bin/bash
cd "$(dirname "$0")"

# Run
if command -v ghdl &> /dev/null; then
    # Compile
    ghdl -a Sudoku.vhdl
    ghdl -e sudoku
    
    for file in ../Matrices/*.matrix; do
        echo "$file"
        
        start=$(date +%s.%N)
        # Run VHDL simulation
        ghdl -r sudoku
        end=$(date +%s.%N)
        
        dt=$(echo "$end - $start" | bc)
        printf "Seconds to process %.3f\n" "$dt"
    done
    rm sudoku.o sudoku
else
    echo "ghdl not found"
fi
