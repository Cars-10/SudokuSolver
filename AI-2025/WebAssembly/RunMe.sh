#!/bin/bash
cd "$(dirname "$0")"

# Run
# We need wasmtime or similar.
if command -v wasmtime &> /dev/null; then
    for file in ../Matrices/*.matrix; do
        echo "$file"
        # Compile WAT to WASM?
        # wasmtime can run wat directly? Yes.
        
        start=$(date +%s.%N)
        wasmtime Sudoku.wat
        end=$(date +%s.%N)
        
        dt=$(echo "$end - $start" | bc)
        printf "Seconds to process %.3f\n" "$dt"
    done
else
    echo "wasmtime not found"
fi
