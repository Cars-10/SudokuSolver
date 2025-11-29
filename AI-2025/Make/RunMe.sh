#!/bin/bash
cd "$(dirname "$0")"
# Run
for file in ../Matrices/*.matrix; do
    echo "$file"
    make -f Sudoku.mk run FILE="$file" | tee run.txt
done
