#!/bin/bash
cd "$(dirname "$0")"
for f in ../Matrices/*.matrix; do
    echo "$f"
    ./Sudoku "$f"
done | tee run.txt
