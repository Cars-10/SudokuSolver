#!/bin/bash
# Compile
./qb64pe/qb64pe -x Sudoku.bas -o Sudoku
# Run
for f in ../Matrices/*.matrix; do
    ./Sudoku "$f"
done | tee run.txt
