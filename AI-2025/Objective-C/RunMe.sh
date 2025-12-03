#!/bin/bash
cd "$(dirname "$0")"
# Compile
clang -framework Foundation -O3 Sudoku.m -o Sudoku
# Run each matrix file (solver only handles one at a time)
for matrix in ../Matrices/*.matrix; do
    ./Sudoku "$matrix"
done
