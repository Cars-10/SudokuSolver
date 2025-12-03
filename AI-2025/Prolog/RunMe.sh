#!/bin/bash
cd "$(dirname "$0")"
# Run each matrix file (solver only handles one at a time)
for matrix in ../Matrices/*.matrix; do
    swipl -O -g main -t halt Sudoku.pl -- "$matrix"
done
