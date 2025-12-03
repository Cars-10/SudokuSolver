#!/bin/bash
cd "$(dirname "$0")"
# Run each matrix file
for matrix in ../Matrices/*.matrix; do
    ./sudoku "$matrix"
done
