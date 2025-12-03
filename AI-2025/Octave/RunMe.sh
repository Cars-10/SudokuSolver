#!/bin/bash
cd "$(dirname "$0")"
# Run each matrix file (solver only handles one at a time)
for matrix in ../Matrices/*.matrix; do
    octave --no-gui --silent Sudoku.m "$matrix"
done
