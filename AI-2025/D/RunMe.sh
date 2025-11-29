#!/bin/bash
cd "$(dirname "$0")"
# Compile
ldc2 -O3 -release Sudoku.d -of=Sudoku
# Run
./Sudoku ../Matrices/*.matrix | tee run.txt
