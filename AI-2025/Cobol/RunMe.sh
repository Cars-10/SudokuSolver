#!/bin/bash
cd "$(dirname "$0")"
# Compile
cobc -x -free -O2 Sudoku.cob -o Sudoku
# Run
./Sudoku ../Matrices/*.matrix | tee run.txt
