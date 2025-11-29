#!/bin/bash
cd "$(dirname "$0")"
# Compile
crystal build --release Sudoku.cr -o Sudoku
# Run
./Sudoku ../Matrices/*.matrix | tee run.txt
