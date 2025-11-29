#!/bin/bash
cd "$(dirname "$0")"
# Compile
clang -framework Foundation -O3 Sudoku.m -o Sudoku
# Run
./Sudoku ../Matrices/*.matrix | tee run.txt
