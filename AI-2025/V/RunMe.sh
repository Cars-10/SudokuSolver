#!/bin/bash
cd "$(dirname "$0")"
# Compile
v -prod Sudoku.v
# Run
./Sudoku ../Matrices/*.matrix | tee run.txt
