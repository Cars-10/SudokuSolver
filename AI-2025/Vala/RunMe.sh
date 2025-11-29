#!/bin/bash
cd "$(dirname "$0")"
# Compile
valac Sudoku.vala -o Sudoku
# Run
./Sudoku ../Matrices/*.matrix | tee run.txt
