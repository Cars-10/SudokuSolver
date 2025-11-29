#!/bin/bash
cd "$(dirname "$0")"
# Compile
ghc -O2 Sudoku.hs -o Sudoku
# Run
./Sudoku ../Matrices/*.matrix | tee run.txt
