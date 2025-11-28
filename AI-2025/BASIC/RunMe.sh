#!/bin/bash
# Compile
../../qb64 -x Sudoku.bas -o Sudoku
# Run
./Sudoku ../Matrices/*.matrix | tee run.txt
