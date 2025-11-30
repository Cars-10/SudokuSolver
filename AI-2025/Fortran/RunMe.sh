#!/bin/zsh
cd "$(dirname $0:A)"
gfortran -O3 -o Sudoku Sudoku.f90
./Sudoku ../Matrices/*.matrix | tee run.txt
