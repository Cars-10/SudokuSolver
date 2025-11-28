#!/bin/zsh
cd "$(dirname $0:A)"
time ./Sudoku  ../Matrices/*.matrix | tee run.txt
