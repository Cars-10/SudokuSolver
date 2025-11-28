#!/bin/zsh
cd "$(dirname $0:A)"
pwd
time ./Sudoku.sh ../Matrices/1.matrix | tee run.txt
