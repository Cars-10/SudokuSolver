#!/bin/zsh
cd "$(dirname $0:A)"
pwd
time ./Sudoku.sh ../Matrices/*.matrix | tee run.txt
