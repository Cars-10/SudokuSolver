#!/bin/zsh
cd "$(dirname $0:A)"
time Sudoku.sh ../Matrices/*.matrix | tee run.txt
