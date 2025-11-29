#!/bin/zsh
cd "$(dirname $0:A)"
time python3 -u Sudoku.py ../Matrices/*.matrix | tee run.txt
