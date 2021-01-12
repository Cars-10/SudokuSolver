#!/bin/zsh
cd "$(dirname $0:A)"
time python3 Sudoku.py ../Matrices/*.matrix
