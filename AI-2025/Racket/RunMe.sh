#!/bin/zsh
cd "$(dirname $0:A)"
time racket Sudoku.rkt ../Matrices/*.matrix | tee run.txt
