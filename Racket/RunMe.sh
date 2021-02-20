#!/bin/zsh
cd "$(dirname $0:A)"
time racket -S tr-pfds-nocheck Sudoku.rkt ../Matrices/*.matrix | tee run.txt
