#!/bin/zsh
cd "$(dirname $0:A)"
time Rscript Sudoku.R ../Matrices/*.matrix | tee run.txt
