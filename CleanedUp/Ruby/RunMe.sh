#!/bin/zsh
cd "$(dirname $0:A)"
time ruby --jit Sudoku.rb ../Matrices/*.matrix | tee run.txt
