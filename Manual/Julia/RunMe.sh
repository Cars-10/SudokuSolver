#!/bin/zsh
cd "$(dirname $0:A)"
time julia Sudoku.jl ../Matrices/*.matrix | tee run.txt
