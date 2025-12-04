#!/bin/zsh
cd "$(dirname $0:A)"
time  Sudoku/target/release/Sudoku ../Matrices/*.matrix  | tee run.txt
