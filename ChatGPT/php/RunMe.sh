#!/bin/zsh
cd "$(dirname $0:A)"

time php Sudoku.php ../Matrices/*.matrix | tee run.txt
