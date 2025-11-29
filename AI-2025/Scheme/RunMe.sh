#!/bin/bash
cd "$(dirname "$0")"
# Run
guile -s Sudoku.scm ../Matrices/*.matrix | tee run.txt
