#!/bin/bash
cd "$(dirname "$0")"
# Run
swipl -O -g main -t halt Sudoku.pl -- ../Matrices/*.matrix | tee run.txt
