#!/bin/bash
cd "$(dirname "$0")"
# Run
groovy Sudoku.groovy ../Matrices/*.matrix | tee run.txt
