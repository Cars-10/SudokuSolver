#!/bin/bash
cd "$(dirname "$0")"
# Compile
gnatmake -O3 Sudoku.adb
# Run
./sudoku ../Matrices/*.matrix | tee run.txt
