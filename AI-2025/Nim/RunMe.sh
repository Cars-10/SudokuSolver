#!/bin/bash
cd "$(dirname "$0")"
# Compile
nim c -d:release --opt:speed -o:Sudoku Sudoku.nim
# Run
./Sudoku ../Matrices/*.matrix | tee run.txt
