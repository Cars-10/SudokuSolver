#!/bin/bash
cd "$(dirname "$0")"
# Compile
odin build Sudoku.odin -file -o:speed -out:Sudoku
# Run
./Sudoku ../Matrices/*.matrix | tee run.txt
