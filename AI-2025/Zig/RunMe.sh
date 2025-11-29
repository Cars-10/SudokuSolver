#!/bin/bash
cd "$(dirname "$0")"
# Compile
zig build-exe -O ReleaseFast Sudoku.zig
# Run
./Sudoku ../Matrices/*.matrix | tee run.txt
