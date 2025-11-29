#!/bin/bash
cd "$(dirname "$0")"
# Compile
haxe -main Sudoku -neko Sudoku.n
# Run
neko Sudoku.n ../Matrices/*.matrix | tee run.txt
