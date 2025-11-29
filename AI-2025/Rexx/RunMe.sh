#!/bin/bash
cd "$(dirname "$0")"
# Run
rexx Sudoku.rexx ../Matrices/*.matrix | tee run.txt
