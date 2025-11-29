#!/bin/bash
cd "$(dirname "$0")"
# Run
awk -f Sudoku.awk ../Matrices/*.matrix | tee run.txt
