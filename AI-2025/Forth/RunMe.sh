#!/bin/bash
cd "$(dirname "$0")"
# Run
gforth Sudoku.fs ../Matrices/*.matrix | tee run.txt
