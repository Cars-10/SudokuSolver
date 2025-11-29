#!/bin/bash
cd "$(dirname "$0")"
# Run
raku Sudoku.raku ../Matrices/*.matrix | tee run.txt
