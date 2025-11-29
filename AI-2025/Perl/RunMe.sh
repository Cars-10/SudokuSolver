#!/bin/bash
cd "$(dirname "$0")"
# Run
perl Sudoku.pl ../Matrices/*.matrix | tee run.txt
