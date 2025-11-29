#!/bin/bash
cd "$(dirname "$0")"
# Run
coffee Sudoku.coffee ../Matrices/*.matrix | tee run.txt
