#!/bin/bash
cd "$(dirname "$0")"
# Run
sbcl --script Sudoku.lisp ../Matrices/*.matrix | tee run.txt
