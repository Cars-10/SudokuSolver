#!/bin/bash
cd "$(dirname "$0")"
# Run
# emacs --script prints to stderr mostly, but message goes to stderr in script mode?
# Actually message goes to stderr.
emacs --script Sudoku.el -- ../Matrices/*.matrix 2>&1 | tee run.txt
