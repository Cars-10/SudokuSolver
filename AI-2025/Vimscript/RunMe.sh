#!/bin/bash
cd "$(dirname "$0")"
# Run
# vim -e (Ex mode) or -S (source script)
# We use -S and redirect output. Vim is chatty.
# We need to pass arguments to Vim.
vim -N -u NONE -i NONE -e -s -S Sudoku.vim -- ../Matrices/*.matrix 2>&1 | tee run.txt
