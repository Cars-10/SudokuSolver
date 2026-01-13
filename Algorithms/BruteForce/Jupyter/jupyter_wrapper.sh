#!/bin/bash
MATRIX="$1"
# nbconvert execute and output to markdown, which prints stdout of cells
# Use environment variable to pass the matrix file
export MATRIX_FILE="$MATRIX"
jupyter nbconvert --execute --to markdown --stdout Sudoku.ipynb 2>/dev/null
