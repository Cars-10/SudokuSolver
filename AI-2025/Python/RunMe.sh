#!/bin/bash
cd "$(dirname "$0")"

# Run the Python solver
# Assuming python3 is available
python3 Sudoku.py ../Matrices/*.matrix
