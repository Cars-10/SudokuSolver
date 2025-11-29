#!/bin/bash
cd "$(dirname "$0")"
# Run
# Try to find sml
if command -v sml &> /dev/null; then
    sml Sudoku.sml ../Matrices/*.matrix | tee run.txt
elif [ -f /usr/local/smlnj/bin/sml ]; then
    /usr/local/smlnj/bin/sml Sudoku.sml ../Matrices/*.matrix | tee run.txt
else
    echo "sml not found"
    exit 1
fi
