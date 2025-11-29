#!/bin/bash
cd "$(dirname "$0")"
# Run
pwsh -File Sudoku.ps1 ../Matrices/*.matrix | tee run.txt
