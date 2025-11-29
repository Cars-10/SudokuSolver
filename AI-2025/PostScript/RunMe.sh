#!/bin/bash
cd "$(dirname "$0")"
# Run
gs -q -dNODISPLAY -dNOSAFER -- Sudoku.ps ../Matrices/*.matrix | tee run.txt
