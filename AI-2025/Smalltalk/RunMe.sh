#!/bin/bash
cd "$(dirname "$0")"
# Run
gst Sudoku.st -a ../Matrices/*.matrix | tee run.txt
