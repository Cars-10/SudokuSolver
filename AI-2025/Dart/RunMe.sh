#!/bin/bash
cd "$(dirname "$0")"
# Run
dart Sudoku.dart ../Matrices/*.matrix | tee run.txt
