#!/bin/bash
cd "$(dirname "$0")"
# Run
lua Sudoku.lua ../Matrices/*.matrix | tee run.txt
