#!/bin/bash
cd "$(dirname "$0")"
# Run
octave --no-gui --silent Sudoku.m ../Matrices/*.matrix | tee run.txt
