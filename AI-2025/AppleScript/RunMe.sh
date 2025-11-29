#!/bin/bash
cd "$(dirname "$0")"
# Run
# osascript prints to stderr for log, so we redirect stderr to stdout to capture it
osascript Sudoku.applescript ../Matrices/*.matrix 2>&1 | tee run.txt
