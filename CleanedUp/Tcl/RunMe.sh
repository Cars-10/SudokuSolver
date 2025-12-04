#!/bin/zsh
cd "$(dirname $0:A)"
time /usr/local/bin/tclsh Sudoku.tcl ../Matrices/*.matrix | tee run.txt
