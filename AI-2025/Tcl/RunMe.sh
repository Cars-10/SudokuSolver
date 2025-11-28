#!/bin/zsh
cd "$(dirname $0:A)"
time /opt/homebrew/opt/tcl-tk@8/bin/tclsh8.6 Sudoku.tcl ../Matrices/*.matrix | tee run.txt
