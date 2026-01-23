#!/bin/bash
MATRIX="$1"
# Use -dNOSAFER to allow reading matrix files
gs -dNODISPLAY -q -dNOSAFER -sInputFile="$MATRIX" Sudoku.ps
