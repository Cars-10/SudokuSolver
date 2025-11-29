#!/bin/bash
cd "$(dirname "$0")"
# Compile
erlc sudoku.erl
# Run
erl -noshell -s sudoku start -s init stop -- ../Matrices/*.matrix | tee run.txt
