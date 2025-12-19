#!/bin/bash
emacs -batch -l sudoku.el --eval "(sudoku-solve \"/app/Matrices/1.matrix\")"
