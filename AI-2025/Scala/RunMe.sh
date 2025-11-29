#!/bin/bash
cd "$(dirname "$0")"
# Compile
scalac Sudoku.scala
# Run
scala Sudoku ../Matrices/*.matrix | tee run.txt
