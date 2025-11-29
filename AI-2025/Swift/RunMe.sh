#!/bin/bash
cd "$(dirname "$0")"
# Compile
swiftc -O Sudoku.swift -o Sudoku
# Run
./Sudoku ../Matrices/*.matrix | tee run.txt
