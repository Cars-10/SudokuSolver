#!/bin/bash
cd "$(dirname "$0")"
# Compile
kotlinc Sudoku.kt -include-runtime -d Sudoku.jar
# Run
/opt/homebrew/opt/openjdk/bin/java -jar Sudoku.jar ../Matrices/*.matrix | tee run.txt
