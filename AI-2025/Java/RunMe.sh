#!/bin/bash
cd "$(dirname "$0")"
/opt/homebrew/opt/openjdk/bin/javac SudokuSolver.java
/opt/homebrew/opt/openjdk/bin/java SudokuSolver ../Matrices/*.matrix | tee run.txt
