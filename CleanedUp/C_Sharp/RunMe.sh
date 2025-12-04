#!/bin/zsh
cd "$(dirname $0:A)/Sudoku"
time dotnet run ../../Matrices/*.matrix  | tee ../run.txt
