#!/bin/zsh
#cd "$(dirname $0:A)"
# Quick and dirty script to run the program and save the output
# Had to do this as the python script was not running properly
# when called from the shell script with the matrices as arguments

echo "" > run.txt
for i in {1..6}
do
    time python3 Sudoku.py ../../Matrices/$i.matrix | tee -a run.txt
done


