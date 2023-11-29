#!/bin/zsh
# Quick and dirty script to run the program and save the output
# Had to do this as the python script was not running properly
# when called from the shell script with the matrices as arguments

echo $(date) > run.txt
for i in {1..6}
do
    matrix="../../Matrices/${i}.matrix"
    print "File ${matrix}" >> run.txt
    /usr/bin/time -o run.txt -a bin/Debug/net8.0/Sudoku ${matrix} | tee -a run.txt
done


