#!/bin/zsh
echo $(date) > run.txt
for i in {1..6}
do
    matrix="../../Matrices/${i}.matrix"
    print "File ${matrix}" >> run.txt
    /usr/bin/time -o run.txt -a php Sudoku.php ${matrix} | tee -a run.txt
done
