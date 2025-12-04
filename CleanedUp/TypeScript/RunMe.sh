cd "$(dirname $0:A)/out"
time node Sudoku.js  ../../Matrices/*.matrix  | tee ../run.txt
