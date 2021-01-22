cd "$(dirname $0:A)"
time node Sudoku.js  ../Matrices/*.matrix  | tee run.txt
