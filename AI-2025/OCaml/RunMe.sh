#!/bin/bash
cd "$(dirname "$0")"
ocamlopt -o Sudoku Sudoku.ml
for f in ../Matrices/*.matrix; do
    echo "$f"
    ./Sudoku "$f"
done | tee run.txt
