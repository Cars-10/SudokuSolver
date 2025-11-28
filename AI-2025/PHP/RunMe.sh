#!/bin/bash
cd "$(dirname "$0")"
for f in ../Matrices/*.matrix; do
    echo "$f"
    php Sudoku.php "$f"
done | tee run.txt
