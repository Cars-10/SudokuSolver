#!/bin/bash
# run_selected_benchmarks.sh

LANGUAGES="C C++ Rust Go Java C_Sharp F_Sharp Nim D Zig Swift Kotlin Crystal Pascal Fortran Ada OCaml Haskell V Vala Julia JavaScript TypeScript BASIC"
ALGORITHMS="BruteForce DLX CP"
MATRICES="1-6"

for lang in $LANGUAGES; do
    for algo in $ALGORITHMS; do
        if [ -d "Algorithms/$algo/$lang" ]; then
            echo "--------------------------------------------------"
            echo "Running $lang ($algo) on matrices $MATRICES..."
            echo "--------------------------------------------------"
            # runMeGlobal takes: Language MatrixSpec Algorithm
            ./runMeGlobal.sh "$lang" "$MATRICES" "$algo"
        fi
    done
done
