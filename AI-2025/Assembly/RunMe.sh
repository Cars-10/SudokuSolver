#!/bin/bash
cd "$(dirname "$0")"

# Compile
# macOS requires Mach-O 64-bit format
nasm -f macho64 Sudoku.asm -o Sudoku.o
# Link with System libraries (using gcc/clang is easiest to link libc)
ld -o Sudoku Sudoku.o -lSystem -syslibroot `xcrun -sdk macosx --show-sdk-path` -e _main -arch x86_64

# Run
for file in ../Matrices/*.matrix; do
    # Preprocess to remove comments for simple fscanf
    grep -v "^#" "$file" > temp.txt
    
    start=$(date +%s.%N)
    ./Sudoku temp.txt
    end=$(date +%s.%N)
    
    dt=$(echo "$end - $start" | bc)
    printf "Seconds to process %.3f\n" "$dt"
done
rm temp.txt Sudoku.o Sudoku
