#!/bin/bash
# Languages/Assembly/runMe.sh - Assembly Sudoku solver benchmark script

LANGUAGE="Assembly"

# Source shared functions from common.sh
source ../common.sh

# Compile first
nasm -f elf64 Sudoku.asm -o Sudoku.o
gcc Sudoku.o -o sudoku

# Set the binary to be executed
SOLVER_BINARY="./sudoku"

# Main execution
main "$@"
