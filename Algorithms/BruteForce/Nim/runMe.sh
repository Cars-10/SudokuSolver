#!/bin/bash
# Nim Sudoku Solver Benchmark Runner
# Uses common.sh for shared benchmark functionality

LANGUAGE="Nim"
COMPILE_CMD="nim c -d:release -o:Sudoku Sudoku.nim"
SOLVER_BINARY="./Sudoku"

# Source common functions
source ../../common.sh

# Check for Nim compiler
check_toolchain "nim"

# Run main
main "$@"
