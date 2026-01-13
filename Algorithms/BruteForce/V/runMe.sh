#!/bin/bash
# V Sudoku Solver Benchmark Script
# Uses common.sh for standardized metrics collection

LANGUAGE="V"
source ../../common.sh

# Check V compiler
check_toolchain v

# Compile with production optimizations
COMPILE_CMD="v -prod -o Sudoku Sudoku.v"
SOLVER_BINARY="./Sudoku"

# Run compilation and benchmarks
main "$@"
