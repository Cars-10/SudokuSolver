#!/bin/bash
# Crystal Sudoku Solver Benchmark Runner
# Uses common.sh for shared benchmark functionality

LANGUAGE="Crystal"
COMPILE_CMD="crystal build --release -o Sudoku Sudoku.cr"
SOLVER_BINARY="./Sudoku"

# Source common functions
source ../common.sh

# Check for Crystal compiler
check_toolchain "crystal"

# Run main
main "$@"
