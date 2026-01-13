#!/bin/bash
# D Sudoku Solver Benchmark Runner
# Uses common.sh for shared benchmark functionality

LANGUAGE="D"
COMPILE_CMD="ldc2 -O -release -of=Sudoku Sudoku.d"
SOLVER_BINARY="./Sudoku"

# Source common functions
source ../../common.sh

# Check for D compiler
if ! command -v ldc2 &> /dev/null; then
    if ! command -v dmd &> /dev/null; then
        report_env_error "D compiler not found (need ldc2 or dmd)"
    else
        COMPILE_CMD="dmd -O -release -of=Sudoku Sudoku.d"
    fi
fi

# Run main
main "$@"
