#!/bin/bash
# Vala Sudoku Solver Benchmark Script
# Uses common.sh for standardized metrics collection

LANGUAGE="Vala"
source ../../common.sh

# Check Vala compiler
check_toolchain valac

# Compile with gio-2.0 package (needed for File I/O)
COMPILE_CMD="valac --pkg gio-2.0 -o Sudoku Sudoku.vala"
SOLVER_BINARY="./Sudoku"

# Run compilation and benchmarks
main "$@"
