#!/bin/bash
# Zig Sudoku Solver - Benchmark Runner
# Uses common.sh for shared benchmark infrastructure

# Configuration
LANGUAGE="Zig"
COMPILE_CMD="zig build-exe Sudoku.zig -O ReleaseFast"
SOLVER_BINARY="./Sudoku"

# Source common functions
source ../../common.sh

# Check for Zig compiler
check_toolchain "zig"

# Run main benchmark
main "$@"
