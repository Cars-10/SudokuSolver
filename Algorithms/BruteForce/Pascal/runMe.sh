#!/bin/bash
# Pascal Sudoku Solver - Benchmark Runner
# Uses common.sh for standardized benchmark execution

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Language configuration
LANGUAGE="Pascal"
SOLVER_BINARY="./sudoku"
COMPILE_CMD="fpc -O2 sudoku.pas"

# Source common functions
source ../../common.sh

# Check for Free Pascal compiler
check_toolchain "fpc"

# Run benchmarks
main "$@"
