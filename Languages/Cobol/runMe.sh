#!/bin/bash
# COBOL Sudoku Solver - Benchmark Runner
# Uses common.sh for shared benchmark infrastructure
#
# NOTE: COBOL solver has performance issues and may timeout on complex puzzles

# Configuration
LANGUAGE="COBOL"
COMPILE_CMD="cobc -x -free -O2 -o Sudoku sudoku.cob"
SOLVER_BINARY="./Sudoku"
TIMEOUT_SECONDS=600  # 10 minutes for COBOL due to slow execution

# Source common functions
source ../common.sh

# Check for GnuCOBOL compiler
check_toolchain "cobc"

# Run main benchmark
main "$@"
