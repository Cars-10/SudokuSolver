#!/bin/bash
# runMe.sh - Common Lisp (SBCL) Sudoku Solver Benchmark
# Created: 2025-12-18

# Language configuration
LANGUAGE="CommonLisp"
SOLVER_BINARY="sbcl --script Sudoku.lisp --"
COMPILE_CMD=""  # No compilation needed for SBCL script

# Source common benchmark functions
source ../common.sh

# Check if SBCL is available
check_toolchain sbcl

# Run benchmarks
main "$@"
