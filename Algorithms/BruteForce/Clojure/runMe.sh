#!/bin/bash
# runMe.sh - Clojure Sudoku Solver Benchmark
# Created: 2025-12-18

# Language configuration
LANGUAGE="Clojure"
SOLVER_BINARY="clojure -M Sudoku.clj"
COMPILE_CMD=""  # No compilation needed for Clojure

# Source common benchmark functions
source ../common.sh

# Check if Clojure is available
check_toolchain clojure

# Run benchmarks
main "$@"
