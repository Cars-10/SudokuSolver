#!/bin/bash
# CoffeeScript Sudoku Solver Benchmark Runner
# Uses common.sh for standardized metrics collection

LANGUAGE="CoffeeScript"
SOLVER_BINARY="coffee Sudoku.coffee"

# Source common functions
source ../common.sh

# CoffeeScript doesn't need compilation - coffee runs .coffee files directly
COMPILE_CMD=""

# Run benchmarks
main "$@"
