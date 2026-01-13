#!/bin/bash
# Racket Sudoku Solver Benchmark Runner
# Uses common.sh for standardized metrics collection

LANGUAGE="Racket"
SOLVER_BINARY="racket Sudoku.rkt"

# Source common functions
source ../common.sh

# Racket doesn't need compilation - runs .rkt files directly
COMPILE_CMD=""

# Run benchmarks
main "$@"
