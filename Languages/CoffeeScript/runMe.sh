#!/bin/bash
# CoffeeScript Sudoku Solver Benchmark Runner

cd "$(dirname "$0")"

LANGUAGE="CoffeeScript"
SOLVER_BINARY="coffee Sudoku.coffee"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

# Source common functions
source ../common.sh

compile() {
    check_toolchain coffee
}

# Run benchmarks
main "$@"
