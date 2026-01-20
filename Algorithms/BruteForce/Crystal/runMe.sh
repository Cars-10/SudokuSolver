#!/bin/bash
# Crystal Sudoku Solver Benchmark Runner
# Uses common.sh for shared benchmark functionality

cd "$(dirname "$0")"

LANGUAGE="Crystal"
SOLVER_BINARY="./Sudoku"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"

# Source common functions
source ../../common.sh

compile() {
    check_toolchain crystal
    echo "Compiling Crystal solver..."
    crystal build --release -o Sudoku Sudoku.cr
    if [ $? -ne 0 ]; then
        report_env_error "Compilation failed"
    fi
}

# Run main
main "$@"
