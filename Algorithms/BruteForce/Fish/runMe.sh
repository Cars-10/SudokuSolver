#!/bin/bash
# Algorithms/BruteForce/Fish/runMe.sh

cd "$(dirname "$0")"

LANGUAGE="Fish"
SOLVER_BINARY="fish Sudoku.fish"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

# Source shared functions
source ../../common.sh

compile() {
    check_toolchain fish
}

# Execute benchmarks
main "$@"
