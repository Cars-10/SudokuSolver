#!/bin/bash
# Languages/Red/runMe.sh

cd "$(dirname "$0")"

LANGUAGE="Red"
SOLVER_BINARY="red Sudoku.red"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

# Source shared functions
source ../../common.sh

compile() {
    # Red is interpreted, check if red is available
    check_toolchain red
}

# Execute benchmarks
main "$@"
