#!/bin/bash
# Languages/Ksh/runMe.sh

cd "$(dirname "$0")"

LANGUAGE="Ksh"
SOLVER_BINARY="ksh Sudoku.ksh"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

# Source shared functions
source ../../common.sh

compile() {
    check_toolchain ksh
}

# Execute benchmarks
main "$@"
