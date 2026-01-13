#!/bin/bash
# Algorithms/BruteForce/Janet/runMe.sh

cd "$(dirname "$0")"

LANGUAGE="Janet"
SOLVER_BINARY="janet Sudoku.janet"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

source ../../common.sh

compile() {
    check_toolchain janet
}

main "$@"
