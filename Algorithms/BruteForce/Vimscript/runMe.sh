#!/bin/bash
# Algorithms/BruteForce/Vimscript/runMe.sh

cd "$(dirname "$0")"

LANGUAGE="Vimscript"
SOLVER_BINARY="vim -u NONE -es -S sudoku.vim --"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"

# Source shared functions
source ../../common.sh

compile() {
    check_toolchain vim
}

# Execute benchmarks
main "$@"
