#!/bin/bash
# Algorithms/BruteForce/EmacsLisp/runMe.sh

cd "$(dirname "$0")"

LANGUAGE="EmacsLisp"
SOLVER_BINARY="emacs --batch --script sudoku.el"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-300}"

source ../../common.sh

compile() {
    check_toolchain emacs
}

main "$@"