#!/bin/bash
# Languages/Pike/runMe.sh

cd "$(dirname "$0")"

LANGUAGE="Pike"
SOLVER_BINARY="pike Sudoku.pike"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

source ../common.sh

compile() {
    check_toolchain pike
}

main "$@"
