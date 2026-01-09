#!/bin/bash
# Languages/Smalltalk/runMe.sh

cd "$(dirname "$0")"

LANGUAGE="Smalltalk"
SOLVER_BINARY="gst Sudoku.st -a"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

# Source shared functions
source ../common.sh

compile() {
    check_toolchain gst
}

# Execute benchmarks
main "$@"
