#!/bin/bash
# Languages/Zsh/runMe.sh

cd "$(dirname "$0")"

LANGUAGE="Zsh"
SOLVER_BINARY="zsh Sudoku.zsh"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

# Source shared functions
source ../common.sh

compile() {
    check_toolchain zsh
}

# Execute benchmarks
main "$@"
