#!/bin/bash
# Languages/BASIC/runMe.sh

cd "$(dirname "$0")"

LANGUAGE="BASIC"
SOLVER_BINARY="./Sudoku"
METRICS_FILE="metrics.json"
TIMEOUT_SECONDS=300

# Source shared functions
source ../common.sh

compile() {
    check_toolchain fbc

    echo "Compiling BASIC solver..."
    fbc Sudoku.bas -x Sudoku

    if [ $? -ne 0 ]; then
        report_env_error "Compilation failed"
    fi
}

# Execute benchmarks
main "$@"
