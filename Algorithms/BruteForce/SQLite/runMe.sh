#!/bin/bash
# Languages/SQLite/runMe.sh

LANGUAGE="SQLite"
SOLVER_BINARY="node Sudoku.js"

# Source shared functions
source ../common.sh

compile() {
    # Check if node is available
    if ! command -v node &> /dev/null; then
        report_env_error "node not found"
    fi
}

# Execute benchmarks
main "$@"
