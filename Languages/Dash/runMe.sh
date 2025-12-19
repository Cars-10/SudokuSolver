#!/bin/bash
# Languages/Dash/runMe.sh

LANGUAGE="Dash"
SOLVER_BINARY="dash Sudoku.dash"

# Source shared functions
source ../common.sh

compile() {
    # Check if dash is available
    if ! command -v dash &> /dev/null; then
        report_env_error "dash not found"
    fi
}

# Execute benchmarks
main "$@"
