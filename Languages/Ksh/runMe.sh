#!/bin/bash
# Languages/Ksh/runMe.sh

LANGUAGE="Ksh"
SOLVER_BINARY="ksh Sudoku.ksh"

# Source shared functions
source ../common.sh

compile() {
    # Check if ksh is available
    if ! command -v ksh &> /dev/null; then
        report_env_error "ksh not found"
    fi
}

# Execute benchmarks
main "$@"
