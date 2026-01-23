#!/bin/bash
# Algorithms/BruteForce/Forth/runMe.sh

LANGUAGE="Forth"
SOLVER_BINARY="gforth Sudoku.fs"

# Source shared functions
source ../../common.sh

compile() {
    # Check if gforth is available
    if ! command -v gforth &> /dev/null; then
        if [ -x "/usr/bin/gforth" ]; then
            SOLVER_BINARY="/usr/bin/gforth Sudoku.fs"
        else
            report_env_error "gforth not found"
        fi
    fi
}

# Execute benchmarks
main "$@"
