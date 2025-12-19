#!/bin/bash
# Languages/Vimscript/runMe.sh

LANGUAGE="Vimscript"
SOLVER_BINARY="vim -u NONE -es -S sudoku.vim --"

# Source shared functions
source ../common.sh

compile() {
    # Check if vim is available
    if ! command -v vim &> /dev/null; then
        report_env_error "vim not found"
    fi
}

# Execute benchmarks
main "$@"
