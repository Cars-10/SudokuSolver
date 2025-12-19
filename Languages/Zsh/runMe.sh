#!/bin/bash
# Languages/Zsh/runMe.sh

LANGUAGE="Zsh"
SOLVER_BINARY="zsh Sudoku.zsh"

# Source shared functions
source ../common.sh

compile() {
    # Check if zsh is available
    if ! command -v zsh &> /dev/null; then
        report_env_error "zsh not found"
    fi
}

# Execute benchmarks
main "$@"
