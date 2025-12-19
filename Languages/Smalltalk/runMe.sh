#!/bin/bash
# Languages/Smalltalk/runMe.sh

LANGUAGE="Smalltalk"
SOLVER_BINARY="gst Sudoku.st -a"

# Source shared functions
source ../common.sh

# GNU Smalltalk doesn't need compilation
compile() {
    # Check if gst is available in container
    if ! command -v gst &> /dev/null; then
        report_env_error "gnu-smalltalk (gst) not found"
    fi
}

# Execute benchmarks
main "$@"
