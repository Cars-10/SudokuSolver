#!/bin/bash
# Languages/Icon/runMe.sh - Icon Sudoku solver benchmark script

LANGUAGE="Icon"

# Source shared functions from common.sh
source ../common.sh

# Compile first
icont -o sudoku Sudoku.icn 2>/dev/null

# Set the binary to be executed
SOLVER_BINARY="./sudoku"

# Main execution
main "$@"
