#!/bin/bash
# Languages/Fennel/runMe.sh - Fennel Sudoku solver benchmark script

LANGUAGE="Fennel"
SOLVER_BINARY="fennel Sudoku.fnl"

# Source shared functions from common.sh
source ../../common.sh

# Main execution
main "$@"
