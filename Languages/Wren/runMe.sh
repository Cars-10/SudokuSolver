#!/bin/bash
# Languages/Wren/runMe.sh - Wren Sudoku solver benchmark script

LANGUAGE="Wren"
SOLVER_BINARY="wren Sudoku.wren"

# Source shared functions from common.sh
source ../common.sh

# Main execution
main "$@"
